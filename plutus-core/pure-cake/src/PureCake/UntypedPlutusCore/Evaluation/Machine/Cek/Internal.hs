{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ImplicitParams           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}

module PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal
    ( EvaluationResult(..)
    , CekValue(..)
    , ErrorWithCause
    , CekBudgetSpender(..)
    , ExBudgetInfo(..)
    , ExBudgetMode(..)
    , CekEmitterInfo(..)
    , EmitterMode(..)
    , CekM (..)
    , MachineParameters(..)
    , runCekDeBruijn
    , throwingWithCause
    )
where

import PlutusPrelude (coerce)

import PureCake.UntypedPlutusCore.Core

import Data.Functor (($>))
import Data.RandomAccessList.Class qualified as Env (cons, empty, indexOne)
import Data.RandomAccessList.SkewBinary qualified as Env (RAList)
import PureCake.PlutusCore.Builtin (BuiltinRuntime (..), BuiltinsRuntime (..), MakeKnownM(..))
import PureCake.PlutusCore.DeBruijn (Index (..), NamedDeBruijn (..), deBruijnInitIndex)
import PureCake.PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..), stimesExBudget)
import PureCake.PlutusCore.Evaluation.Machine.Exception (EvaluationError (..), ErrorWithCause (..),
                                                         MachineError (..), CekUserError (..),
                                                         EvaluationResult (..))
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts (CekMachineCosts (..))

import Control.Monad (unless)
import Control.Monad.Catch (catch, throwM)
-- import Control.Monad.Except (MonadError (..))
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Data.Word64Array.Word8 (WordArray, iforWordArray, overIndex, readArray, toWordArray)

data MachineParameters =
    MachineParameters {
      machineCosts    :: CekMachineCosts
    , builtinsRuntime :: BuiltinsRuntime DefaultFun CekValue
    }

data StepKind
    = BConst
    | BVar
    | BLamAbs
    | BApply
    | BDelay
    | BForce
    | BBuiltin -- Cost of evaluating a Builtin AST node, not the function itself
    deriving stock (Show, Eq, Ord, Enum, Bounded)

cekStepCost :: CekMachineCosts -> StepKind -> ExBudget
cekStepCost costs kind = case kind of
    BConst   -> cekConstCost costs
    BVar     -> cekVarCost costs
    BLamAbs  -> cekLamCost costs
    BApply   -> cekApplyCost costs
    BDelay   -> cekDelayCost costs
    BForce   -> cekForceCost costs
    BBuiltin -> cekBuiltinCost costs

data ExBudgetCategory
    = BStep StepKind
    | BBuiltinApp DefaultFun  -- Cost of evaluating a fully applied builtin function
    | BStartup
    deriving stock (Show, Eq, Ord)

-- See Note [Show instance for BuiltinRuntime].
instance Show (BuiltinRuntime CekValue) where
    show _ = "<builtin_runtime>"

-- 'Values' for the modified CEK machine.
data CekValue =
    -- This bang gave us a 1-2% speed-up at the time of writing.
    VCon Const
  | VDelay !Term !CekValEnv
  | VLamAbs !NamedDeBruijn !Term !CekValEnv
    -- | A partial builtin application, accumulating arguments for eventual full application.
    -- We don't need a 'CekValEnv' here unlike in the other constructors, because 'VBuiltin'
    -- values always store their corresponding 'Term's fully discharged, see the comments at
    -- the call sites (search for 'VBuiltin').
  | VBuiltin
      !DefaultFun
      -- ^ So that we know, for what builtin we're calculating the cost. We can sneak this into
      -- 'BuiltinRuntime', so that we don't need to store it here, but somehow doing so was
      -- consistently slowing evaluation down by half a percent. Might be noise, might be not, but
      -- at least we know that removing this @fun@ is not helpful anyway. See this commit reversing
      -- the change: https://github.com/input-output-hk/plutus/pull/4778/commits/86a3e24ca3c671cc27c6f4344da2bcd14f961706
      Term
      -- ^ This must be lazy. It represents the fully discharged partial application of the builtin
      -- function that we're going to run when it's fully saturated.  We need the 'Term' to be able
      -- to return it in case full saturation is never achieved and a partial application needs to
      -- be returned in the result. The laziness is important, because the arguments are discharged
      -- values and discharging is expensive, so we don't want to do it unless we really have
      -- to. Making this field strict resulted in a 3-4.5% slowdown at the time of writing.
      !(BuiltinRuntime CekValue)
      -- ^ The partial application and its costing function.
      -- Check the docs of 'BuiltinRuntime' for details.
    deriving stock (Show)

type CekValEnv = Env.RAList CekValue

-- | The CEK machine is parameterized over a @spendBudget@ function. This makes the budgeting machinery extensible
-- and allows us to separate budgeting logic from evaluation logic and avoid branching on the union
-- of all possible budgeting state types during evaluation.
newtype CekBudgetSpender s = CekBudgetSpender
    { unCekBudgetSpender :: ExBudgetCategory -> ExBudget -> CekM s ()
    }

-- General enough to be able to handle a spender having one, two or any number of 'STRef's
-- under the hood.
-- | Runtime budgeting info.
data ExBudgetInfo cost s = ExBudgetInfo
    { _exBudgetModeSpender       :: !(CekBudgetSpender s)  -- ^ A spending function.
    , _exBudgetModeGetFinal      :: !(ST s cost) -- ^ For accessing the final state.
    , _exBudgetModeGetCumulative :: !(ST s ExBudget) -- ^ For accessing the cumulative budget.
    }

-- We make a separate data type here just to save the caller of the CEK machine from those pesky
-- 'ST'-related details.
-- | A budgeting mode to execute the CEK machine in.
newtype ExBudgetMode cost = ExBudgetMode
    { unExBudgetMode :: forall s. ST s (ExBudgetInfo cost s)
    }

type Slippage = Word8

-- See Note [Cost slippage]
-- | The default number of slippage (in machine steps) to allow.
defaultSlippage :: Slippage
defaultSlippage = 200

type CekEmitter s = [String] -> CekM s ()

-- | Runtime emitter info, similar to 'ExBudgetInfo'.
data CekEmitterInfo s = CekEmitterInfo {
    _cekEmitterInfoEmit       :: !(CekEmitter s)
    , _cekEmitterInfoGetFinal :: !(ST s [Text])
    }

-- | An emitting mode to execute the CEK machine in, similar to 'ExBudgetMode'.
newtype EmitterMode = EmitterMode
    { unEmitterMode :: forall s. ST s ExBudget -> ST s (CekEmitterInfo s)
    }

-- | Implicit parameter for the builtin runtime.
type GivenCekRuntime = (?cekRuntime :: BuiltinsRuntime DefaultFun CekValue)
-- | Implicit parameter for the log emitter reference.
type GivenCekEmitter s = (?cekEmitter :: CekEmitter s)
-- | Implicit parameter for budget spender.
type GivenCekSpender s = (?cekBudgetSpender :: CekBudgetSpender s)
type GivenCekCosts = (?cekCosts :: CekMachineCosts)

-- | Constraint requiring all of the machine's implicit parameters.
type GivenCekReqs s = (GivenCekRuntime, GivenCekEmitter s, GivenCekSpender s, GivenCekCosts)

-- | The monad the CEK machine runs in.
newtype CekM s a = CekM
    { unCekM :: ST s a
    } deriving newtype (Functor, Applicative, Monad)

throwingDischarged
    :: EvaluationError
    -> CekValue
    -> CekM s x
throwingDischarged e = throwingWithCause e . Just . dischargeCekValue

-- instance MonadError ErrorWithCause (CekM s) where
    -- See Note [Throwing exceptions in ST].
throwError :: ErrorWithCause -> CekM s a
throwError = CekM . throwM

-- See Note [Catching exceptions in ST].
catchError :: CekM s a -> (ErrorWithCause -> CekM s a) -> CekM s a
catchError a h = CekM . unsafeIOToST $ aIO `catch` hIO where
    aIO = unsafeRunCekM a
    hIO = unsafeRunCekM . h

    -- | Unsafely run a 'CekM' computation in the 'IO' monad by converting the
    -- underlying 'ST' to it.
    unsafeRunCekM :: CekM s a -> IO a
    unsafeRunCekM = unsafeSTToIO . unCekM

spendBudgetCek :: GivenCekSpender s => ExBudgetCategory -> ExBudget -> CekM s ()
spendBudgetCek = let (CekBudgetSpender spend) = ?cekBudgetSpender in spend

-- see Note [Scoping].
-- | Instantiate all the free variables of a term by looking them up in an environment.
-- Mutually recursive with dischargeCekVal.
dischargeCekValEnv :: CekValEnv
                   -> Term
                   -> Term
dischargeCekValEnv valEnv = go 0
 where
  -- The lamCnt is just a counter that measures how many lambda-abstractions
  -- we have descended in the `go` loop.
  go :: Word64 -> Term -> Term
  go !lamCnt = \t0 -> case t0 of
    LamAbs name body -> LamAbs name $ go (lamCnt+1) body
    var@(Var (NamedDeBruijn _ ndbnIx)) -> let ix = coerce ndbnIx :: Word64  in
        if lamCnt >= ix
        -- the index n is less-than-or-equal than the number of lambdas we have descended
        -- this means that n points to a bound variable, so we don't discharge it.
        then var
        else maybe
               -- var is free, leave it alone
               var
               -- var is in the env, discharge its value
               dischargeCekValue
               -- index relative to (as seen from the point of view of) the environment
               (Env.indexOne valEnv $ ix - lamCnt)
    Apply fun arg    -> Apply (go lamCnt fun) $ go lamCnt arg
    Delay term       -> Delay $ go lamCnt term
    Force term       -> Force $ go lamCnt term
    t -> t

-- | Convert a 'CekValue' into a 'Term' by replacing all bound variables with the terms
-- they're bound to (which themselves have to be obtain by recursively discharging values).
dischargeCekValue :: CekValue -> Term
dischargeCekValue = \t -> case t of
    VCon val                           -> Constant val
    VDelay body env                    -> dischargeCekValEnv env $ Delay body
    -- 'computeCek' turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
    -- argument of 'computeCek' and hence we need to start discharging outside of the reassembled
    -- lambda, otherwise @name@ could clash with the names that we have in @env@.
    VLamAbs (NamedDeBruijn n _ix) body env ->
        -- The index on the binder is meaningless, we put `0` by convention, see 'Binder'.
        dischargeCekValEnv env $ LamAbs (NamedDeBruijn n deBruijnInitIndex) body
    -- We only return a discharged builtin application when (a) it's being returned by the machine,
    -- or (b) it's needed for an error message.
    -- @term@ is fully discharged, so we can return it directly without any further discharging.
    VBuiltin _ term _                    -> term

data Context
    = FrameApplyFun !CekValue !Context
    | FrameApplyArg !CekValEnv !Term !Context
    | FrameForce !Context
    | NoFrame
    deriving stock (Show)

tryError :: CekM s a -> CekM s (Either ErrorWithCause a)
tryError a = (Right <$> a) `catchError` (pure . Left)

runCekM
    :: forall a cost.
       MachineParameters
    -> ExBudgetMode cost
    -> EmitterMode
    -> (forall s. GivenCekReqs s => CekM s a)
    -> (Either ErrorWithCause a, cost, [Text])
runCekM (MachineParameters costs runtime) (ExBudgetMode getExBudgetInfo) (EmitterMode getEmitterMode) a = runST $ do
    ExBudgetInfo{_exBudgetModeSpender, _exBudgetModeGetFinal, _exBudgetModeGetCumulative} <- getExBudgetInfo
    CekEmitterInfo{_cekEmitterInfoEmit, _cekEmitterInfoGetFinal} <- getEmitterMode _exBudgetModeGetCumulative
    let ?cekRuntime = runtime
        ?cekEmitter = _cekEmitterInfoEmit
        ?cekBudgetSpender = _exBudgetModeSpender
        ?cekCosts = costs
    errOrRes <- unCekM $ tryError a
    st <- _exBudgetModeGetFinal
    logs <- _cekEmitterInfoGetFinal
    pure (errOrRes, st, logs)

-- | Look up a variable name in the environment.
lookupVarName :: forall s.
                 NamedDeBruijn
              -> CekValEnv
              -> CekM s CekValue
lookupVarName varName@(NamedDeBruijn _ varIx) varEnv =
    case varEnv `Env.indexOne` coerce varIx of
        Nothing  -> throwingWithCause (InternalEvaluationError OpenTermEvaluatedMachineError)
                      $ Just (Var varName)
        Just val -> pure val

-- | Take pieces of a possibly partial builtin application and either create a 'CekValue' using
-- 'makeKnown' or a partial builtin application depending on whether the built-in function is
-- fully saturated or not.
evalBuiltinApp
    :: (GivenCekReqs s)
    => DefaultFun
    -> Term
    -> BuiltinRuntime CekValue
    -> CekM s CekValue
evalBuiltinApp fun term runtime = case runtime of
    BuiltinResult cost getX -> do
        spendBudgetCek (BBuiltinApp fun) cost
        case getX of
            MakeKnownFailure logs err       -> do
                ?cekEmitter logs
                throwingWithCause (InternalEvaluationError err) (Just term)
            MakeKnownSuccess x              -> pure x
            MakeKnownSuccessWithLogs logs x -> ?cekEmitter logs $> x
    _ -> pure $ VBuiltin fun term runtime

-- See Note [Compilation peculiarities].
-- | The entering point to the CEK machine's engine.
enterComputeCek
    :: forall s
    . (GivenCekReqs s)
    => Context
    -> CekValEnv
    -> Term
    -> CekM s Term
enterComputeCek = computeCek (toWordArray 0) where
    -- | The computing part of the CEK machine.
    -- Either
    -- 1. adds a frame to the context and calls 'computeCek' ('Force', 'Apply')
    -- 2. calls 'returnCek' on values ('Delay', 'LamAbs', 'Constant', 'Builtin')
    -- 3. throws 'EvaluationFailure' ('Error')
    -- 4. looks up a variable in the environment and calls 'returnCek' ('Var')
    computeCek
        :: WordArray
        -> Context
        -> CekValEnv
        -> Term
        -> CekM s Term
    -- s ; ρ ▻ {L A}  ↦ s , {_ A} ; ρ ▻ L
    computeCek !unbudgetedSteps !ctx !env (Var varName) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BVar unbudgetedSteps
        val <- lookupVarName varName env
        returnCek unbudgetedSteps' ctx val
    computeCek !unbudgetedSteps !ctx !_ (Constant val) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BConst unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VCon val)
    computeCek !unbudgetedSteps !ctx !env (LamAbs name body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BLamAbs unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VLamAbs name body env)
    computeCek !unbudgetedSteps !ctx !env (Delay body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BDelay unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VDelay body env)
    -- s ; ρ ▻ lam x L  ↦  s ◅ lam x (L , ρ)
    computeCek !unbudgetedSteps !ctx !env (Force body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BForce unbudgetedSteps
        computeCek unbudgetedSteps' (FrameForce ctx) env body
    -- s ; ρ ▻ [L M]  ↦  s , [_ (M,ρ)]  ; ρ ▻ L
    computeCek !unbudgetedSteps !ctx !env (Apply fun arg) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BApply unbudgetedSteps
        computeCek unbudgetedSteps' (FrameApplyArg env arg ctx) env fun
    -- s ; ρ ▻ abs α L  ↦  s ◅ abs α (L , ρ)
    -- s ; ρ ▻ con c  ↦  s ◅ con c
    -- s ; ρ ▻ builtin bn  ↦  s ◅ builtin bn arity arity [] [] ρ
    computeCek !unbudgetedSteps !ctx !_ (Builtin bn) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BBuiltin unbudgetedSteps
        let meaning = unBuiltinsRuntime ?cekRuntime bn
        -- 'Builtin' is fully discharged.
        returnCek unbudgetedSteps' ctx (VBuiltin bn (Builtin bn) meaning)
    -- s ; ρ ▻ error A  ↦  <> A
    computeCek !_ !_ !_ Error =
        throwingWithCause (UserEvaluationError CekEvaluationFailure) Nothing

    {- | The returning phase of the CEK machine.
    Returns 'EvaluationSuccess' in case the context is empty, otherwise pops up one frame
    from the context and uses it to decide how to proceed with the current value v.

      * 'FrameForce': call forceEvaluate
      * 'FrameApplyArg': call 'computeCek' over the context extended with 'FrameApplyFun'
      * 'FrameApplyFun': call 'applyEvaluate' to attempt to apply the function
          stored in the frame to an argument.
    -}
    returnCek
        :: WordArray
        -> Context
        -> CekValue
        -> CekM s Term
    --- Instantiate all the free variable of the resulting term in case there are any.
    -- . ◅ V           ↦  [] V
    returnCek !unbudgetedSteps NoFrame val = do
        spendAccumulatedBudget unbudgetedSteps
        pure $ dischargeCekValue val
    -- s , {_ A} ◅ abs α M  ↦  s ; ρ ▻ M [ α / A ]*
    returnCek !unbudgetedSteps (FrameForce ctx) fun = forceEvaluate unbudgetedSteps ctx fun
    -- s , [_ (M,ρ)] ◅ V  ↦  s , [V _] ; ρ ▻ M
    returnCek !unbudgetedSteps (FrameApplyArg argVarEnv arg ctx) fun =
        computeCek unbudgetedSteps (FrameApplyFun fun ctx) argVarEnv arg
    -- s , [(lam x (M,ρ)) _] ◅ V  ↦  s ; ρ [ x  ↦  V ] ▻ M
    -- FIXME: add rule for VBuiltin once it's in the specification.
    returnCek !unbudgetedSteps (FrameApplyFun fun ctx) arg =
        applyEvaluate unbudgetedSteps ctx fun arg

    -- | @force@ a term and proceed.
    -- If v is a delay then compute the body of v;
    -- if v is a builtin application then check that it's expecting a type argument,
    -- and either calculate the builtin application or stick a 'Force' on top of its 'Term'
    -- representation depending on whether the application is saturated or not,
    -- if v is anything else, fail.
    forceEvaluate
        :: WordArray
        -> Context
        -> CekValue
        -> CekM s Term
    forceEvaluate !unbudgetedSteps !ctx (VDelay body env) = computeCek unbudgetedSteps ctx env body
    forceEvaluate !unbudgetedSteps !ctx (VBuiltin fun term runtime) = do
        -- @term@ is fully discharged, and so @term'@ is, hence we can put it in a 'VBuiltin'.
        let term' = Force term
        case runtime of
            -- It's only possible to force a builtin application if the builtin expects a type
            -- argument next.
            BuiltinExpectForce runtime' -> do
                -- We allow a type argument to appear last in the type of a built-in function,
                -- otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
                -- application.
                res <- evalBuiltinApp fun term' runtime'
                returnCek unbudgetedSteps ctx res
            _ ->
                throwingWithCause (InternalEvaluationError BuiltinTermArgumentExpectedMachineError) (Just term')
    forceEvaluate !_ !_ val =
        throwingDischarged (InternalEvaluationError NonPolymorphicInstantiationMachineError) val

    -- | Apply a function to an argument and proceed.
    -- If the function is a lambda 'lam x ty body' then extend the environment with a binding of @v@
    -- to x@ and call 'computeCek' on the body.
    -- If the function is a builtin application then check that it's expecting a term argument,
    -- and either calculate the builtin application or stick a 'Apply' on top of its 'Term'
    -- representation depending on whether the application is saturated or not.
    -- If v is anything else, fail.
    applyEvaluate
        :: WordArray
        -> Context
        -> CekValue -- lhs of application
        -> CekValue -- rhs of application
        -> CekM s Term
    applyEvaluate !unbudgetedSteps !ctx (VLamAbs _ body env) arg =
        computeCek unbudgetedSteps ctx (Env.cons arg env) body
    -- Annotating @f@ and @exF@ with bangs gave us some speed-up, but only until we added a bang to
    -- 'VCon'. After that the bangs here were making things a tiny bit slower and so we removed them.
    applyEvaluate !unbudgetedSteps !ctx (VBuiltin fun term runtime) arg = do
        let argTerm = dischargeCekValue arg
            -- @term@ and @argTerm@ are fully discharged, and so @term'@ is, hence we can put it
            -- in a 'VBuiltin'.
            term' = Apply term argTerm
        case runtime of
            -- It's only possible to apply a builtin application if the builtin expects a term
            -- argument next.
            BuiltinExpectArgument f -> do
                res <- evalBuiltinApp fun term' $ f arg
                returnCek unbudgetedSteps ctx res
            _ ->
                throwingWithCause (InternalEvaluationError UnexpectedBuiltinTermArgumentMachineError)
                                  (Just term')
    applyEvaluate !_ !_ val _ =
        throwingDischarged (InternalEvaluationError NonFunctionalApplicationMachineError) val

    -- | Spend the budget that has been accumulated for a number of machine steps.
    spendAccumulatedBudget :: WordArray -> CekM s ()
    spendAccumulatedBudget !unbudgetedSteps = iforWordArray unbudgetedSteps spend

    -- Making this a definition of its own causes it to inline better than actually writing it inline, for
    -- some reason.
    -- Skip index 7, that's the total counter!
    -- See Note [Structure of the step counter]
    {-# INLINE spend #-}
    spend !i !w = unless (i == 7) $
      let kind = toEnum i in spendBudgetCek (BStep kind)
                                            (stimesExBudget w (cekStepCost ?cekCosts kind))

    -- | Accumulate a step, and maybe spend the budget that has accumulated for a number of machine steps, but only if we've exceeded our slippage.
    stepAndMaybeSpend :: StepKind -> WordArray -> CekM s WordArray
    stepAndMaybeSpend !kind !unbudgetedSteps = do
        -- See Note [Structure of the step counter]
        -- This generates let-expressions in GHC Core, however all of them bind unboxed things and
        -- so they don't survive further compilation, see https://stackoverflow.com/a/14090277
        let !ix = fromIntegral $ fromEnum kind
            !unbudgetedSteps' = overIndex 7 (+1) $ overIndex ix (+1) unbudgetedSteps
            !unbudgetedStepsTotal = readArray unbudgetedSteps' 7
        -- There's no risk of overflow here, since we only ever increment the total
        -- steps by 1 and then check this condition.
        if unbudgetedStepsTotal >= defaultSlippage
        then spendAccumulatedBudget unbudgetedSteps' >> pure (toWordArray 0)
        else pure unbudgetedSteps'

-- See Note [Compilation peculiarities].
-- | Evaluate a term using the CEK machine and keep track of costing, logging is optional.
runCekDeBruijn
    :: MachineParameters
    -> ExBudgetMode cost
    -> EmitterMode
    -> Term
    -> (Either ErrorWithCause Term, cost, [Text])
runCekDeBruijn params mode emitMode term =
    runCekM params mode emitMode $ do
        spendBudgetCek BStartup (cekStartupCost ?cekCosts)
        enterComputeCek NoFrame Env.empty term

throwingWithCause :: EvaluationError -> Maybe Term -> CekM s x
throwingWithCause e cause = throwError $ ErrorWithCause e cause
