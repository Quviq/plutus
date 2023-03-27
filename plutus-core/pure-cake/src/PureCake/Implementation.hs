{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module PureCake.Implementation where

import PureCake.HaskellPrelude

type Word8 = Integer
type WordArray = [Integer]

-- | Counts size in machine words.
type ExMemory = Integer

-- | Counts CPU units in picoseconds: maximum value for Integer is 2^63 ps, or
-- appproximately 106 days.
type ExCPU = Integer

type ExRestrictingBudget = ExBudget

type BuiltinsRuntime fun val = fun -> BuiltinRuntime val

-- | The CEK machine is parameterized over a @spendBudget@ function. This makes the budgeting machinery extensible
-- and allows us to separate budgeting logic from evaluation logic and avoid branching on the union
-- of all possible budgeting state types during evaluation.
type CekBudgetSpender = ExBudgetCategory -> ExBudget -> CekM ()

-- We make a separate data type here just to save the caller of the CEK machine from those pesky
-- 'ST'-related details.
-- | A budgeting mode to execute the CEK machine in.
type ExBudgetMode = IO ExBudgetInfo

type CekEmitter = [String] -> CekM ()

-- | An emitting mode to execute the CEK machine in, similar to 'ExBudgetMode'.
type EmitterMode = IO ExBudget -> IO CekEmitterInfo

type CekM = IO

-- PURECAKE START

main :: IO ()
main = pure ()

-- TODO: this is a bit of an approximation!
throwError :: ErrorWithCause -> CekM a
throwError e = raise

initWordArray :: WordArray
initWordArray = [0,0,0,0,0,0,0,0]

overIndex :: Integer -> (Word8 -> Word8) -> WordArray -> WordArray
overIndex i f ws =
  if i == 0 then
    case ws of
      w : ws' -> f w : ws'
      []      -> error "out of bounds"
  else
    case ws of
      w : ws' -> w : overIndex (i - 1) f ws'
      []      -> error "out of bounds"

readWordArray :: WordArray -> Integer -> Word8
readWordArray a i = index a i

iforWordArray :: WordArray -> (Integer -> Word8 -> CekM ()) -> CekM ()
iforWordArray ws f =
  let go i ws' = case ws' of
        []       -> pure ()
        w : ws'' -> do
          f i w
          go (i + 1) ws''
  in go 0 ws

data ExBudget = ExBudget Integer Integer
-- data ExBudget = ExBudget ExCPU ExMemory

minusExBudget :: ExBudget -> ExBudget -> ExBudget
minusExBudget b1 b2 =
  case b1 of
    ExBudget c1 m1 -> case b2 of
      ExBudget c2 m2 -> ExBudget (c1 - c2) (m1 - m2)

stimesExBudget :: Integer -> ExBudget -> ExBudget
stimesExBudget r b = case b of
  ExBudget cpu mem -> ExBudget (r * cpu) (r * mem)

data CekMachineCosts = CekMachineCosts ExBudget ExBudget ExBudget ExBudget ExBudget ExBudget ExBudget ExBudget

cekStartupCost :: CekMachineCosts -> ExBudget
cekStartupCost x = case x of CekMachineCosts a b c d e f g h -> a
cekVarCost :: CekMachineCosts -> ExBudget
cekVarCost x = case x of CekMachineCosts a b c d e f g h -> b
cekConstCost :: CekMachineCosts -> ExBudget
cekConstCost x = case x of CekMachineCosts a b c d e f g h -> c
cekLamCost :: CekMachineCosts -> ExBudget
cekLamCost x = case x of CekMachineCosts a b c d e f g h -> d
cekDelayCost :: CekMachineCosts -> ExBudget
cekDelayCost x = case x of CekMachineCosts a b c d e f g h -> e
cekForceCost :: CekMachineCosts -> ExBudget
cekForceCost x = case x of CekMachineCosts a b c d e f g h -> f
cekApplyCost :: CekMachineCosts -> ExBudget
cekApplyCost x = case x of CekMachineCosts a b c d e f g h -> g
cekBuiltinCost :: CekMachineCosts -> ExBudget
cekBuiltinCost x = case x of CekMachineCosts a b c d e f g h -> h

defaultCekMachineCosts :: CekMachineCosts
defaultCekMachineCosts =
  CekMachineCosts (ExBudget 100 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)
                  (ExBudget 23000 100)

data MakeKnownM a
    = MakeKnownFailure [String] MachineError
    | MakeKnownSuccess a
    | MakeKnownSuccessWithLogs [String] a

data BuiltinRuntime val
    = BuiltinResult ExBudget (MakeKnownM val)
    | BuiltinExpectArgument (val -> BuiltinRuntime val)
    | BuiltinExpectForce (BuiltinRuntime val)

data StepKind
    = BConst
    | BVar
    | BLamAbs
    | BApply
    | BDelay
    | BForce
    | BBuiltin -- Cost of evaluating a Builtin AST node, not the function itself

toEnumStepKind :: Integer -> StepKind
toEnumStepKind i =
       if i == 0 then BConst
  else if i == 1 then BVar
  else if i == 2 then BLamAbs
  else if i == 3 then BApply
  else if i == 4 then BDelay
  else if i == 5 then BForce
  else if i == 6 then BBuiltin
  else error "toEnumStepKind"

fromEnumStepKind :: StepKind -> Integer
fromEnumStepKind sk = case sk of
  BConst   -> 0
  BVar     -> 1
  BLamAbs  -> 2
  BApply   -> 3
  BDelay   -> 4
  BForce   -> 5
  BBuiltin -> 6

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

-- 'Values' for the modified CEK machine.
data CekValue =
    -- This bang gave us a 1-2% speed-up at the time of writing.
    VCon Const
  | VDelay Term [CekValue]
  | VLamAbs NamedDeBruijn Term [CekValue]
    -- | A partial builtin application, accumulating arguments for eventual full application.
    -- We don't need a 'CekValEnv' here unlike in the other constructors, because 'VBuiltin'
    -- values always store their corresponding 'Term's fully discharged, see the comments at
    -- the call sites (search for 'VBuiltin').
  | VBuiltin
      DefaultFun
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
      (BuiltinRuntime CekValue)
      -- ^ The partial application and its costing function.
      -- Check the docs of 'BuiltinRuntime' for details.

-- | When unlifting of a PLC term into a Haskell value fails, this error is thrown.
data UnliftingError
    = UnliftingErrorE String

-- | Errors which can occur during a run of an abstract machine.
data MachineError
    = NonPolymorphicInstantiationMachineError
      -- ^ An attempt to reduce a not immediately reducible type instantiation.
    | NonWrapUnwrappedMachineError
      -- ^ An attempt to unwrap a not wrapped term.
    | NonFunctionalApplicationMachineError
      -- ^ An attempt to reduce a not immediately reducible application.
    | OpenTermEvaluatedMachineError
      -- ^ An attempt to evaluate an open term.
    | UnliftingMachineError UnliftingError
      -- ^ An attempt to compute a constant application resulted in 'ConstAppError'.
    | BuiltinTermArgumentExpectedMachineError
      -- ^ A builtin expected a term argument, but something else was received
    | UnexpectedBuiltinTermArgumentMachineError
      -- ^ A builtin received a term argument when something else was expected
    | UnknownBuiltin DefaultFun

-- | The type of errors (all of them) which can occur during evaluation
-- (some are used-caused, some are internal).
data EvaluationError
    = InternalEvaluationError MachineError
      -- ^ Indicates bugs.
    | UserEvaluationError CekUserError
      -- ^ Indicates user errors.

data CekUserError
    -- @plutus-errors@ prevents this from being strict. Not that it matters anyway.
    = CekOutOfExError ExBudget -- ^ The final overspent (i.e. negative) budget.
    | CekEvaluationFailure -- ^ Error has been called or a builtin application has failed

-- | An error and (optionally) what caused it.
data ErrorWithCause = ErrorWithCause EvaluationError (Maybe Term)

data NamedDeBruijn = NamedDeBruijn String Integer

data Term
    = Var NamedDeBruijn
    | LamAbs NamedDeBruijn Term
    | Apply Term Term
    | Force Term
    | Delay Term
    | Constant Const
    | Builtin DefaultFun
    | Error

data Const =
    ConstInteger Integer
  | ConstString String
  | ConstBool Bool
  | ConstUnit
  | ConstPair Const Const
  | ConstList [Const]

data DefaultFun = AddInteger

-- General enough to be able to handle a spender having one, two or any number of 'STRef's
-- under the hood.
-- | Runtime budgeting info.
data ExBudgetInfo =
  ExBudgetInfo
    (ExBudgetCategory -> ExBudget -> IO ())         -- ^ A spending function.
    (IO ExBudget) -- ^ For accessing the final state.
    (IO ExBudget)            -- ^ For accessing the cumulative budget.

getExBudgetModeSpender :: ExBudgetInfo -> CekBudgetSpender
getExBudgetModeSpender x = case x of ExBudgetInfo a b c -> a
getExBudgetModeGetFinal :: ExBudgetInfo -> IO ExRestrictingBudget
getExBudgetModeGetFinal x = case x of ExBudgetInfo a b c -> b
getExBudgetModeGetCumulative :: ExBudgetInfo -> IO ExBudget
getExBudgetModeGetCumulative x = case x of ExBudgetInfo a b c -> c

-- See Note [Cost slippage]
-- | The default number of slippage (in machine steps) to allow.
defaultSlippage :: Integer
defaultSlippage = 200

-- | Runtime emitter info, similar to 'ExBudgetInfo'.
data CekEmitterInfo = CekEmitterInfo ([String] -> IO ()) (IO [String])

getCekEmitterInfoEmit :: CekEmitterInfo -> [String] -> IO ()
getCekEmitterInfoEmit i = case i of CekEmitterInfo a b -> a
getCekEmitterInfoGetFinal :: CekEmitterInfo -> IO [String]
getCekEmitterInfoGetFinal i = case i of CekEmitterInfo a b -> b

deBruijnIndex :: NamedDeBruijn -> Integer
deBruijnIndex db = case db of NamedDeBruijn s ix -> ix

indexOne :: [a] -> Integer -> Maybe a
indexOne xs i =
  if (i - 1) >= len xs
  then Nothing
  else Just (index xs (i - 1))

-- | Convert a 'CekValue' into a 'Term' by replacing all bound variables with the terms
-- they're bound to (which themselves have to be obtain by recursively discharging values).
dischargeCekValue :: CekValue -> Term
dischargeCekValue t = case t of
    VCon val                           -> Constant val
    VDelay body env                    -> dischargeCekValEnv env $ Delay body
    -- 'computeCek' turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
    -- argument of 'computeCek' and hence we need to start discharging outside of the reassembled
    -- lambda, otherwise @name@ could clash with the names that we have in @env@.
    VLamAbs db body env ->
      case db of
        NamedDeBruijn n ix ->
          -- The index on the binder is meaningless, we put `0` by convention, see 'Binder'.
          dischargeCekValEnv env $ LamAbs (NamedDeBruijn n 0) body
    -- We only return a discharged builtin application when (a) it's being returned by the machine,
    -- or (b) it's needed for an error message.
    -- @term@ is fully discharged, so we can return it directly without any further discharging.
    VBuiltin a term b                    -> term

-- see Note [Scoping].
-- | Instantiate all the free variables of a term by looking them up in an environment.
-- Mutually recursive with dischargeCekVal.
dischargeCekValEnv :: [CekValue]
                   -> Term
                   -> Term
dischargeCekValEnv valEnv =
  -- The lamCnt is just a counter that measures how many lambda-abstractions
  -- we have descended in the `go` loop.
  let go :: Integer -> Term -> Term
      go lamCnt t0 = case t0 of
        LamAbs name body -> LamAbs name (go (lamCnt + 1) body)
        Var db ->
          let ix = deBruijnIndex db in
          if lamCnt >= ix
          -- the index n is less-than-or-equal than the number of lambdas we have descended
          -- this means that n points to a bound variable, so we don't discharge it.
          then t0
          else maybe
                 -- var is free, leave it alone
                 t0
                 -- var is in the env, discharge its value
                 dischargeCekValue
                 -- index relative to (as seen from the point of view of) the environment
                 (indexOne valEnv (ix - lamCnt))
        Apply fun arg    -> Apply (go lamCnt fun) (go lamCnt arg)
        Delay term       -> Delay (go lamCnt term)
        Force term       -> Force (go lamCnt term)
        _ -> t0
  in go 0

throwingDischarged
    :: EvaluationError
    -> CekValue
    -> CekM x
throwingDischarged e tm = throwingWithCause e $ Just (dischargeCekValue tm)

throwingWithCause :: EvaluationError -> Maybe Term -> CekM x
throwingWithCause e cause = throwError $ ErrorWithCause e cause

data Context
    = FrameApplyFun CekValue Context
    | FrameApplyArg [CekValue] Term Context
    | FrameForce Context
    | NoFrame

runCekM :: ExBudgetMode
        -> EmitterMode
        -> (CekEmitter -> CekBudgetSpender -> CekM a)
        -> IO (Maybe a, ExRestrictingBudget, [String])
runCekM getExBudgetInfo getEmitterMode a = do
    exBudgetMode   <- getExBudgetInfo
    let exBudgetModeSpender       = getExBudgetModeSpender exBudgetMode
        exBudgetModeGetFinal      = getExBudgetModeGetFinal exBudgetMode
        exBudgetModeGetCumulative = getExBudgetModeGetCumulative exBudgetMode
    cekEmitterInfo <- getEmitterMode exBudgetModeGetCumulative
    let cekEmitter             = getCekEmitterInfoEmit cekEmitterInfo
        cekEmitterInfoGetFinal = getCekEmitterInfoGetFinal cekEmitterInfo
        cekBudgetSpender       = exBudgetModeSpender
    errOrRes <- tryError $ a cekEmitter cekBudgetSpender
    st <- exBudgetModeGetFinal
    logs <- cekEmitterInfoGetFinal
    pure (errOrRes, st, logs)

-- | Look up a variable name in the environment.
lookupVarName :: NamedDeBruijn
              -> [CekValue]
              -> CekM CekValue
lookupVarName varName varEnv =
    case indexOne varEnv (deBruijnIndex varName) of
        Nothing  -> throwingWithCause (InternalEvaluationError OpenTermEvaluatedMachineError)
                      $ Just (Var varName)
        Just val -> pure val

defaultRuntime :: BuiltinsRuntime DefaultFun CekValue
defaultRuntime bi =
  let failure = BuiltinResult (ExBudget 0 0) (MakeKnownFailure [] BuiltinTermArgumentExpectedMachineError) in
  case bi of
  -- TODO: the budget here is liable to change once the tests start failing
  -- Also, I have no clue if this is actually right or if we need to use the
  -- other constructors from CekValue as well here??
    AddInteger ->
      BuiltinExpectArgument (\ c ->
        case c of
          VCon con ->
            case con of
              ConstInteger i ->
                BuiltinExpectArgument (\ c' ->
                  case c' of
                    VCon con' ->
                      case con' of
                        ConstInteger j -> BuiltinResult (ExBudget 0 0) (MakeKnownSuccess (VCon (ConstInteger (i + j))))
                        _ -> failure
                    _ -> failure
                )
              _ -> failure
          _ -> failure
      )

-- | Take pieces of a possibly partial builtin application and either create a 'CekValue' using
-- 'makeKnown' or a partial builtin application depending on whether the built-in function is
-- fully saturated or not.
evalBuiltinApp
    :: CekEmitter
    -> CekBudgetSpender
    -> DefaultFun
    -> Term
    -> BuiltinRuntime CekValue
    -> CekM CekValue
evalBuiltinApp cekEmitter cekSpender fun term runtime = case runtime of
    BuiltinResult cost getX -> do
        cekSpender (BBuiltinApp fun) cost
        let cont = case getX of
              MakeKnownFailure logs err       -> do
                  cekEmitter logs
                  throwingWithCause (InternalEvaluationError err) (Just term)
              MakeKnownSuccess x              -> pure x
              MakeKnownSuccessWithLogs logs x -> do
                cekEmitter logs
                pure x
        cont
    _ -> pure $ VBuiltin fun term runtime

-- See Note [Compilation peculiarities].
-- | The entering point to the CEK machine's engine.
enterComputeCek
    :: CekEmitter
    -> CekBudgetSpender
    -> Context
    -> [CekValue]
    -> Term
    -> CekM Term
enterComputeCek cekEmitter cekSpender =
  let -- | The computing part of the CEK machine.
      -- Either
      -- 1. adds a frame to the context and calls 'computeCek' ('Force', 'Apply')
      -- 2. calls 'returnCek' on values ('Delay', 'LamAbs', 'Constant', 'Builtin')
      -- 3. throws 'EvaluationFailure' ('Error')
      -- 4. looks up a variable in the environment and calls 'returnCek' ('Var')
      computeCek
          :: WordArray
          -> Context
          -> [CekValue]
          -> Term
          -> CekM Term
      computeCek unbudgetedSteps ctx env tm =
        case tm of
          -- s ; ρ ▻ {L A}  ↦ s , {_ A} ; ρ ▻ L
          Var varName -> do
              unbudgetedSteps' <- stepAndMaybeSpend BVar unbudgetedSteps
              val <- lookupVarName varName env
              returnCek unbudgetedSteps' ctx val
          Constant val -> do
              unbudgetedSteps' <- stepAndMaybeSpend BConst unbudgetedSteps
              returnCek unbudgetedSteps' ctx (VCon val)
          LamAbs name body -> do
              unbudgetedSteps' <- stepAndMaybeSpend BLamAbs unbudgetedSteps
              returnCek unbudgetedSteps' ctx (VLamAbs name body env)
          Delay body -> do
              unbudgetedSteps' <- stepAndMaybeSpend BDelay unbudgetedSteps
              returnCek unbudgetedSteps' ctx (VDelay body env)
          -- s ; ρ ▻ lam x L  ↦  s ◅ lam x (L , ρ)
          Force body -> do
              unbudgetedSteps' <- stepAndMaybeSpend BForce unbudgetedSteps
              computeCek unbudgetedSteps' (FrameForce ctx) env body
          -- s ; ρ ▻ [L M]  ↦  s , [_ (M,ρ)]  ; ρ ▻ L
          Apply fun arg -> do
              unbudgetedSteps' <- stepAndMaybeSpend BApply unbudgetedSteps
              computeCek unbudgetedSteps' (FrameApplyArg env arg ctx) env fun
          -- s ; ρ ▻ abs α L  ↦  s ◅ abs α (L , ρ)
          -- s ; ρ ▻ con c  ↦  s ◅ con c
          -- s ; ρ ▻ builtin bn  ↦  s ◅ builtin bn arity arity [] [] ρ
          Builtin bn -> do
              unbudgetedSteps' <- stepAndMaybeSpend BBuiltin unbudgetedSteps
              let meaning = defaultRuntime bn
              -- 'Builtin' is fully discharged.
              returnCek unbudgetedSteps' ctx (VBuiltin bn (Builtin bn) meaning)
          -- s ; ρ ▻ error A  ↦  <> A
          Error ->
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
          -> CekM Term
      returnCek unbudgetedSteps frame val =
        case frame of
          NoFrame -> do
              spendAccumulatedBudget unbudgetedSteps
              pure (dischargeCekValue val)
          FrameForce ctx -> forceEvaluate unbudgetedSteps ctx val
          FrameApplyArg argVarEnv arg ctx ->
              computeCek unbudgetedSteps (FrameApplyFun val ctx) argVarEnv arg
          FrameApplyFun fun ctx ->
              applyEvaluate unbudgetedSteps ctx fun val

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
          -> CekM Term
      forceEvaluate unbudgetedSteps ctx val =
        case val of
          VDelay body env -> computeCek unbudgetedSteps ctx env body
          VBuiltin fun term runtime ->
            -- @term@ is fully discharged, and so @term'@ is, hence we can put it in a 'VBuiltin'.
            let term' = Force term in
            case runtime of
                -- It's only possible to force a builtin application if the builtin expects a type
                -- argument next.
                BuiltinExpectForce runtime' -> do
                    -- We allow a type argument to appear last in the type of a built-in function,
                    -- otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
                    -- application.
                    res <- evalBuiltinApp cekEmitter cekSpender fun term' runtime'
                    returnCek unbudgetedSteps ctx res
                _ ->
                    throwingWithCause (InternalEvaluationError BuiltinTermArgumentExpectedMachineError) (Just term')
          _ ->
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
          -> CekM Term
      applyEvaluate unbudgetedSteps ctx val arg =
        case val of
          VLamAbs name body env ->
              computeCek unbudgetedSteps ctx (arg:env) body
          -- Annotating @f@ and @exF@ with bangs gave us some speed-up, but only until we added a bang to
          -- 'VCon'. After that the bangs here were making things a tiny bit slower and so we removed them.
          VBuiltin fun term runtime -> do
              let argTerm = dischargeCekValue arg
                  -- @term@ and @argTerm@ are fully discharged, and so @term'@ is, hence we can put it
                  -- in a 'VBuiltin'.
                  term' = Apply term argTerm
              case runtime of
                  -- It's only possible to apply a builtin application if the builtin expects a term
                  -- argument next.
                  BuiltinExpectArgument f -> do
                      res <- evalBuiltinApp cekEmitter cekSpender fun term' (f arg)
                      returnCek unbudgetedSteps ctx res
                  _ ->
                      throwingWithCause (InternalEvaluationError UnexpectedBuiltinTermArgumentMachineError)
                                        (Just term')
          _ ->
              throwingDischarged (InternalEvaluationError NonFunctionalApplicationMachineError) val

      -- | Spend the budget that has been accumulated for a number of machine steps.
      spendAccumulatedBudget :: WordArray -> CekM ()
      spendAccumulatedBudget unbudgetedSteps = iforWordArray unbudgetedSteps spend

      -- Making this a definition of its own causes it to inline better than actually writing it inline, for
      -- some reason.
      -- Skip index 7, that's the total counter
      -- See Note [Structure of the step counter]
      {-# INLINE spend #-}
      spend i w =
        if i == 7 then pure () else
        let kind = toEnumStepKind i in
        cekSpender (BStep kind)
                   (stimesExBudget w (cekStepCost defaultCekMachineCosts kind))

      -- | Accumulate a step, and maybe spend the budget that has accumulated for a number of machine steps, but only if we've exceeded our slippage.
      stepAndMaybeSpend :: StepKind -> WordArray -> CekM WordArray
      stepAndMaybeSpend kind unbudgetedSteps =
          -- See Note [Structure of the step counter]
          -- This generates let-expressions in GHC Core, however all of them bind unboxed things and
          -- so they don't survive further compilation, see https://stackoverflow.com/a/14090277
          let ix = fromEnumStepKind kind
              unbudgetedSteps' = overIndex 7 (\ x -> x + 1) (overIndex ix (\ x -> x + 1) unbudgetedSteps)
              unbudgetedStepsTotal = readWordArray unbudgetedSteps' 7
          -- There's no risk of overflow here, since we only ever increment the total
          -- steps by 1 and then check this condition.
          in if unbudgetedStepsTotal >= defaultSlippage
             then do spendAccumulatedBudget unbudgetedSteps'
                     pure initWordArray
             else pure unbudgetedSteps'
  in computeCek initWordArray

-- See Note [Compilation peculiarities].
-- | Evaluate a term using the CEK machine and keep track of costing, logging is optional.
runCekDeBruijn
    :: ExRestrictingBudget
    -> EmitterMode
    -> Term
    -> IO (Maybe Term, ExRestrictingBudget, [String])
runCekDeBruijn limit emitMode term =
    runCekM (restricting limit) emitMode (\ cekEmitter cekSpender -> do
        cekSpender BStartup (cekStartupCost defaultCekMachineCosts)
        enterComputeCek cekEmitter cekSpender NoFrame [] term)

-- | For execution, to avoid overruns.
restricting :: ExRestrictingBudget -> ExBudgetMode
restricting initB = case initB of
    ExBudget cpuInit memInit -> do
      -- We keep the counters in a PrimArray. This is better than an STRef since it stores its contents unboxed.
      --
      -- If we don't specify the element type then GHC has difficulty inferring it, but it's
      -- annoying to specify the monad, since it refers to the 's' which is not in scope.
      ref <- newArray 2
      let
          cpuIx = 0
          memIx = 1
          readCpu = readArray ref cpuIx
          writeCpu cpu = writeArray ref cpuIx cpu
          readMem = readArray ref memIx
          writeMem mem = writeArray ref memIx mem

      writeCpu cpuInit
      writeMem memInit
      let
          spend w b = case b of
            ExBudget cpuToSpend memToSpend -> do
              cpuLeft <- readCpu
              memLeft <- readMem
              let cpuLeft' = cpuLeft - cpuToSpend
              let memLeft' = memLeft - memToSpend
              -- Note that even if we throw an out-of-budget error, we still need to record
              -- what the final state was.
              writeCpu cpuLeft'
              writeMem memLeft'
              if (cpuLeft' < 0) || (memLeft' < 0)
                then do
                    let budgetLeft = ExBudget cpuLeft' memLeft'
                    throwingWithCause
                        (UserEvaluationError (CekOutOfExError budgetLeft))
                        Nothing
                else pure ()
          remaining = do
            cpu <- readCpu
            mem <- readMem
            pure (ExBudget cpu mem)
          cumulative = do
              r <- remaining
              pure (minusExBudget initB r)
          final = remaining
      pure (ExBudgetInfo spend final cumulative)

-- PURECAKE STOP
