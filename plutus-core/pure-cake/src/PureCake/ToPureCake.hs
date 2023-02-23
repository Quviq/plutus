{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
module PureCake.ToPureCake where

import PlutusCore.DeBruijn.Internal qualified as PLC
import PlutusCore.Default.Builtins qualified as PLC
import PlutusCore.Default.Universe qualified as PLC
import UntypedPlutusCore.Core qualified as PLC

import PureCake.PlutusCore.DeBruijn.Internal qualified as Cake
import PureCake.PlutusCore.Default.Builtins qualified as Cake
import PureCake.PlutusCore.Default.Universe qualified as Cake
import PureCake.UntypedPlutusCore.Core qualified as Cake

termToCake :: PLC.Term  PLC.NamedDeBruijn  PLC.DefaultUni PLC.DefaultFun ann
           -> Cake.Term Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun ann
termToCake = \ case
  PLC.Var ann name      -> Cake.Var ann (nameToCake name)
  PLC.LamAbs ann name t -> Cake.LamAbs ann (nameToCake name) (termToCake t)
  PLC.Apply ann s t     -> Cake.Apply ann (termToCake s) (termToCake t)
  PLC.Force ann t       -> Cake.Force ann (termToCake t)
  PLC.Delay ann t       -> Cake.Delay ann (termToCake t)
  PLC.Constant ann c    -> Cake.Constant ann (constToCake c)
  PLC.Builtin ann fun   -> Cake.Builtin ann (funToCake fun)
  PLC.Error ann         -> Cake.Error ann

funToCake :: PLC.DefaultFun -> Cake.DefaultFun
funToCake = toEnum . fromEnum

constToCake :: PLC.Some (PLC.ValueOf PLC.DefaultUni) -> Cake.Some (Cake.ValueOf Cake.DefaultUni)
constToCake (PLC.Some val) = Cake.Some (valToCake val)

valToCake :: PLC.ValueOf PLC.DefaultUni a -> Cake.ValueOf Cake.DefaultUni a
valToCake (PLC.ValueOf uni a) = Cake.ValueOf (uniToCake uni) a

uniToCake :: forall k (a :: k). PLC.DefaultUni (PLC.Esc a) -> Cake.DefaultUni (Cake.Esc a)
uniToCake = \ case
    PLC.DefaultUniInteger    -> Cake.DefaultUniInteger
    PLC.DefaultUniByteString -> Cake.DefaultUniByteString
    PLC.DefaultUniString     -> Cake.DefaultUniString
    PLC.DefaultUniUnit       -> Cake.DefaultUniUnit
    PLC.DefaultUniBool       -> Cake.DefaultUniBool
    PLC.DefaultUniProtoList  -> Cake.DefaultUniProtoList
    PLC.DefaultUniProtoPair  -> Cake.DefaultUniProtoPair
    PLC.DefaultUniApply f a  -> Cake.DefaultUniApply (uniToCake f) (uniToCake a)
    PLC.DefaultUniData       -> Cake.DefaultUniData

nameToCake :: PLC.NamedDeBruijn -> Cake.NamedDeBruijn
nameToCake (PLC.NamedDeBruijn str (PLC.Index ix)) = Cake.NamedDeBruijn str (Cake.Index ix)

