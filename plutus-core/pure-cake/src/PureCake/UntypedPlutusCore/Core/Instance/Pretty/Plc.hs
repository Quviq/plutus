{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.UntypedPlutusCore.Core.Instance.Pretty.Plc () where

import PlutusPrelude

import PureCake.UntypedPlutusCore.Core.Instance.Pretty.Classic ()
import PureCake.UntypedPlutusCore.Core.Instance.Pretty.Readable ()
import PureCake.UntypedPlutusCore.Core.Type

import PureCake.PlutusCore.Pretty.Plc

deriving via PrettyAny (Term name uni fun ann)
    instance DefaultPrettyPlcStrategy (Term name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Term name uni fun ann)
deriving via PrettyAny (Program name uni fun ann)
    instance DefaultPrettyPlcStrategy (Program name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Program name uni fun ann)
