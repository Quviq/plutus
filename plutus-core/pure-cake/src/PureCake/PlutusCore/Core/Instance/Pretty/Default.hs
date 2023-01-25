-- | While the flexible pretty-printing infrastructure is useful when you want it,
-- it's helpful to have an implementation of the default Pretty typeclass that
-- does the default thing.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module PureCake.PlutusCore.Core.Instance.Pretty.Default () where

import PlutusPrelude

import PlutusCore.Pretty.PrettyConst
import PureCake.PlutusCore.Core.Instance.Pretty.Classic ()
import PureCake.PlutusCore.Core.Type
import PureCake.PlutusCore.Name
import PureCake.PlutusCore.Pretty.Classic

import Universe

instance Pretty TyName where
    pretty = prettyClassicDef

instance Pretty Name where
    pretty = prettyClassicDef

instance Pretty ann => Pretty (Kind ann) where
    pretty = prettyClassicDef

instance (PrettyClassic tyname, Pretty (SomeTypeIn uni), Pretty ann) =>
            Pretty (Type tyname uni ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic tyname
        , PrettyClassic name
        , Pretty (SomeTypeIn uni)
        , Closed uni, uni `Everywhere` PrettyConst
        , Pretty fun
        , Pretty ann
        ) => Pretty (Term tyname name uni fun ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic tyname
        , PrettyClassic name
        , Pretty (SomeTypeIn uni)
        , Closed uni, uni `Everywhere` PrettyConst
        , Pretty fun
        , Pretty ann
        ) => Pretty (Program tyname name uni fun ann) where
    pretty = prettyClassicDef
