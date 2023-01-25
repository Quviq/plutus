module PureCake.PlutusCore.Pretty
    (
    -- * Basic types and functions
      Doc
    , Pretty (..)
    , PrettyBy (..)
    , IgnorePrettyConfig (..)
    , AttachPrettyConfig (..)
    , Render (..)
    , display
    , displayBy
    -- * Defaults
    , prettyPlcDef
    , displayPlcDef
    , prettyPlcDebug
    , displayPlcDebug
    -- * Global configuration
    , CondensedErrors (..)
    , PrettyConfigPlcOptions (..)
    , PrettyConfigPlcStrategy (..)
    , PrettyConfigPlc (..)
    , PrettyPlc
    , defPrettyConfigPlcOptions
    , defPrettyConfigPlcClassic
    , debugPrettyConfigPlcClassic
    , defPrettyConfigPlcReadable
    , debugPrettyConfigPlcReadable
    -- * Custom functions for PLC types.
    , prettyPlcClassicDef
    , prettyPlcClassicDebug
    , prettyPlcReadableDef
    , prettyPlcReadableDebug
    , prettyPlcCondensedErrorBy
    , displayPlcCondensedErrorClassic
    -- * Names
    , PrettyConfigName (..)
    , HasPrettyConfigName (..)
    , defPrettyConfigName
    , debugPrettyConfigName
    -- * Classic view
    , PrettyConfigClassic (..)
    , PrettyClassicBy
    , PrettyClassic
    , consAnnIf
    , prettyClassicDef
    , prettyClassicDebug
    -- * Readable view
    , ShowKinds (..)
    , PrettyConfigReadable (..)
    , PrettyReadableBy
    , PrettyReadable
    , topPrettyConfigReadable
    , botPrettyConfigReadable
    -- * Utils
    , prettyBytes
    , ConstConfig (..)
    , PrettyConst
    , prettyConst
    , displayConst
    ) where

import PlutusCore.Pretty.ConfigName
import PlutusCore.Pretty.PrettyConst
import PlutusCore.Pretty.Utils
import PureCake.PlutusCore.Pretty.Classic
import PureCake.PlutusCore.Pretty.Default
import PureCake.PlutusCore.Pretty.Extra ()
import PureCake.PlutusCore.Pretty.Plc
import PureCake.PlutusCore.Pretty.Readable

import Text.Pretty
import Text.PrettyBy
