-- | Reexports from modules from the @Builtin@ folder.
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module PureCake.PlutusCore.Builtin where

data BuiltinRuntime val = BuiltinRuntime

data BuiltinsRuntime fun val = BuiltinsRuntime
    { unBuiltinsRuntime :: fun -> BuiltinRuntime val
    }

data BuiltinMeaning val cost = BuiltinMeaning (cost -> BuiltinRuntime val)
