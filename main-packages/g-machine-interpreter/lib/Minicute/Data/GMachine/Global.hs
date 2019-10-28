{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Global
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , Global
  , empty
  , allocAddress
  , updateAddress
  , findAddress
  ) where

import Prelude hiding ( fail )

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import GHC.Generics
import Minicute.Data.Common ( Identifier(..) )
import Minicute.Data.GMachine.Address

import qualified Data.Map as Map
import qualified Data.Text.Prettyprint.Doc as PP

newtype Global
  = Global (Map.Map Identifier Address)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

instance Pretty Global where
  pretty (Global m)
    = "global"
      PP.<+>
      PP.vsep
      ( if Map.null m
        then
          [ "{"
          , "}"
          ]
        else
          [ "{"
          , PP.indent 2 . prettyGlobalItems $ globalItems
          , "}"
          ]
      )
    where
      globalMaxIdLen
        = maximum
          . fmap (length . \(Identifier str, _) -> str)
          $ globalItems
      globalItems = Map.toAscList m

      prettyGlobalItems = PP.vsep . fmap prettyGlobalItem
      prettyGlobalItem (ident, addr)
        = PP.fill globalMaxIdLen (pretty ident)
          PP.<+> "->" PP.<+> pretty addr

makeWrapped ''Global

empty :: Global
empty = Global Map.empty

allocAddress :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
allocAddress ident addr = _Wrapped . at ident .= Just addr

updateAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> Address -> m ()
updateAddress ident addr = _Wrapped . at ident %%~= updateAddress'
  where
    updateAddress' (Just _) = pure ((), Just addr)
    updateAddress' Nothing = fail $ "updateAddress: No registered address for the identifier " <> show ident

findAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> m Address
findAddress ident = use (_Wrapped . at ident) >>= findAddress'
  where
    findAddress' (Just addr) = pure addr
    findAddress' Nothing = fail $ "findAddress: No registered address for the identifier " <> show ident
