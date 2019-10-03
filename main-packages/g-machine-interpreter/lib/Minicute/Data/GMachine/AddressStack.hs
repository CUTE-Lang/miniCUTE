{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.AddressStack
  ( module Minicute.Data.GMachine.Address

  , AddressStack
  , empty
  , pushAddr
  , pushAddrs
  , popAddr
  , popAddrs
  , popAllAddrs
  , peekAddr
  , peekNthAddr
  , checkSize
  ) where

import Control.Lens.Getter ( to, use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Minicute.Data.GMachine.Address

newtype AddressStack
  = AddressStack [Address]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeWrapped ''AddressStack

empty :: AddressStack
empty = AddressStack []

pushAddr :: (MonadState s m, s ~ AddressStack) => Address -> m ()
pushAddr addr = _Wrapped %= (addr :)

pushAddrs :: (MonadState s m, s ~ AddressStack) => [Address] -> m ()
pushAddrs addrs = _Wrapped %= (addrs <>)

popAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
popAddr = do
  addrs <- use _Wrapped
  case addrs of
    addr : addrs' -> do
      _Wrapped .= addrs'
      pure addr
    _ -> fail "popAddr: There is no address to pop"

popAddrs :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m [Address]
popAddrs n = do
  addrs <- use _Wrapped
  let (result, addrs') = splitAt n addrs
  when (length result < n) $
    fail $ "popAddrs: there are not enough addresses to pop " <> show n <> " addresses"
  _Wrapped .= addrs'
  pure result

popAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
popAllAddrs = _Wrapped <<.= []

peekAddr :: (MonadState s m, s ~ AddressStack) => m Address
peekAddr = do
  addrs <- use _Wrapped
  case addrs of
    addr : _ -> pure addr
    _ -> fail "peekAddr: There is no address to peek"

peekNthAddr :: (MonadState s m, s ~ AddressStack) => Int -> m Address
peekNthAddr n = do
  addrs <- use _Wrapped
  case drop (n - 1) addrs of
    addr : _ -> pure addr
    _ -> fail $ "peekNthAddr: There are not enough addresses to peek the " <> show n <> "th address"

checkSize :: (MonadState s m, s ~ AddressStack) => Int -> m Bool
checkSize n = use (_Wrapped . to (isLongerThan n))
  where
    isLongerThan len = not . null . drop (len - 1)
