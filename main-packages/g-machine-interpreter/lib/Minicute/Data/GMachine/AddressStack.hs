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

import Prelude hiding ( fail )

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
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
popAddr = _Wrapped %%~= popAddr'
  where
    popAddr' (addr : addrs) = pure (addr, addrs)
    popAddr' _ = fail "popAddr: There is no address to pop"

popAddrs :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m [Address]
popAddrs n = _Wrapped %%~= popAddrs'
  where
    popAddrs' addrs =
      let
        result = splitAt n addrs
      in
        if length (fst result) == n
        then pure result
        else fail $ "popAddrs: there are not enough addresses to pop " <> show n <> " addresses"

popAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
popAllAddrs = _Wrapped <<.= []

peekAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
peekAddr = use _Wrapped >>= peekAddr'
  where
    peekAddr' (addr : _) = pure addr
    peekAddr' _ = fail "peekAddr: There is no address to peek"

peekNthAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m Address
peekNthAddr n = use _Wrapped >>= peekNthAddr'
  where
    peekNthAddr' addrs =
      case drop (n - 1) addrs of
        addr : _ -> pure addr
        _ -> fail $ "peekNthAddr: There are not enough addresses to peek the " <> show n <> "th address"

checkSize :: (MonadState s m, s ~ AddressStack) => Int -> m Bool
checkSize n = isLongerThan n <$> use _Wrapped
  where
    isLongerThan len = not . null . drop (len - 1)
