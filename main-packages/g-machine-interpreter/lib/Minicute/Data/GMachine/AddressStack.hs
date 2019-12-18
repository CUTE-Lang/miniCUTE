{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
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
  , peekAllAddrs
  , checkSize
  ) where

import Prelude hiding ( fail )

import Control.Lens.At ( ix )
import Control.Lens.Fold ( has )
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
{-# INLINE empty #-}

pushAddr :: (MonadState s m, s ~ AddressStack) => Address -> m ()
pushAddr addr = _Wrapped %= (addr :)
{-# INLINABLE pushAddr #-}

pushAddrs :: (MonadState s m, s ~ AddressStack) => [Address] -> m ()
pushAddrs addrs = _Wrapped %= (addrs <>)
{-# INLINABLE pushAddrs #-}

popAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
popAddr = _Wrapped %%~= popAddr'
  where
    popAddr' (addr : addrs) = pure (addr, addrs)
    popAddr' _ = fail "popAddr: There is no address to pop"
    {-# INLINABLE popAddr' #-}
{-# INLINABLE popAddr #-}

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
    {-# INLINABLE popAddrs' #-}
{-# INLINABLE popAddrs #-}

popAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
popAllAddrs = _Wrapped <<.= []
{-# INLINABLE popAllAddrs #-}

peekAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
peekAddr = use _Wrapped >>= peekAddr'
  where
    peekAddr' (addr : _) = pure addr
    peekAddr' _ = fail "peekAddr: There is no address to peek"
    {-# INLINABLE peekAddr' #-}
{-# INLINABLE peekAddr #-}

peekNthAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m Address
peekNthAddr n = use _Wrapped >>= peekNthAddr'
  where
    peekNthAddr' addrs
      = case addrs ^? ix n of
          Just addr -> pure addr
          _ -> fail $ "peekNthAddr: There are not enough addresses to peek the " <> show n <> "th address"
    {-# INLINABLE peekNthAddr' #-}
{-# INLINABLE peekNthAddr #-}

peekAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
peekAllAddrs = use _Wrapped
{-# INLINABLE peekAllAddrs #-}

checkSize :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m Bool
checkSize 0 = pure True
checkSize n = has (ix (n - 1)) <$> use _Wrapped
{-# INLINABLE checkSize #-}
