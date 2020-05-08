{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Control.Lens.At ( ix )
import Control.Lens.Fold ( has )
import Control.Lens.Getter ( use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
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

_addressStack :: Iso' AddressStack [Address]
_addressStack = coerced
{-# INLINE _addressStack #-}


empty :: AddressStack
empty = AddressStack []
{-# INLINE empty #-}

pushAddr :: (MonadState s m, s ~ AddressStack) => Address -> m ()
pushAddr addr = _addressStack %= (addr :)
{-# INLINE pushAddr #-}

pushAddrs :: (MonadState s m, s ~ AddressStack) => [Address] -> m ()
pushAddrs addrs = _addressStack %= (addrs <>)
{-# INLINE pushAddrs #-}

popAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
popAddr = _addressStack %%~= popAddr'
  where
    popAddr' (addr : addrs) = pure (addr, addrs)
    popAddr' _ = fail "popAddr: There is no address to pop"
    {-# INLINE popAddr' #-}
{-# INLINE popAddr #-}

popAddrs :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m [Address]
popAddrs n = _addressStack %%~= popAddrs'
  where
    popAddrs' addrs =
      let
        result = splitAt n addrs
      in
        if length (fst result) == n
        then pure result
        else fail $ "popAddrs: there are not enough addresses to pop " <> show n <> " addresses"
    {-# INLINE popAddrs' #-}
{-# INLINE popAddrs #-}

popAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
popAllAddrs = _addressStack <<.= []
{-# INLINE popAllAddrs #-}

peekAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => m Address
peekAddr = use _addressStack >>= peekAddr'
  where
    peekAddr' (addr : _) = pure addr
    peekAddr' _ = fail "peekAddr: There is no address to peek"
    {-# INLINE peekAddr' #-}
{-# INLINE peekAddr #-}

peekNthAddr :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m Address
peekNthAddr n = use _addressStack >>= peekNthAddr'
  where
    peekNthAddr' addrs
      = case addrs ^? ix n of
          Just addr -> pure addr
          _ -> fail $ "peekNthAddr: There are not enough addresses to peek the " <> show n <> "th address"
    {-# INLINE peekNthAddr' #-}
{-# INLINE peekNthAddr #-}

peekAllAddrs :: (MonadState s m, s ~ AddressStack) => m [Address]
peekAllAddrs = use _addressStack
{-# INLINE peekAllAddrs #-}

checkSize :: (MonadState s m, s ~ AddressStack, MonadFail m) => Int -> m Bool
checkSize 0 = pure True
checkSize n = has (ix (n - 1)) <$> use _addressStack
{-# INLINE checkSize #-}
