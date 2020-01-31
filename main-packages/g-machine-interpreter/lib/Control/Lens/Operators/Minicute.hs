-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Control.Lens.Operators.Minicute
  ( (%%~=)
  ) where

import Control.Lens.Internal.Getter ( AlongsideRight(..) )
import Control.Lens.Type ( LensLike )
import Control.Monad.State ( MonadState(..) )


-- |
-- @_l %%~= f@ statefully modifies targets of @_l@ using @f@,
-- and returns some extra information retured from @f@.
(%%~=) :: (MonadState s m) => LensLike (AlongsideRight m r) s s a b -> (a -> m (r, b)) -> m r
_l %%~= f = do
  st <- get
  -- This operation cannot use 'Control.Lens.Wrapped.alaf'
  -- because 'AlongsideRight' does not have an instance of
  -- 'Control.Lens.Wrapped.Wrapped'.
  pair <- getAlongsideRight $ _l (AlongsideRight . f) st
  state . const $ pair
{-# INLINABLE (%%~=) #-}

infixr 4 %%~=
