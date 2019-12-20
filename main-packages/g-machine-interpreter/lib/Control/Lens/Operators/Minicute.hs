-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Control.Lens.Operators.Minicute
  ( (%%~=)
  ) where

import Control.Lens.Internal.Getter ( AlongsideRight(..) )
import Control.Lens.Type
import Control.Monad.State ( MonadState(..) )

infixr 4 %%~=

(%%~=) :: (MonadState s m) => LensLike (AlongsideRight m r) s s a b -> (a -> m (r, b)) -> m r
_l %%~= f = do
  st <- get
  pair <- getAlongsideRight . _l (AlongsideRight . f) $ st
  state (const pair)
{-# INLINABLE (%%~=) #-}
