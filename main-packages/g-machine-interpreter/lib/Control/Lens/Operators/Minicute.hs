-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Control.Lens.Operators.Minicute
  ( (%%~=)
  ) where

import Control.Lens.Internal.Getter ( AlongsideLeft(..), getAlongsideLeft )
import Control.Lens.Type
import Control.Monad.State ( MonadState, gets, put )
import Data.Tuple ( swap )

infixr 4 %%~=

(%%~=) :: (MonadState s m) => LensLike (AlongsideLeft m r) s s a b -> (a -> m (r, b)) -> m r
_l %%~= f = do
  along <- gets $ _l (AlongsideLeft . fmap swap . f)
  (st, res) <- getAlongsideLeft along
  put st
  pure res
