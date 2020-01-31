{-# OPTIONS_HADDOCK hide #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- TH functions to define tuple functions
module Data.Tuple.Minicute.Internal.TH
  ( makeTupleZippers
  ) where

import Control.Exception
import Control.Monad
import Data.Monoid ( Ap(..) )
import Language.Haskell.TH

-- |
-- @makeTupleZippers n@ generates the classes and instances of @TupleZipperN@s
-- where @N@s are the numbers between @1@ and @n@ inclusively.
makeTupleZippers :: Int -> DecsQ
makeTupleZippers n
  = assert (n >= 1)
    $ getAp . foldMap (Ap . makeTupleZipper) $ [1..n]
  where
    makeTupleZipper m
      = assert (m >= 1)
        $ (:) <$> tupleZipperClass <*> tupleZipperInstances
      where
        className = mkName $ "TupleZipper" <> suffix
        unzipName = mkName $ "tupleUnzip" <> suffix
        zipName = mkName $ "tupleZip" <> suffix
        suffix = show m

        tupleZipperClass = do
          t <- newName "v"
          t' <- newName "v"
          classD (cxt []) className [plainTV t, plainTV t'] [funDep [t'] [t], funDep [t] [t']]
            [ sigD unzipName $ arrowT `appsT` fmap varT [t, t']
            , sigD zipName $ arrowT `appsT` fmap varT [t', t]
            ]

        tupleZipperInstances
          = traverse makeTupleZipperInstance [(max 3 (m + 1))..(n + 1)]

        makeTupleZipperInstance l = assert (l >= 3) $ do
          names <- replicateM l $ newName "v"
          let
            (beforeNames, aName : bName : afterNames) = splitAt (m - 1) names

            makeTupleZipperTuples tupF varF = (f, f')
              where
                abF = tupF $ fmap varF [aName, bName]
                f = tupF $ fmap varF beforeNames <> [abF] <> fmap varF afterNames
                f' = tupF $ fmap varF names

            (t, t') = makeTupleZipperTuples tupT varT
            (p, p') = makeTupleZipperTuples tupP varP
            (e, e') = makeTupleZipperTuples tupE varE

          instanceD (cxt []) (conT className `appsT` [t, t'])
            [ funD unzipName [clause [p] (normalB e') []]
            , funD zipName [clause [p'] (normalB e) []]
            , pragInlD unzipName Inlinable FunLike AllPhases
            , pragInlD zipName Inlinable FunLike AllPhases
            ]

        {-# INLINE tupleZipperClass #-}
        {-# INLINE tupleZipperInstances #-}
        {-# INLINE makeTupleZipperInstance #-}

tupT :: [TypeQ] -> TypeQ
tupT ts = tupleT (length ts) `appsT` ts
{-# INLINE tupT #-}

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT
{-# INLINE appsT #-}
