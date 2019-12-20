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
import Data.Foldable
import Language.Haskell.TH

makeTupleZippers :: Int -> DecsQ
makeTupleZippers n
  = assert (n >= 1)
    $ fmap fold . traverse makeTupleZipper $ [1..n]
  where
    makeTupleZipper m
      = assert (m >= 1)
        $ (:) <$> makeTupleZipperClass <*> makeTupleZipperInstances
      where
        className = mkName $ "TupleZipper" <> suffix
        unzipName = mkName $ "tupleUnzip" <> suffix
        zipName = mkName $ "tupleZip" <> suffix
        suffix = show m

        makeTupleZipperClass = do
          t <- newName "t"
          t' <- newName "t"
          classD (cxt []) className [plainTV t, plainTV t'] [funDep [t'] [t], funDep [t] [t']]
            [ sigD unzipName $ arrowT `appsT` fmap varT [t, t']
            , sigD zipName $ arrowT `appsT` fmap varT [t', t]
            ]

        makeTupleZipperInstances
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

tupT :: [TypeQ] -> TypeQ
tupT ts = tupleT (length ts) `appsT` ts
{-# INLINE tupT #-}

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT
{-# INLINE appsT #-}
