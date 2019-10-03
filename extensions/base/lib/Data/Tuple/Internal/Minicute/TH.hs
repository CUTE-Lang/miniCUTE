{-# OPTIONS_HADDOCK prune #-}
-- |
-- TH functions to define tuple functions
module Data.Tuple.Internal.Minicute.TH
  ( makeTupleZippers
  ) where

import Control.Monad
import Data.Foldable
import Language.Haskell.TH

makeTupleZippers :: Int -> DecsQ
makeTupleZippers n
  = fmap fold . traverse makeTupleZipper $ [1..n]
  where
    -- m should be equal to or greater than 1
    makeTupleZipper m
      = (:) <$> makeTupleZipperClass <*> makeTupleZipperInstances
      where
        className = mkName $ "TupleZipper" <> show m
        unzipName = mkName $ "tupleUnzip" <> show m
        zipName = mkName $ "tupleZip" <> show m

        makeTupleZipperClass = do
          t <- newName "t"
          t' <- newName "t"
          classD (cxt []) className [plainTV t, plainTV t'] [funDep [t'] [t], funDep [t] [t']]
            [ sigD unzipName $ arrowT `appT` varT t `appT` varT t'
            , sigD zipName $ arrowT `appT` varT t' `appT` varT t
            ]

        makeTupleZipperInstances
          = traverse makeTupleZipperInstance [(max 3 (m + 1))..(n + 1)]

        -- l should be equal to or greater than 3
        makeTupleZipperInstance l = do
          names <- replicateM l $ newName "v"
          let
            (beforeNames, aName : bName : afterNames) = splitAt (m - 1) names
            abT = tupleT 2 `appT` varT aName `appT` varT bName
            t = tupleT (l - 1) `appsT` (fmap varT beforeNames <> [abT] <> fmap varT afterNames)
            t' = tupleT l `appsT` fmap varT names
            abP = tupP $ fmap varP [aName, bName]
            p = tupP $ fmap varP beforeNames <> [abP] <> fmap varP afterNames
            p' = tupP $ fmap varP names
            abV = tupE $ fmap varE [aName, bName]
            v = tupE $ fmap varE beforeNames <> [abV] <> fmap varE afterNames
            v' = tupE $ fmap varE names
          instanceD (cxt []) (conT className `appT` t `appT` t')
            [ funD unzipName [clause [p] (normalB v') []]
            , funD zipName [clause [p'] (normalB v) []]
            , pragInlD unzipName Inlinable FunLike AllPhases
            , pragInlD zipName Inlinable FunLike AllPhases
            ]

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT
