{-# LANGUAGE TemplateHaskell #-}

module Language.Ordinis.Typechecker.Names
  ( Names,
    newName,
    runNames,
    sequentialNames,
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Static

data Names :: Effect

type instance DispatchOf Names = Static NoSideEffects

newtype instance StaticRep Names = Names [Text]

newName :: Names :> es => Eff es Text
newName =
  getStaticRep @Names >>= \case
    Names [] -> error "newName: exhausted name supply"
    Names (name : names) -> do
      putStaticRep (Names names)
      pure name

-- | The provided list must be an infinite stream
runNames :: [Text] -> Eff (Names : es) a -> Eff es a
runNames = evalStaticRep . Names

-- | All strings in lexographic order
sequentialNames :: [Text]
sequentialNames = [1 ..] >>= fmap T.pack . flip replicateM ['a' .. 'z']
