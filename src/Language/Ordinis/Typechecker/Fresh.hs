{-# LANGUAGE TemplateHaskell #-}

module Language.Ordinis.Typechecker.Fresh where

import Control.Monad
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Static

data Fresh :: Effect

data FreshId = FreshId

type instance DispatchOf Fresh = Static WithSideEffects

newtype instance StaticRep Fresh = Fresh (IORef [Text])

fresh :: Fresh :> es => Eff es Text
fresh = do
  Fresh names <- getStaticRep
  unsafeEff_ (readIORef names) >>= \case
    [] -> error "fresh: Exhausted an infinite list...?"
    n : ns -> do
      unsafeEff_ (writeIORef names ns)
      pure n

type (~>) f g = forall a. f a -> g a

runFresh :: IOE :> es => Eff (Fresh : es) ~> Eff es
runFresh eff = do
  names <- liftIO $ newIORef ([1 ..] >>= fmap T.pack . flip replicateM ['a' .. 'z'])
  evalStaticRep (Fresh names) eff
