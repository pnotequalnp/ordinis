module Language.Ordinis.Typechecker where

import Effectful
import Effectful.Error.Static
import Language.Ordinis.Syntax

data TypeError
  = UnificationError

runTypechecker :: Error TypeError :> es => Module Located -> Eff es ()
runTypechecker m = error "not implemented"
