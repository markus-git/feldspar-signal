module Feldspar.Signal
  ( module Feldspar
  ) where



import Data.Typeable

import Language.Embedded.Imperative

import Feldspar (Type, Data, value, eval, Int32)
import Feldspar.Core.Constructs (mkVariable)
import Feldspar.Compiler.FromImperative

import Language.C.Monad
import Language.Embedded.Backend.C ()



type instance VarPred Data = Type

type Pred = Typeable :/\: Type

instance EvalExp Data
  where
    litExp  = value
    evalExp = eval

instance CompExp Data
  where
    varExp  = mkVariable
    compExp = translateExpr

refProg :: Program (Tag Pred Data (RefCMD Pred Data)) (Data Int32)
refProg = do
    r1 <- initRef 4
    r2 <- initRef 5
    a  <- unsafeFreezeRef r1
    b  <- getRef r2
    let c = a+b
    setRef r2 c
    return c

evalRef :: IO Int32
evalRef = fmap evalExp $ interpret refProg

compRef = prettyCGen $ wrapMain $ interpret refProg

