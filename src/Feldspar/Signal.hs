{-# LANGUAGE TypeOperators   #-}

module Feldspar.Signal where

import           Frontend.Signal (Sig, Signal(..))
import qualified Frontend.Signal as S

import           Frontend.Stream (Str, Stream(..))
import qualified Frontend.Stream as Sr

import Control.Monad.Operational.Compositional
import Language.Embedded.Imperative

import Data.Typeable
import Text.PrettyPrint.Mainland

import qualified Backend.Compiler.Compiler   as C
import qualified Feldspar.Compiler.ToMutable as F
import qualified Text.Printf                 as Printf

import           Feldspar (M, Data)
import qualified Feldspar as F

import           Feldspar.Stream (Stream)
import qualified Feldspar.Stream as FS

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type CMD
  =   RefCMD     E
  :+: ControlCMD E
  :+: ControlCMD E

type P  a = Program CMD (IExp CMD a)
type S  a = Sig CMD a
type Sr a = Str CMD a
type E    = Data

--------------------------------------------------------------------------------
-- ** FIR Filter

fir :: [E Float] -> S Float -> S Float
fir as = sums . muls as . delays ds
  where ds = replicate (length as) 0

sums :: [S Float] -> S Float
sums = foldr1 (+)

muls :: [E Float] -> [S Float] -> [S Float]
muls as = zipWith (*) (map S.repeat as)

delays :: [E Float] -> S Float -> [S Float]
delays as s = scanl (flip S.delay) s as

--------------------------------------------------------------------------------
-- ** IIR Filter

iir :: [E Float] -> [E Float] -> S Float -> S Float
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ S.delay 0 o
    o = (1 / S.repeat a) * (u - l)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

connect_io :: (S Float -> S Float) -> IO (Program CMD ())
connect_io s = do
  prog <- compiler s
  return $ do
    ref <- initRef (6 :: Data F.Float) :: Program CMD (Ref F.Float)
    out <- newRef                      :: Program CMD (Ref F.Float)

    let stream = Sr.run $ prog
               $ Stream $ return
               $ getRef ref

    let cont = return F.true :: Program CMD (E Bool)

    while cont $ do
      o <- stream
      modifyRef ref (+1)
      setRef out o

--------------------------------------------------------------------------------

test1 :: IO ()
test1 = do
  p <- connect_io $ fir [1,2]
  F.drawAST $ F.toMutable p

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compiler :: (F.Type a, Typeable a, Typeable b) => (S a -> S b) -> IO (Sr a -> Sr b)
compiler = C.compiler

icompiler
  :: (F.Type a, Typeable a, Typeable b)
  => (S a -> S b)   -- ^ Signal function
  -> P a            -- ^ Source
  -> (P b -> P ())  -- ^ Sink
  -> IO (P ())
icompiler sf source sink = do
  srf <- compiler sf
  return $ do
    let inp = Sr.stream source
        out = Sr.run $ srf inp
    sink $ out
        
--------------------------------------------------------------------------------
{-
compilerF :: (Show a, Typeable a, Typeable b) => (S a -> S b) -> IO (Stream a -> Stream b)
compilerF sf = do
  srf <- compiler sf
  return $ do
    undefined -- impossibru!!!

icompilerF
  :: (Show a, Typeable a, Typeable b)
  => (S a -> S b)
  -> M a
  -> (M b -> M ())
  -> IO (M ())
icompilerF sf source sink = do
  srf <- compiler sf
  return $ do    
    undefined -- impossibru!!!
-}
--------------------------------------------------------------------------------
