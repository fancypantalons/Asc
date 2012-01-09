module Cpu (execute, run) where

import Text.ParserCombinators.ReadP

import Parser
import State
import Heap as Heap
import Util

import Data.Char
import Data.Array.IArray

--
-- Helper function which takes an Addr value and translates it into an
-- absolute stack address.
-- 
translate :: State -> Addr -> Integer
translate state (Absolute i)   = i
translate state (Relative r i) = val + i
  where !val = regGet state r

--
-- Instruction execution logic.
--
execute :: State -> Instr -> IO State
execute state (PUSH a) = return $ push state $! stackGet state $ translate state a
execute state (PUSHI (Just r)) = return $ push state $! stackGet state $ ((stackTopI state) + regGet state r)
execute state (PUSHI Nothing) = return $ push newState val
  where (top, newState) = popi state
        !val = stackGet state top

execute state (PUSHA a) = return $ pushi state (translate state a)

execute state (POP a) = return $ stackSet newState (translate state a) top
  where (top, newState) = pop state

execute state (POPI (Just r)) = return $ stackSet newState addr val
  where !rval = regGet state r
        (val, addr, newState) = 
          let (v, s) = pop state 
              ((ISlot a), s2) = pop s
          in (v, a + rval, s2)

execute state (POPI Nothing) = return $ stackSet newState addr val
  where (val, addr, newState) = 
          let (v, s) = pop state 
              ((ISlot a), s2) = pop s
          in (v, a, s2)

execute state (CONSTI v) = return $ pushi state v
execute state (CONSTR v) = return $ pushd state v

execute state DUP = return $ push state $! stackTop state
execute state (ADJUST v) = return $ state { sp = sp state + v }

execute state (ALLOC i) = return $ pushi newState addr
  where (newHeap, addr) = Heap.allocate (heap state) i
        newState = state { heap = newHeap }

execute state FREE = return newState
  where (addr, s1) = popi state
        newHeap = Heap.free (heap s1) addr
        newState = s1 { heap = newHeap }

execute state ADDI = return $ combinei (+) state
execute state ADDR = return $ combined (+) state
execute state SUBI = return $ combinei (-) state
execute state SUBR = return $ combined (-) state
execute state MULI = return $ combinei (*) state
execute state MULR = return $ combined (*) state
execute state DIVI = return $ combinei (div) state
execute state DIVR = return $ combined (/) state
execute state MOD = return $ combinei (mod) state

execute state ITOR = return $ pushd newState $ (fromIntegral val :: Double)
  where (val, newState) = popi state

execute state RTOI = return $ pushi newState $ round val
  where (val, newState) = popd state

execute state EQI = return $ combinei (cmp (==)) state
execute state EQR = return $ combinedi (cmp (==)) state
execute state LTI = return $ combinei (cmp (<)) state
execute state LTR = return $ combinedi (cmp (<)) state
execute state GTI = return $ combinei (cmp (>)) state
execute state GTR = return $ combinedi (cmp (>)) state

execute state OR = return $ combinei (cmp (\a b -> (a /= 0) || (b /= 0))) state
execute state AND = return $ combinei (cmp (\a b -> (a /= 0) && (b /= 0))) state
execute state NOT = return $ pushi newState (boolToInteger $ not $ integerToBool val)
  where (val, newState) = popi state

execute state (IFZ a) = return $ newState { pc = addr }
  where (val, newState) = popi state
        addr = if (not $ integerToBool val)
                 then translate state a
                 else pc state

execute state (IFNZ a) = return $ newState { pc = addr }
  where (val, newState) = popi state
        addr = if (integerToBool val)
                 then translate state a
                 else pc state

execute state (GOTO a) = return $ state { pc = translate state a }

execute state (CALL r a) = return s4
  where s1 = pushi state $ pc state
        s2 = pushi s1 $! regGet state r
        s3 = s2 { pc = translate s2 a }
        s4 = regSet s3 r (sp s3)

execute state (RET r) = return $ s3 { pc = oldpc }
  where (oldreg, s1) = popi state
        (oldpc, s2) = popi s1
        s3 = regSet s2 r oldreg

execute state READI = do
  l <- getLine
  
  case (readP_to_S pi l) of
    []          -> error "That integer was no good anyway"
    (v, _):_ -> return $ pushi state v

  where pi = do
          skipSpaces
          v <- parseInt
          skipSpaces
          eof

          return v

execute state READR = do
  l <- getLine

  case (readP_to_S pd l) of
    []          -> error "Double bad"
    (v, _):_ -> return $ pushd state v

  where pd = do
          skipSpaces
          v <- parseDouble
          skipSpaces
          eof

          return v

execute state READC = do 
  c <- getChar
  return $ pushi state (fromIntegral $ ord c)

execute state WRITEI = do
  let (val, s2) = popi state

  (putStr $ show val) >> return s2

execute state WRITER = do
  let (val, s2) = popd state

  (putStr $ show val) >> return s2

execute state WRITEC = do
  let (val, s2) = popi state

  (putChar $ chr $ fromIntegral val) >> return s2

execute state STOP = return $ state { running = False }
execute state CORE = (putStrLn $ show state) >> return state

execute state (TRACE Nothing) = return $ newState { tracing = integerToBool flg }
  where (flg, newState) = popi state

execute state (TRACE (Just v)) = return $ state { tracing = integerToBool v }

runOnce :: State -> IO State
runOnce state = do
  if (tracing state)
    then dump state 
    else return ()

  execute s2 instr >>= return

  where instr = program state ! pc state
        s2    = state { pc = pc state + 1 }

run :: State -> IO State
run state = do
  s2 <- runOnce state

  if (running s2)
    then run s2 >>= return
    else return s2
