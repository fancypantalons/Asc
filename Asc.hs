module Asc where

import Parser
import Array
import Data.List
import Text.ParserCombinators.ReadP
import Data.Char
import System (getArgs)

data Slot = ISlot Integer | DSlot Double | Empty
          deriving Show

type RegisterSet = Array Integer Integer
type Memory = Array Integer Slot
type InstrList = Array Integer Instr

data State = State { 
               pc      :: Integer,
               sp      :: Integer,
               hp      :: Integer,
               running :: Bool,
               tracing :: Bool,
               display :: RegisterSet,
               stack   :: Memory,
               program :: InstrList,
               blocks  :: [Integer]
             } deriving Show

--
-- Given the specified program, extract all the labels and map them to their
-- addresses.
--
getLabels :: Program -> [(String, Integer)]
getLabels p = extract p 0
  where extract [] i = []
        extract ((labels, _):rest) i = (map (\l -> (l, i)) labels) ++ (extract rest (i + 1))

--
-- Given a program, translate all the labels to addresses and return a raw
-- instruction stream of the transformed program.
--
resolveLabels :: Program -> [Instr]
resolveLabels p = map (\(_, i) -> translate i) p
  where labels = getLabels p
   
        resolve lbl = 
          case (lookup lbl labels) of
            Nothing   -> error ("Unknown label encountered: " ++ lbl)
            Just addr -> Absolute addr

        translate (PUSH (Named lbl))   = PUSH (resolve lbl)
        translate (PUSHA (Named lbl))  = PUSHA (resolve lbl)
        translate (POP (Named lbl))    = POP (resolve lbl)
        translate (IFZ (Named lbl))    = IFZ (resolve lbl)
        translate (IFNZ (Named lbl))   = IFNZ (resolve lbl)
        translate (IFERR (Named lbl))  = IFERR (resolve lbl)
        translate (GOTO (Named lbl))   = GOTO (resolve lbl)
        translate (CALL r (Named lbl)) = CALL r (resolve lbl)
        translate x                    = x

newState :: Program -> State
newState p = State {
             display = array (0, 15) [ (i, 0) | i <- [0..15] ],
             stack   = array (0, 32767) [ (i, Empty) | i <- [0..32767] ],
             program = listArray (0, len - 1) listing,
             pc      = 0,
             sp      = 0,
             hp      = 32768,
             blocks  = [],
             running = True,
             tracing = False
           }
  where listing = resolveLabels p
        len     = fromIntegral $ length listing

--
-- Various helper functions for the instruction implementation.
-- 

translate :: State -> Addr -> Integer
translate state (Absolute i)   = i
translate state (Relative r i) = regGet state r + i

stackGet :: State -> Integer -> Slot
stackGet state i = (stack state) ! i

stackSet :: State -> Integer -> Slot -> State
stackSet state i slot = state { stack = (stack state) // [(i, slot)] }

stackTop :: State -> Slot
stackTop state = stackGet state $ (sp state - 1)

stackTopI :: State -> Integer
stackTopI state = case (stackTop state) of
                    (ISlot val) -> val
                    _           -> error "Expected integer but found something else"

regGet :: State -> Integer -> Integer
regGet state i = (display state) ! i

regSet :: State -> Integer -> Integer -> State
regSet state i val = state { display = (display state) // [(i, val)] }

push :: State -> Slot -> State
push state slot = (stackSet state (sp state) slot) { sp = sp state + 1 }

pushi :: State -> Integer -> State
pushi state val = push state (ISlot val)

pushd :: State -> Double -> State
pushd state val = push state (DSlot val)

pop :: State -> (Slot, State)
pop state 
  | (pc state) == 0 = error "Tried to pop past top of stack"
  | otherwise       = (slot, newState)
  where newState = state { sp = sp state - 1 }
        slot     = stackGet newState (sp newState)

popi :: State -> (Integer, State)
popi state = 
  case (pop state) of
    ((ISlot val), state) -> (val, state)
    _                    -> error "Expected integer slot but found something else"

popd :: State -> (Double, State)
popd state =
  case (pop state) of
    ((DSlot val), state) -> (val, state)
    _                    -> error "Expected double slot but found something else"

pop2 :: State -> (Slot, Slot, State)
pop2 state = let (a, s1) = pop state 
                 (b, s2) = pop s1
             in (a, b, s2)

combine :: (Slot -> Slot -> Slot) -> State -> State
combine op state = push newState result
  where (a, b, newState) = pop2 state
        result           = op b a

combinei :: (Integer -> Integer -> Integer) -> State -> State
combinei op state = combine (\(ISlot a) (ISlot b) -> ISlot (op a b)) state

combined :: (Double -> Double -> Double) -> State -> State
combined op state = combine (\(DSlot a) (DSlot b) -> DSlot (op a b)) state

combinedi :: (Double -> Double -> Integer) -> State -> State
combinedi op state = combine (\(DSlot a) (DSlot b) -> ISlot (op a b)) state

cmp :: (a -> a -> Bool) -> a -> a -> Integer
cmp pred a b = case (pred a b) of
                 True -> 1
                 False -> 0

--
-- Instruction execution logic.
--
execute :: State -> Instr -> IO State
execute state (PUSH a) = return $ push state (stackGet state $ translate state a)
execute state (PUSHI Nothing) = return $ push state (stackTop state)
execute state (PUSHI (Just r)) = return $ push state (stackGet state $ ((stackTopI state) + regGet state r))
execute state (PUSHA a) = return $ pushi state (translate state a)
execute state (PUSHR r) = return $ pushi state (regGet state r)

execute state (POP a) = return $ stackSet newState (translate state a) top
  where (top, newState) = pop state

execute state (POPI Nothing) = return $ stackSet newState addr val
  where (val, addr, newState) = 
          let (v, s) = pop state 
              ((ISlot a), s2) = pop s
          in (v, a, s2)

execute state (POPI (Just r)) = return $ stackSet newState addr val
  where (val, addr, newState) = 
          let (v, s) = pop state 
              ((ISlot a), s2) = pop s
          in (v, a + (regGet state r), s2)

execute state (POPR r) = return $ regSet newState r val
  where (val, newState) = popi state

execute state (CONSTI v) = return $ pushi state v
execute state (CONSTR v) = return $ pushd state v

execute state DUP = return $ push state (stackTop state)
execute state (ADJUST v) = return $ state { sp = sp state + v }

execute state (ALLOC i) = return state
execute state FREE = return state

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

execute state OR = return $ combinei (cmp (\a b -> (a == 1) || (b == 1))) state
execute state AND = return $ combinei (cmp (\a b -> (a == 1) && (b == 1))) state
execute state NOT = return $ pushi newState (result val)
  where (val, newState) = popi state
        result 1 = 0
        result 0 = 1
        result _ = error "Buh?"

execute state (IFZ a) = return $ newState { pc = addr }
  where (val, newState) = popi state
        addr = case val of
                 0 -> translate state a
                 1 -> pc state

execute state (IFNZ a) = return $ newState { pc = addr }
  where (val, newState) = popi state
        addr = case val of
                 1 -> translate state a
                 0 -> pc state

execute state (GOTO a) = return $ state { pc = translate state a }

execute state (CALL r a) = return s4
  where s1 = pushi state $ pc state
        s2 = pushi s1 $ regGet state r
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
  putStr $ show val
  return s2

  where (val, s2) = popi state

execute state WRITER = do
  putStr $ show val
  return s2

  where (val, s2) = popd state

execute state WRITEC = do
  putChar $ chr $ fromIntegral val
  return s2

  where (val, s2) = popi state

execute state STOP = return $ state { running = False }
execute state CORE = do
  putStrLn $ show state
  execute state STOP

execute state (TRACE v) = return $ state { tracing = val }
  where val = if (v == 0) 
                then False
                else True

runOnce :: State -> IO State
runOnce state = do
  if (tracing state)
    then putStrLn $ show instr
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

load :: [String] -> State
load listing = newState $ parse listing

loadFile :: String -> IO State
loadFile f = do
  p <- parseFile f

  return $ newState p

showHelp :: IO ()
showHelp = do
  putStrLn "Usage: ./Asc <file> <file> <file> ..."
  return ()

main :: IO ()
main = do
  args <- getArgs

  case args of
    []    -> showHelp
    files -> mapM_ (\f -> loadFile f >>= run) args

  return ()
