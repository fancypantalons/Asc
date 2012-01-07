module Asc where

import Parser
import Heap as Heap

import IO
import System.IO

import Text.ParserCombinators.ReadP
import Data.Array.IArray
import Data.Array.IO 
import Data.List
import Data.Char
import System (getArgs)

data Slot = ISlot Integer | DSlot Double | Empty
instance Show Slot where
  show (ISlot v) = show v
  show (DSlot v) = show v
  show Empty     = "_"

type Memory = IOArray Integer Slot
type RegisterSet = Array Integer Integer
type InstrList = Array Integer Instr

data State = State { 
               running :: ! Bool,
               pc      :: ! Integer,
               sp      :: ! Integer,
               tracing :: ! Bool,
               stack   :: ! Memory,
               display :: ! RegisterSet,
               program :: ! InstrList,
               heap    :: ! Heap.HeapInfo
             } 

instance Show State where
  show state = ""

--
-- Split an array into a set of subarrays containing n or less elements
-- each.
--
subgroups :: Int -> [a] -> [[a]]
subgroups n []  = []
subgroups n arr = group:(subgroups n rest)
  where group = take n arr
        rest  = drop n arr

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

newState :: Program -> IO State
newState p = do
  mem <- newListArray (0, 32767) [ Empty | i <- [0..32767] ]

  return $ State {
             display = array (0, 15) [ (i, 0) | i <- [0..15] ],
             stack   = mem,
             program = listArray (0, len - 1) listing,
             pc      = 0,
             sp      = 0,
             heap    = Heap.newHeap 32768,
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
translate state (Relative r i) = val + i
  where !val = regGet state r

stackGet :: State -> Integer -> IO Slot
stackGet state i = readArray (stack state) i >>= return

stackSet :: State -> Integer -> Slot -> IO ()
stackSet state i slot = writeArray (stack state) i slot

stackTop :: State -> IO Slot
stackTop state = stackGet state $ (sp state - 1)

stackTopI :: State -> IO Integer
stackTopI state = do 
  t <- stackTop state
  
  case t of
    (ISlot val) -> return val
    _           -> error "Expected integer but found something else"

regGet :: State -> Integer -> Integer
regGet state i = (display state) ! i

regSet :: State -> Integer -> Integer -> State
regSet state i val = state { display = (display state) // [(i, val)] }

push :: State -> Slot -> IO State
push state slot = stackSet state (sp state) slot >> return (state { sp = sp state + 1 })

pushi :: State -> Integer -> IO State
pushi state val = push state (ISlot val)

pushd :: State -> Double -> IO State
pushd state val = push state (DSlot val)

pop :: State -> IO (Slot, State)
pop state 
  | (sp state) == 0 = error "Tried to pop past top of stack"
  | otherwise       = do
    slot <- stackGet state (sp state - 1)

    return (slot, state { sp = sp state - 1 })

popi :: State -> IO (Integer, State)
popi state = do
  res <- pop state

  case res of
    ((ISlot val), state) -> return (val, state)
    _                    -> error "Expected integer slot but found something else"

popd :: State -> IO (Double, State)
popd state = do
  res <- pop state

  case res of
    ((DSlot val), state) -> return (val, state)
    _                    -> error "Expected double slot but found something else"

pop2 :: State -> IO (Slot, Slot, State)
pop2 state = do
  (a, s1) <- pop state 
  (b, s2) <- pop s1

  return (a, b, s2)

combine :: (Slot -> Slot -> Slot) -> State -> IO State
combine op state = do
  (a, b, newState) <- pop2 state
  
  (push newState $ op b a) >>= return

combinei :: (Integer -> Integer -> Integer) -> State -> IO State
combinei op state = combine (\(ISlot a) (ISlot b) -> ISlot (op a b)) state

combined :: (Double -> Double -> Double) -> State -> IO State
combined op state = combine (\(DSlot a) (DSlot b) -> DSlot (op a b)) state

combinedi :: (Double -> Double -> Integer) -> State -> IO State
combinedi op state = combine (\(DSlot a) (DSlot b) -> ISlot (op a b)) state

cmp :: (a -> a -> Bool) -> a -> a -> Integer
cmp pred a b = case (pred a b) of
                 True -> 1
                 False -> 0

--
-- Instruction execution logic.
--
execute :: State -> Instr -> IO State
execute state (PUSH a) = stackGet state (translate state a) >>= push state >>= return
execute state (PUSHI (Just r)) = do
  top <- stackTopI state

  stackGet state (top + rval) >>= push state >>= return

  where !rval = regGet state r

execute state (PUSHI Nothing) = do
  (top, newState) <- popi state

  stackGet newState top >>= push newState >>= return

execute state (PUSHA a) = pushi state (translate state a)

execute state (POP a) = do
  (top, newState) <- pop state

  stackSet newState (translate state a) top
  return newState

execute state (POPI (Just r)) = do
  let !rval = regGet state r
  
  (v, s) <- pop state
  ((ISlot a), s2) <- pop s

  stackSet s2 (a + rval) v
  return s2

execute state (POPI Nothing) = do
  (v, s) <- pop state
  ((ISlot a), s2) <- pop s

  stackSet s2 a v
  return s2

execute state (CONSTI v) = pushi state v
execute state (CONSTR v) = pushd state v

execute state DUP = stackTop state >>= push state >>= return
execute state (ADJUST v) = return $ state { sp = sp state + v }

execute state (ALLOC i) = pushi newState addr
  where (newHeap, addr) = Heap.allocate (heap state) i
        newState = state { heap = newHeap }

execute state FREE = do
  (addr, s1) <- popi state

  return s1 { heap = Heap.free (heap s1) addr }

execute state ADDI = combinei (+) state
execute state ADDR = combined (+) state
execute state SUBI = combinei (-) state
execute state SUBR = combined (-) state
execute state MULI = combinei (*) state
execute state MULR = combined (*) state
execute state DIVI = combinei (div) state
execute state DIVR = combined (/) state
execute state MOD = combinei (mod) state

execute state ITOR = do
  (val, newState) <- popi state

  pushd newState (fromIntegral val :: Double) >>= return

execute state RTOI = do
  (val, newState) <- popd state

  pushi newState (round val) >>= return

execute state EQI = combinei (cmp (==)) state
execute state EQR = combinedi (cmp (==)) state
execute state LTI = combinei (cmp (<)) state
execute state LTR = combinedi (cmp (<)) state
execute state GTI = combinei (cmp (>)) state
execute state GTR = combinedi (cmp (>)) state

execute state OR = combinei (cmp (\a b -> (a /= 0) || (b /= 0))) state
execute state AND = combinei (cmp (\a b -> (a /= 0) && (b /= 0))) state
execute state NOT = do
  (val, newState) <- popi state

  pushi newState (result val) >>= return

  where result 1 = 0
        result _ = 1

execute state (IFZ a) = do
  (val, newState) <- popi state

  return (newState { pc = addr val })

  where addr 0 = translate state a
        addr _ = pc state

execute state (IFNZ a) = do
  (val, newState) <- popi state

  return (newState { pc = addr val })

  where addr 0 = pc state 
        addr _ = translate state a

execute state (GOTO a) = return $ state { pc = translate state a }

execute state (CALL r a) = do
  let !rval = regGet state r

  s1 <- pushi state $ pc state
  s2 <- pushi s1 rval

  let s3 = s2 { pc = translate s2 a }
  
  return $ regSet s3 r (sp s3)

execute state (RET r) = do
  (oldreg, s1) <- popi state
  (oldpc, s2) <- popi s1

  let s3 = regSet s2 r oldreg

  return $ s3 { pc = oldpc }

execute state READI = do
  l <- getLine
  
  case (readP_to_S pi l) of
    []       -> error "That integer was no good anyway"
    (v, _):_ -> pushi state v >>= return

  where pi = do
          skipSpaces
          v <- parseInt
          skipSpaces
          eof

          return v

execute state READR = do
  l <- getLine

  case (readP_to_S pd l) of
    []       -> error "Double bad"
    (v, _):_ -> pushd state v >>= return

  where pd = do
          skipSpaces
          v <- parseDouble
          skipSpaces
          eof

          return v

execute state READC = do 
  c <- getChar

  pushi state (fromIntegral $ ord c) >>= return

execute state WRITEI = do
  (val, s2) <- popi state

  putStr $ show val
  return s2

execute state WRITER = do
  (val, s2) <- popd state

  putStr $ show val
  return s2

execute state WRITEC = do
  (val, s2) <- popi state

  putChar $ chr $ fromIntegral val
  return s2

execute state STOP = return $ state { running = False }
execute state CORE = do
  putStrLn $ show state
  return state

execute state (TRACE Nothing) = do
  (flg, newState) <- popi state

  let val = if (flg == 0)
              then False
              else True

  return $ newState { tracing = val }

execute state (TRACE (Just v)) = return $ state { tracing = val }
  where val = if (v == 0) 
                then False
                else True

execute state _ = return state

trace :: State -> Instr -> Bool -> IO ()
trace _ _ False        = return()
trace state instr True = do
  els <- getElems $ stack state

  let st = map show $ take (fromIntegral $ sp state) els

  putStrLn $ "----"
  putStrLn $ "Regs\t: [" ++ (intercalate ", " $ map show $ elems $ display state) ++ "]"
  mapM_ putStrLn $ map (\g -> "Stk\t: [" ++ intercalate ", " g ++ "]") $ subgroups 10 st
  putStrLn $ "Ctrl\t: " ++ intercalate "\t" [ 
      "PC: " ++ (show $ pc state),
      "SP: " ++ (show $ sp state)
    ]
  putStrLn $ "Heap\t: " ++ (show $ heap state)
  putStrLn $ "Inst\t: " ++ (show instr) 

runOnce :: State -> IO State
runOnce state = do
  trace state instr (tracing state)
  execute s2 instr >>= return

  where !instr = program state ! pc state
        s2    = state { pc = pc state + 1 }

run :: State -> IO State
run state = do
  s2 <- runOnce state

  if (running s2)
    then run s2 >>= return
    else return s2

load :: [String] -> IO State
load listing = newState $ parse listing

loadFile :: String -> IO State
loadFile f = do
  p <- parseFile f

  newState p >>= return

showHelp :: IO ()
showHelp = do
  putStrLn "Usage: ./Asc <file> <file> <file> ..."
  return ()

main :: IO ()
main = do
  args <- getArgs

  hSetBinaryMode stdout True

  case args of
    []    -> showHelp
    files -> mapM_ (\f -> loadFile f >>= run) args

  return ()
