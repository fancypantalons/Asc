{-# LANGUAGE TypeSynonymInstances #-}

module State
(
  Slot (ISlot, DSlot, Empty),
  Memory,
  RegisterSet,
  InstrList,
  State (running, pc, sp, tracing, display, stack, program, heap),
  newState,
  regGet,
  regSet,
  stackGet,
  stackSet,
  stackElems,
  stackTop,
  stackTopI,
  push,
  pushi,
  pushd,
  pop,
  popi,
  popd,
  combine,
  combinei,
  combined,
  combinedi,
  dump
)
where

import System.IO.Unsafe

import Parser
import Heap as Heap

import Data.Array.IArray
import Data.Array.IO
import Data.List

data Slot = ISlot Integer | DSlot Double | Empty
instance Show Slot where
  show (ISlot v) = show v
  show (DSlot v) = show v
  show Empty     = "_"

type Memory = IOArray Integer Slot
instance Show Memory where
  show mem = show $ unsafePerformIO $ getElems mem

type RegisterSet = Array Integer Integer
type InstrList = Array Integer Instr

data State = State { 
               running :: ! Bool,
               pc      :: ! Integer,
               sp      :: ! Integer,
               tracing :: ! Bool,
               display :: ! RegisterSet,
               stack   :: ! Memory,
               program :: ! InstrList,
               heap    :: ! Heap.HeapInfo
             } deriving Show

newState :: [Instr] -> IO State
newState listing = do
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
  where len     = fromIntegral $ length listing

regGet :: State -> Integer -> Integer
regGet state i = (display state) ! i

regSet :: State -> Integer -> Integer -> State
regSet state i val = state { display = (display state) // [(i, val)] }

stackGet :: State -> Integer -> Slot
stackGet state i = unsafePerformIO $ readArray (stack state) i

stackSet :: State -> Integer -> Slot -> State
stackSet state i slot = unsafePerformIO $ writeArray (stack state) i slot >> return state

stackElems :: State -> [Slot]
stackElems state = unsafePerformIO $ getElems (stack state) 

stackTop :: State -> Slot
stackTop state = stackGet state $ (sp state - 1)

stackTopI :: State -> Integer
stackTopI state = unwrap $ stackTop state
  where unwrap (ISlot val) = val
        unwrap _           = error "Expected integer but found something else"

push :: State -> Slot -> State
push state slot = newState { sp = sp newState + 1 }
  where newState = stackSet state (sp state) slot

pushi :: State -> Integer -> State
pushi state val = push state (ISlot val)

pushd :: State -> Double -> State
pushd state val = push state (DSlot val)

pop :: State -> (Slot, State)
pop state 
  | (sp state) == 0 = error "Tried to pop past top of stack"
  | otherwise       = (slot, newState)
  where newState = state { sp = sp state - 1 }
        !slot     = stackGet newState (sp newState)

popi :: State -> (Integer, State)
popi state = unwrap $ pop state
  where unwrap ((ISlot val), state) = (val, state)
        unwrap _                    = error "Expected integer slot but found something else"

popd :: State -> (Double, State)
popd state = unwrap $ pop state
  where unwrap ((DSlot val), state) = (val, state)
        unwrap _                    = error "Expected double slot but found something else"

combine :: (Slot -> Slot -> Slot) -> State -> State
combine op state = push newState result
  where (a, s1)       = pop state 
        (b, newState) = pop s1
        result        = op b a

combinei :: (Integer -> Integer -> Integer) -> State -> State
combinei op state = combine (\(ISlot a) (ISlot b) -> ISlot (op a b)) state

combined :: (Double -> Double -> Double) -> State -> State
combined op state = combine (\(DSlot a) (DSlot b) -> DSlot (op a b)) state

combinedi :: (Double -> Double -> Integer) -> State -> State
combinedi op state = combine (\(DSlot a) (DSlot b) -> ISlot (op a b)) state

--
-- Some logic to dump a nice trace of the given state instance.
--
subgroups :: Int -> [a] -> [[a]]
subgroups n []  = []
subgroups n arr = group:(subgroups n rest)
  where group = take n arr
        rest  = drop n arr

dump :: State -> IO ()
dump state = do
  let st = map show $ take (fromIntegral $ sp state) (stackElems state)

  putStrLn $ "----"
  putStrLn $ "Regs\t: [" ++ (intercalate ", " $ map show $ elems $ display state) ++ "]"
  mapM_ putStrLn $ map (\g -> "Stk\t: [" ++ intercalate ", " g ++ "]") $ subgroups 10 st
  putStrLn $ "Ctrl\t: " ++ intercalate "\t" [ 
      "PC: " ++ (show $ pc state),
      "SP: " ++ (show $ sp state)
    ]
  putStrLn $ "Heap\t: " ++ (show $ heap state)
  putStrLn $ "Inst\t: " ++ (show instr) 

  where instr = program state ! pc state
