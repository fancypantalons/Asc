module Asc where

import Parser
import Array
import Data.List

data Slot = ISlot Integer | DSlot Double | Empty
          deriving Show

type RegisterSet = Array Integer Integer
type Memory = Array Integer Slot
type InstrList = Array Integer Instr

data State = State { 
               display :: RegisterSet,
               stack   :: Memory,
               program :: InstrList,
               pc      :: Integer,
               sp      :: Integer
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
