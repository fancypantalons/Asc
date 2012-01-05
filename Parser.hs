module Parser
(
  Label,
  Reg,
  Addr(Absolute, Named, Relative),
  Instr(
    PUSH, PUSHI, PUSHA,
    POP, POPI,
    CONSTI, CONSTR,
    DUP,
    ADJUST,
    ALLOC, FREE,
    ADDI, ADDR,
    SUBI, SUBR,
    MULI, MULR,
    DIVI, DIVR,
    MOD,
    ITOR, RTOI,
    EQI, EQR,
    LTI, LTR,
    GTI, GTR,
    OR, AND, NOT,
    IFZ, IFNZ, IFERR,
    GOTO,
    CALL, RET,
    READI, READR, READC,
    WRITEI, WRITER, WRITEC,
    STOP,
    LABEL
  ),
  Program,
  parse,
  parseFile,
  parseInt,
  parseDouble
)
where

import IO
import Control.Monad

import Data.Char
import Data.List

import Text.ParserCombinators.ReadP
import Text.Read.Lex as Lex

type Label = String

type Reg = Integer

data Addr = Absolute Integer
          | Named Label
          | Relative Reg Integer
          deriving Show

data Instr = PUSH Addr | PUSHI (Maybe Reg) | PUSHA Addr
           | POP Addr | POPI (Maybe Reg)
           | CONSTI Integer | CONSTR Double
           | DUP
           | ADJUST Integer
           | ALLOC Integer
           | FREE
           | ADDI | ADDR
           | SUBI | SUBR
           | MULI | MULR
           | DIVI | DIVR
           | MOD
           | ITOR | RTOI
           | EQI | EQR
           | LTI | LTR
           | GTI | GTR
           | OR | AND | NOT
           | IFZ Addr | IFNZ Addr | IFERR Addr
           | GOTO Addr
           | CALL Reg Addr | RET Reg
           | READI | READR | READC
           | WRITEI | WRITER | WRITEC
           | STOP
           | LABEL Label
           deriving Show

type Program = [([Label], Instr)]

--
-- Given a predicate and a list, take each element and turn it into a list,
-- and compress runs of elements matching the predicate into the same list.
--
collectPairs :: (a -> Bool) -> [a] -> [[a]]                                                                            
collectPairs p lst = doit lst                                                                                          
  where doit [] = []                                                                                                   
        doit (x:[]) = [[x]]                                                                                            
        doit (x:xs)                                                                                                    
          | (p a) && (p x)  = ((x:a:as):rs)                                                                            
          | otherwise       = ([x]:rest)                                                                               
          where rest@((a:as):rs) = doit xs                                                                             

--
-- Pretty simple, kill anything following a '#' character.
stripComment :: String -> String
stripComment = takeWhile (\c -> c /= '#')

--
-- Takes the result of the given ReadP and wraps it in a Just.  This is
-- useful when using option to return Nothing or the parse result.
-- 
maybeP :: ReadP a -> ReadP (Maybe a)
maybeP p = do
  v <- p
  return (Just v)

-- 
-- Read one or more spaces from input.
--
parseSpaces :: ReadP ()
parseSpaces = do
  many1 (satisfy isSpace)
  return ()

--
-- Read a series of digits from input.
--
parseDigits :: ReadP String
parseDigits = many1 $ satisfy isDigit

--
-- Read a string of digits, optionally preceeded by a unary minute sign.
-- Then uses read to convert to an Integer.
--
parseInt :: ReadP Integer
parseInt = do
  neg <- option Nothing (maybeP $ char '-')
  rest <- parseDigits

  let v = read rest

  case neg of
    Nothing -> return v
    Just _  -> return $ negate v

--
-- Read a string of the form:
--
-- [-]<digits>[e[-]<digits>]
--
-- And convert to a Double.
--
parseDouble :: ReadP Double
parseDouble = do
  neg <- option Nothing (maybeP $ char '-')
  integral <- parseDigits
  frac <- option (0 :: Double) parseFrac
  exp <- option (0 :: Double) parseExp

  let v = ((read integral :: Double) + frac) * (10 ** exp)

  case neg of
    Nothing -> return v
    Just _  -> return $ negate v

  where parseFrac = do
          char '.'
          v <- parseDigits

          return (read ('0':'.':v) :: Double)

        parseExp = do
          l <- look
          char 'E'
          neg <- option Nothing (maybeP $ char '-')
          v <- parseDigits

          case neg of
            Nothing -> return (read v :: Double)
            Just _  -> return $ negate (read v :: Double)

--
-- Reads a string of characters starting with an alpha character,
-- followed by one or more non-space characters.
--
parseSym :: ReadP String
parseSym = do
  c <- satisfy isAlpha
  rest <- many $ satisfy (not . isSpace)

  return (c:rest)

parseAddr :: ReadP Addr
parseAddr = absolute +++ named +++ relative
  where absolute   = do
          v <- parseInt

          return (Absolute v)

        named      = do
          s <- parseSym

          return (Named s)

        relative   = do
          a <- parseInt
          char '['
          r <- parseInt
          char ']'

          return (Relative r a)

parseNoArgs :: ReadP Instr
parseNoArgs = 
  ((string "DUP") >> return DUP) +++
  ((string "FREE") >> return FREE) +++
  ((string "ADDI") >> return ADDI) +++
  ((string "ADDR") >> return ADDR) +++
  ((string "SUBI") >> return SUBI) +++
  ((string "SUBR") >> return SUBR) +++
  ((string "MULI") >> return MULI) +++
  ((string "MULR") >> return MULR) +++
  ((string "DIVI") >> return DIVI) +++
  ((string "DIVR") >> return DIVR) +++
  ((string "MOD") >> return MOD) +++
  ((string "ITOR") >> return ITOR) +++
  ((string "RTOI") >> return RTOI) +++
  ((string "EQI") >> return EQI) +++
  ((string "EQR") >> return EQR) +++
  ((string "LTI") >> return LTI) +++
  ((string "LTR") >> return LTR) +++
  ((string "GTI") >> return GTI) +++
  ((string "GTR") >> return GTR) +++
  ((string "OR") >> return OR) +++
  ((string "AND") >> return AND) +++
  ((string "NOT") >> return NOT) +++
  ((string "READI") >> return READI) +++
  ((string "READR") >> return READR) +++
  ((string "READC") >> return READC) +++
  ((string "WRITEI") >> return WRITEI) +++
  ((string "WRITER") >> return WRITER) +++
  ((string "WRITEC") >> return WRITEC) +++
  ((string "STOP") >> return STOP)

parseNumArg :: ReadP Instr
parseNumArg =
  ((string "CONSTI") >> parseSpaces >> parseInt >>= constiop) +++
  ((string "CONSTR") >> parseSpaces >> parseDouble >>= constrop) +++
  ((string "ADJUST") >> parseSpaces >> parseInt >>= adjustop) +++
  ((string "ALLOC") >> parseSpaces >> parseInt >>= allocop) +++
  ((string "RET") >> parseSpaces >> parseInt >>= retop)

  where constiop v = return (CONSTI v)
        constrop v = return (CONSTR v)
        adjustop v = return (ADJUST v)
        allocop  v = return (ALLOC v)
        retop    v = return (RET v)

parseAddrArg :: ReadP Instr
parseAddrArg =
  ((string "PUSH") >> parseSpaces >> parseAddr >>= pushop) +++
  ((string "PUSHA") >> parseSpaces >> parseAddr >>= pushaop) +++
  ((string "POP") >> parseSpaces >> parseAddr >>= popop) +++
  ((string "IFZ") >> parseSpaces >> parseAddr >>= ifzop) +++
  ((string "IFNZ") >> parseSpaces >> parseAddr >>= ifnzop) +++
  ((string "IFERR") >> parseSpaces >> parseAddr >>= iferrop) +++
  ((string "GOTO") >> parseSpaces >> parseAddr >>= gotoop) 

  where pushop v = return (PUSH v)
        pushaop v = return (PUSHA v)
        popop v = return (POP v)
        ifzop v = return (IFZ v)
        ifnzop v = return (IFNZ v)
        iferrop v = return (IFERR v)
        gotoop v = return (GOTO v)

parseMaybeReg :: ReadP Instr
parseMaybeReg =
  ((string "PUSHI") >> (option Nothing (maybeP parseInt)) >>= pushiop) +++
  ((string "POPI") >> (option Nothing (maybeP parseInt)) >>= popiop)

  where pushiop v = return (PUSHI v)
        popiop v = return (POPI v)

parseCall :: ReadP Instr
parseCall = do
  string "CALL"

  parseSpaces
  r <- parseInt
  skipSpaces
  char ','
  skipSpaces
  a <- parseAddr

  return (CALL r a)

parseOp :: ReadP Instr
parseOp = (parseNoArgs +++ parseNumArg +++ parseAddrArg +++ parseMaybeReg +++ parseCall)

parseLabelAndOp :: ReadP [Instr]
parseLabelAndOp = do
  skipSpaces
  l <- parseSym
  parseSpaces
  i <- parseOp
  skipSpaces
  eof

  return [(LABEL l), i]

parseLabel :: ReadP [Instr]
parseLabel = do
  skipSpaces
  l <- parseSym
  skipSpaces
  eof

  return [(LABEL l)]

parseOnlyOp :: ReadP [Instr]
parseOnlyOp = do
  skipSpaces
  i <- parseOp
  skipSpaces
  eof

  return [i]

parseEmpty :: ReadP [Instr]
parseEmpty = do
  skipSpaces
  eof

  return []

parseLine :: ReadP [Instr]
parseLine = parseLabelAndOp +++ parseOnlyOp +++ parseLabel +++ parseEmpty

parseInstrs :: [String] -> [Instr]
parseInstrs lines = foldr (++) [] $ map doParse preprocessed
  where preprocessed = map ((map toUpper) . stripComment) lines
        doParse = fst . head . readP_to_S parseLine

--
-- Given an ASC program, expressed as a series of instructions, one per line,
-- parse the source into a logical model.  Each row in the resulting array
-- represents a pair containing an instruction and zero or more labels 
-- associated with that location in the source file.
--
parse :: [String] -> Program
parse lines = collapse $ collectPairs isLabel $ parseInstrs lines
  -- The collectPairs call above collapses series of labels together into
  -- a single array, and places the rest in their own arrays.  Collapse
  -- then pairs up the labels with the next instruction in the sequence.
  where collapse []                 = []
        collapse (((LABEL _):_):[]) = []
        
        collapse (a@((LABEL _):_):(b:[]):rest) = (labels a, b):(collapse rest)
        collapse ((a:[]):rest)                 = ([], a):(collapse rest)

        isLabel i = case i of
                      (LABEL _) -> True
                      _         -> False

        labels = map (\(LABEL l) -> l)

--
-- Parse the contents of the specified file.
--
parseFile :: String -> IO Program
parseFile s =  liftM (parse . lines) $ readFile s

