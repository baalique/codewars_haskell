-- https://www.codewars.com/kata/58e61f3d8ff24f774400002c

module AssemblerInterpreter where

import           Control.Monad                  ( (<=<) )
import           Data.Char                      ( isNumber )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Data.Map                       ( (!) )
import           Data.Ord                       ( Ordering(..) )

data Program a = Program
    { getSubroutines :: Subroutines
    , getState       :: a
    }
type Register = String
type Registers = M.Map Register Int
type Output = String
type CompareResult = Maybe Ordering
data ProgramState = ProgramState
    { getRegisters     :: Registers
    , getOutput        :: Output
    , getCompareResult :: CompareResult
    }
type Subroutines = M.Map SubroutineName [Instruction]
data Var = VarNumber Int | VarRegister Register deriving (Eq)
type SubroutineName = String
data StringOrVar = SOVString String | SOVVar Var deriving (Eq)
data InstructionStack = InstructionStack
    { getCurrentInstructionStack :: [Instruction]
    , getFutureInstructionStack  :: [Instruction]
    }
data Instruction = Mov Register Var
                 | Inc Register
                 | Dec Register
                 | Add Register Var
                 | Sub Register Var
                 | Mul Register Var
                 | Div Register Var
                 | Label SubroutineName
                 | Jmp SubroutineName
                 | Cmp Var Var
                 | Jne SubroutineName
                 | Je SubroutineName
                 | Jge SubroutineName
                 | Jg SubroutineName
                 | Jle SubroutineName
                 | Jl SubroutineName
                 | Call SubroutineName
                 | Ret
                 | Msg [StringOrVar]
                 | End
                 deriving (Eq)

instance Functor Program where
    fmap f (Program subroutines is) = Program subroutines (f is)

instance Semigroup ProgramState where
    ps1 <> ps2 = ProgramState regs out cmpr
      where
        regs = getRegisters ps1 <> getRegisters ps2
        out  = getOutput ps1 <> getOutput ps2
        cmpr = getCompareResult ps1 <> getCompareResult ps2

instance Monoid ProgramState where
    mempty = ProgramState M.empty "" Nothing

interpret :: String -> Maybe Output
interpret xs = fmap (getOutput . getState) executed
  where
    executed     = execSubroutine (Program subroutines mempty) (InstructionStack instructions [])
    instructions = subroutines ! mainSubroutineName
    subroutines  = generateSubroutines . map getRow . prepareInput $ xs

execSubroutine :: Program ProgramState -> InstructionStack -> Maybe (Program ProgramState)
execSubroutine p (   InstructionStack []              _) = Nothing
execSubroutine p is@(InstructionStack ((Mov x y) : _) _) = execSubroutine (execMov (varToValue y p) x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Inc x  ) : _) _) = execSubroutine (execInc x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Dec x  ) : _) _) = execSubroutine (execDec x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Add x y) : _) _) = execSubroutine (execAdd (varToValue y p) x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Sub x y) : _) _) = execSubroutine (execSub (varToValue y p) x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Mul x y) : _) _) = execSubroutine (execMul (varToValue y p) x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Div x y) : _) _) = execSubroutine (execDiv (varToValue y p) x p) (cutHead is)
execSubroutine p is@(InstructionStack ((Label x) : _) _) = execSubroutine p (cutHead is)
execSubroutine p is@(InstructionStack ((Jmp   x) : _) _) = execSubroutine p (updateInstructions (Jmp x) p is)
execSubroutine p is@(InstructionStack ((Cmp x y) : _) _) = execSubroutine (execCmp x y p) (cutHead is)
execSubroutine p is@(InstructionStack ((Jne  x ) : _) _) = execSubroutine p (updateInstructions (Jne x) p is)
execSubroutine p is@(InstructionStack ((Je   x ) : _) _) = execSubroutine p (updateInstructions (Je x) p is)
execSubroutine p is@(InstructionStack ((Jge  x ) : _) _) = execSubroutine p (updateInstructions (Jge x) p is)
execSubroutine p is@(InstructionStack ((Jg   x ) : _) _) = execSubroutine p (updateInstructions (Jg x) p is)
execSubroutine p is@(InstructionStack ((Jle  x ) : _) _) = execSubroutine p (updateInstructions (Jle x) p is)
execSubroutine p is@(InstructionStack ((Jl   x ) : _) _) = execSubroutine p (updateInstructions (Jl x) p is)
execSubroutine p is@(InstructionStack ((Call x ) : _) _) = execSubroutine p (updateInstructions (Call x) p is)
execSubroutine p is@(InstructionStack (Ret       : _) _) = execSubroutine p (updateInstructions Ret p is)
execSubroutine p is@(InstructionStack ((Msg x)   : _) _) = execSubroutine (execMsg x p) (cutHead is)
execSubroutine p is@(InstructionStack (End       : _) _) = Just p

cutHead :: InstructionStack -> InstructionStack
cutHead is = is { getCurrentInstructionStack = tail $ getCurrentInstructionStack is }

execRegisterF :: (Int -> Int) -> Register -> Program ProgramState -> Program ProgramState
execRegisterF f reg = fmap $ \p -> updateRegisters p (M.update (Just . f) reg $ getRegisters p)

execCompareF :: (CompareResult -> CompareResult) -> Program ProgramState -> Program ProgramState
execCompareF f = fmap $ \p -> updateCompareResult p (f $ getCompareResult p)

execMov :: Int -> Register -> Program ProgramState -> Program ProgramState
execMov val reg = fmap $ \p -> updateRegisters p (M.insertWith const reg val $ getRegisters p)

execInc :: Register -> Program ProgramState -> Program ProgramState
execInc = execRegisterF (+ 1)

execDec :: Register -> Program ProgramState -> Program ProgramState
execDec = execRegisterF (subtract 1)

execAdd :: Int -> Register -> Program ProgramState -> Program ProgramState
execAdd val = execRegisterF (+ val)

execSub :: Int -> Register -> Program ProgramState -> Program ProgramState
execSub val = execRegisterF (subtract val)

execMul :: Int -> Register -> Program ProgramState -> Program ProgramState
execMul val = execRegisterF (* val)

execDiv :: Int -> Register -> Program ProgramState -> Program ProgramState
execDiv val = execRegisterF (`div` val)

execCmp :: Var -> Var -> Program ProgramState -> Program ProgramState
execCmp x y p = execCompareF (Just . \_ -> compare (varToValue x p) (varToValue y p)) p

execMsg :: [StringOrVar] -> Program ProgramState -> Program ProgramState
execMsg x p@(Program subrs ps) = Program subrs $ updateOutput ps (getOutput ps ++ parseMsg x p)

parseMsg :: [StringOrVar] -> Program ProgramState -> Output
parseMsg [] p = ""
parseMsg ((SOVVar (VarNumber x)) : xs) p = show x ++ parseMsg xs p
parseMsg ((SOVVar (VarRegister x)) : xs) p = show (registerToValue p x) ++ parseMsg xs p
parseMsg ((SOVString x) : xs) p = x ++ parseMsg xs p

updateInstructions :: Instruction -> Program ProgramState -> InstructionStack -> InstructionStack
updateInstructions (Jmp  lbl) = \p is -> is { getCurrentInstructionStack = getSubroutineFromState p lbl }
updateInstructions (Jne  lbl) = updateCompareInstructions [LT, GT] lbl
updateInstructions (Je   lbl) = updateCompareInstructions [EQ] lbl
updateInstructions (Jge  lbl) = updateCompareInstructions [EQ, GT] lbl
updateInstructions (Jg   lbl) = updateCompareInstructions [GT] lbl
updateInstructions (Jle  lbl) = updateCompareInstructions [LT, EQ] lbl
updateInstructions (Jl   lbl) = updateCompareInstructions [LT] lbl
updateInstructions (Call lbl) = updateCallInstructions lbl
updateInstructions Ret        = updateReturnInstructions
updateInstructions _          = undefined

updateCompareInstructions :: [Ordering]
                          -> SubroutineName
                          -> Program ProgramState
                          -> InstructionStack
                          -> InstructionStack
updateCompareInstructions cmprList lbl p is = case (getCompareResult . getState) p of
    Nothing   -> cutHead is
    Just cmpr -> if cmpr `elem` cmprList then replacedInstructions else cutHead is
        where replacedInstructions = is { getCurrentInstructionStack = getSubroutineFromState p lbl }

updateCallInstructions :: SubroutineName -> Program ProgramState -> InstructionStack -> InstructionStack
updateCallInstructions lbl p (InstructionStack is fis) =
    InstructionStack (getSubroutineFromState p lbl) (tail $ is ++ fis)

updateReturnInstructions :: a -> InstructionStack -> InstructionStack
updateReturnInstructions _ (InstructionStack cur fut) = InstructionStack (fut ++ fut) []

updateRegisters :: ProgramState -> Registers -> ProgramState
updateRegisters ps regs = ps { getRegisters = regs }

updateOutput :: ProgramState -> Output -> ProgramState
updateOutput ps out = ps { getOutput = out }

updateCompareResult :: ProgramState -> CompareResult -> ProgramState
updateCompareResult ps cmpr = ps { getCompareResult = cmpr }

varToValue :: Var -> Program ProgramState -> Int
varToValue (VarNumber   x) _ = x
varToValue (VarRegister x) p = registerToValue p x

registerToValue :: Program ProgramState -> Register -> Int
registerToValue (Program _ ps) x = getRegisters ps ! x

getSubroutineFromState :: Program ProgramState -> SubroutineName -> [Instruction]
getSubroutineFromState (Program subrs _) lbl = if M.member lbl subrs then subrs ! lbl else []

generateSubroutines :: [Instruction] -> Subroutines
generateSubroutines = getAllSubroutines (M.singleton mainSubroutineName []) []

getAllSubroutines :: Subroutines -> [Instruction] -> [Instruction] -> Subroutines
getAllSubroutines acc []          []                   = reverseMainSubroutine acc
getAllSubroutines acc cur         []                   = insertSubroutine (reverseMainSubroutine acc) cur
getAllSubroutines acc []          (lbl@(Label _) : xs) = getAllSubroutines acc [lbl] xs
getAllSubroutines acc srs@(_ : _) (Ret           : xs) = getAllSubroutines (insertSubroutine acc (Ret : srs)) [] xs
getAllSubroutines acc srs@(_ : _) (lbl@(Label _) : xs) = getAllSubroutines (insertSubroutine acc srs) [lbl] xs
getAllSubroutines acc srs@(_ : _) (x             : xs) = getAllSubroutines acc (x : srs) xs
getAllSubroutines acc []          (x             : xs) = getAllSubroutines (insertToMainSubroutine acc [x]) [] xs

insertSubroutine :: Subroutines -> [Instruction] -> Subroutines
insertSubroutine acc cur = case parseLabel $ last cur of
    Just lbl -> M.insert lbl (reverse $ init cur) acc
    Nothing  -> acc

insertToMainSubroutine :: Subroutines -> [Instruction] -> Subroutines
insertToMainSubroutine acc x = M.insertWith (++) mainSubroutineName x acc

reverseMainSubroutine :: Subroutines -> Subroutines
reverseMainSubroutine = M.update (Just . reverse) mainSubroutineName

parseLabel :: Instruction -> Maybe SubroutineName
parseLabel (Label x) = Just x
parseLabel _         = Nothing

parseVar :: String -> Var
parseVar var = if all isNumber var then VarNumber $ read var else VarRegister var

parseStringOrVar :: String -> StringOrVar
parseStringOrVar var =
    if head var == '\'' && last var == '\'' then SOVString $ init $ tail var else SOVVar $ VarRegister var

getRow :: [String] -> Instruction
getRow ["mov", x, y]           = Mov x (parseVar y)
getRow ["inc", x]              = Inc x
getRow ["dec", x]              = Dec x
getRow ["add", x, y]           = Add x (parseVar y)
getRow ["sub", x, y]           = Sub x (parseVar y)
getRow ["mul", x, y]           = Mul x (parseVar y)
getRow ["div", x, y]           = Div x (parseVar y)
getRow [lbl] | last lbl == ':' = Label $ init lbl
getRow ["jmp", x]              = Jmp x
getRow ["cmp", x, y]           = Cmp (parseVar x) (parseVar y)
getRow ["jne" , x]             = Jne x
getRow ["je"  , x]             = Je x
getRow ["jge" , x]             = Jge x
getRow ["jg"  , x]             = Jg x
getRow ["jle" , x]             = Jle x
getRow ["jl"  , x]             = Jl x
getRow ["call", x]             = Call x
getRow ["ret"       ]          = Ret
getRow ("msg" : rest)          = Msg $ map parseStringOrVar rest
getRow ["end"       ]          = End
getRow _                       = undefined

prepareInput :: String -> [[String]]
prepareInput = filter (not . null) . map clearInput . splitOn "\n"

clearInput :: String -> [String]
clearInput = splitBySpaces <=< (clearSpecials . splitQuotes . clearComments)

splitBySpaces :: String -> [String]
splitBySpaces xs = if isQuoted xs then [xs] else filter (not . null) . words $ xs

clearSpecials :: [String] -> [String]
clearSpecials = map (\x -> (if isQuoted x then id else filter (`notElem` specials)) x)

isQuoted :: String -> Bool
isQuoted = (== '\'') . head

splitQuotes :: String -> [String]
splitQuotes = reverse . map reverse . splitQuotes' [] [] []

splitQuotes' :: [String] -> String -> String -> String -> [String]
splitQuotes' acc []       []          []          = acc
splitQuotes' acc []       quotesStack []          = quotesStack : acc
splitQuotes' acc symStack []          []          = symStack : acc
splitQuotes' acc symStack []          ('\'' : xs) = splitQuotes' (symStack : acc) [] ['\''] xs
splitQuotes' acc []       quotesStack ('\'' : xs) = splitQuotes' (('\'' : quotesStack) : acc) [] [] xs
splitQuotes' acc symStack []          (x    : xs) = splitQuotes' acc (x : symStack) [] xs
splitQuotes' acc []       quoteStack  (x    : xs) = splitQuotes' acc [] (x : quoteStack) xs
splitQuotes' _   _        _           _           = undefined

clearComments :: String -> String
clearComments = takeWhile (/= ';')

mainSubroutineName :: SubroutineName
mainSubroutineName = "main"

specials :: String
specials = "`~!@#$%^&*()-=+[{]};\'\"\\|,<.>/?"
