import Data.List
import Data.Char
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data EvaluationData = Boolean Bool | Int Integer
  deriving Show

type Stack = [EvaluationData]

type StateData = (String, EvaluationData)

type State = [StateData]

createEmptyStack :: Stack
createEmptyStack = []

evaluation2Str :: EvaluationData -> String
evaluation2Str (Int a) = show a
evaluation2Str (Boolean a) = show a


stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = evaluation2Str x
stack2Str (x:xs) = evaluation2Str x ++ "," ++ stack2Str xs

createEmptyState :: State
createEmptyState = []

stateData2Str :: StateData -> String
stateData2Str (key,value) = key ++ "=" ++ evaluation2Str value

stateSort :: StateData -> StateData -> Ordering
stateSort (keyA, _) (keyB, _) = compare keyA keyB 

state2Str :: State -> String
state2Str state =  intercalate "," (map (\x -> stateData2Str x) (sortBy stateSort state))

-- interpreter
runInst :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- add
runInst Add (code, (Int a: Int b: xs), state) = (code, (Int (a+b):xs), state)
runInst Add (code, stack, state) = error $ "Run-time error"

-- sub
runInst Sub (code, (Int a: Int b: xs), state) = (code, (Int (a-b):xs), state)
runInst Sub (code, stack, state) = error $ "Run-time error"

-- mult
runInst Mult (code, (Int a: Int b: xs), state) = (code, (Int (a*b):xs), state)
runInst Mult (code, stack, state) = error $ "Run-time error"

-- push integer, false, true
runInst (Push i) (code, stack, state) = (code, (Int i : stack), state)
runInst Fals (code, stack, state) = (code, (Boolean False: stack), state)
runInst Tru (code, stack, state) = (code, (Boolean True: stack), state)

-- equal op
runInst Equ (code, (Int a: Int b: xs), state)
  | a == b = (code, (Boolean True :xs), state)
  | a /= b = (code, (Boolean False :xs), state)

runInst Equ (code, (Boolean a: Boolean b: xs), state)
  | a == b = (code, (Boolean True :xs), state)
  | a /= b = (code, (Boolean False :xs), state)

runInst Equ (code, stack, state) = error $ "Run-time error"

-- neg op
runInst Neg (code, (Boolean a : xs), state) = (code, (Boolean (not a): xs), state)
runInst Neg (code, stack, state) =  error $ "Run-time error"

-- le op (int only)
runInst Le (code, (Int a: Int b: xs), state)
  | a <= b = (code, (Boolean True :xs), state)
  | a > b = (code, (Boolean False :xs), state)

runInst Le (code, stack, state) =  error $ "Run-time error"


-- and op
runInst And (code, (Boolean a: Boolean b: xs), state) = (code, (Boolean (a && b): xs), state)
runInst And (code, stack, state) =  error $ "Run-time error"

-- noop
runInst Noop vm = vm

-- store op
runInst (Store key) (code, (x:xs), state) = case (filter (\(skey, _) -> skey == key) state) of [] -> (code, xs, state ++ [(key, x)])
                                                                                               (l: _) -> (code, xs, (filter (\(skey, _) -> skey /= key) state) ++ [(key, x)])

-- fetch op
runInst (Fetch key) (code, stack, state) = case (filter (\(skey, _) -> skey == key) state) of [] -> error $ "Run-time error"
                                                                                              ((_, value):_) -> (code, [value] ++ stack,state)
-- branch op
runInst (Branch c1 c2) (code, (Boolean a: xs), state)
  | a == True = (c1 ++ code, xs, state)
  | a == False = (c2 ++ code, xs, state)

runInst (Branch c1 c2) (code, stack, state) = ([], stack, state) -- according to the spec, it should halt instead of throwing a error?

-- loop op
runInst (Loop c1 c2) (code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) --when there's no code left leave
run ((x:xs), stack, state) = run(runInst x (xs, stack, state))

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Add] == ("14","")
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Push 10,Push 4,Equ] == ("False","")
-- testAssembler [Push 10,Push 10,Equ] == ("True","")
-- testAssembler [Tru, Tru, Equ] == ("True", "")
-- testAssembler [Fals, Tru, Equ] == ("False", "")
-- testAssembler [Fals, Neg] == ("True", "")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
-- testAssembler [Tru, Tru, Add] -> ERROR
-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp = Sum Stm Stm | Subs Stm Stm| Multi Stm Stm | IntLit Integer | Var String -- the variable has no type
  deriving Show

data Bexp = Lte Stm Stm | EqualsInt Stm Stm | Not Stm | EqualsBool Stm Stm | Bool Bool | VarB String
  deriving Show

data Stm = Aex Aexp | Bexp Bexp
  deriving Show

type Program = [Stm]

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO


--- Tokenizer section
data Token = Punctuation Char | Number Integer | Identifier String | Operator String | Keyword String | Assignment | TBool Bool
  deriving Show


tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (x:xs)
  | x `elem` "+-*" = [Operator [x]] ++ tokenizer xs
  | x `elem` "();" = [Punctuation x] ++ tokenizer xs
  | x `elem` " " = tokenizer xs
  | isDigit x = let (num, rest) = span isDigit (x:xs)
                in [Number (read num :: Integer)] ++ tokenizer rest
  | isAlpha x = case (takeWhile (\x -> not (x `elem` " +-*();")) (x:xs)) of "while" -> [Keyword "while"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "if" -> [Keyword "if"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "else" -> [Keyword "else"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "not" -> [Keyword "not"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "and" -> [Operator "and"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "True" -> [TBool True] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            "False" -> [TBool False] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
                                                                            otherwise -> [Identifier otherwise] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=")) (x:xs))
  | x `elem` ":" = let (next:rest) = xs
                   in case next of '=' -> [Assignment] ++ tokenizer rest
                                   otherwise -> tokenizer (next:rest)
  | x `elem` "=" = let (next:rest) = xs
                   in case next of '=' -> [Operator "=="] ++ tokenizer rest
                                   otherwise -> [Operator "="] ++ tokenizer (next:rest)
  | x `elem` "<" = let (next:rest) = xs
                   in case next of '=' -> [Operator "<="] ++ tokenizer rest
                                   otherwise -> tokenizer (next:rest)
  | otherwise = error $ ("Could not make a lexical analysis of this input" ++ show x)


-- Parsing section

-- Aexp
parseIntOrVarOrParen :: [Token] -> Maybe (Stm, [Token])
parseIntOrVarOrParen (Number a: restTokens) = Just (Aex (IntLit a), restTokens)
parseIntOrVarOrParen (Identifier a: restTokens) = Just (Aex (Var a), restTokens)
parseIntOrVarOrParen (Punctuation '(': restTokens)
  = case (parseAexp restTokens) of 
      Just (expr, (Punctuation ')': restTokens2)) -> Just (expr, restTokens2)
      Nothing -> Nothing 
parseIntOrVarOrParen tokens = Nothing

parseProdOrRest :: [Token] -> Maybe (Stm, [Token])
parseProdOrRest tokens =
  case (parseIntOrVarOrParen tokens) of
    Just (expr1, (Operator "*": restTokens1)) -> 
      case (parseIntOrVarOrParen restTokens1) of
        Just (expr2, restTokens2) -> Just (Aex (Multi expr1 expr2), restTokens2)
        Nothing -> Nothing
    Just (expr1, restTokens1) -> Just (expr1, restTokens1) 
    Nothing -> Nothing

parseSumOrSubOrRest :: [Token] -> Maybe (Stm, [Token])
parseSumOrSubOrRest tokens = 
  case (parseProdOrRest tokens) of
    Just (expr1, (Operator "+": restTokens1)) -> 
      case (parseSumOrSubOrRest restTokens1) of
        Just (expr2, restTokens2) -> Just (Aex (Sum expr1 expr2), restTokens2)
        Nothing -> Nothing
  
    Just (expr1, (Operator "-": restTokens1)) -> 
      case (parseSumOrSubOrRest restTokens1) of
        Just (expr2, restTokens2) -> Just (Aex (Subs expr1 expr2), restTokens2)
        Nothing -> Nothing
  
    Just (expr1, restTokens1) -> Just (expr1, restTokens1)
    Nothing -> Nothing

parseAexp :: [Token] -> Maybe (Stm, [Token])
parseAexp tokens = parseSumOrSubOrRest tokens

-- Bexp

parseTvalOrVar :: [Token] -> Maybe (Stm, [Token])
parseTvalOrVar (TBool a: restTokens) = Just(Bexp (Bool a), restTokens)
parseTvalOrVar (Identifier a: restTokens) = Just(Bexp (VarB a), restTokens)
parseTvalOrVar tokens = Nothing


parseLteOrAexp :: [Token] -> Maybe (Stm, [Token])
parseLteOrAexp tokens =
  case (parseAexp tokens) of
    Just(exp1, (Operator "<=" : restTokens1)) ->
      case (parseAexp restTokens1) of
        Just(exp2, restTokens2) -> Just(Bexp (Lte exp1 exp2), restTokens2)
        Nothing -> Nothing
    otherwise -> otherwise

parseEqIntOrAexp :: [Token] -> Maybe (Stm, [Token])
parseEqIntOrAexp tokens = 
  case (parseAexp tokens) of
    Just(exp1, (Operator "==" : restTokens1)) ->
      case (parseAexp restTokens1) of
        Just(exp2, restTokens2) -> Just(Bexp (EqualsInt exp1 exp2), restTokens2)
        Nothing -> Nothing
    otherwise -> otherwise

parseAexpDerivedBexp :: [Token] -> Maybe (Stm, [Token])
parseAexpDerivedBexp tokens = 
  case (parseEqIntOrAexp tokens) of
    Just(Bexp a, restTokens) -> Just(Bexp a, restTokens)
    otherwise ->
      case (parseLteOrAexp tokens) of
        Just(Bexp a, restTokens) -> Just(Bexp a, restTokens)
        otherwise ->
          case (parseTvalOrVar tokens) of
            Just (exp, restTokens) -> Just(exp, restTokens)
            Nothing -> Nothing



parseBexp :: [Token] -> Maybe (Stm, [Token])
parseBexp tokens = undefined

-- general

parseAexpOrBexp :: [Token] -> Maybe (Stm, [Token])
parseAexpOrBexp tokens = 
  case (parseAexp tokens) of
    Just(expr, restTokens) -> Just (expr, restTokens)
    Nothing -> case (parseBexp tokens) of
      Just(expr, restTokens) -> Just (expr, restTokens)
      Nothing -> Nothing
        


-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String) 
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
