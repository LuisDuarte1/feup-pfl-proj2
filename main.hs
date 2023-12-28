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
data Aexp = Sum Stm Stm | Subs Stm Stm| Multi Stm Stm | IntLit Integer | Var String
  deriving Show

data Bexp = Lte Stm Stm | EqualsInt Stm Stm | Not Stm | EqualsBool Stm Stm | AndBool Stm Stm | Bool Bool | VarB String
  deriving Show

-- If: consists of the Bexp, the list of statements if it's true and finally the list of statements if it's false
-- While: consists of the Bexp, and the body
data Stm = Aex Aexp | Bexp Bexp | Assignment String Stm | If Stm [Stm] [Stm] | While Stm [Stm]
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (IntLit a) = [Push a]
compA (Var a) = [Fetch a]

compA (Multi (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Mult]
compA (Multi a b) = error $ "Supplied a non Aexp statement into a multiplication operation"

compA (Subs (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Sub]
compA (Subs a b) = error $ "Supplied a non Aexp statement into a subtraction operation"

compA (Sum (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Add]
compA (Sum a b) = error $ "Supplied a non Aexp statement into a addition operation"


compB :: Bexp -> Code
compB (Lte (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Le]
compB (Lte a b) = error $ "Supplied a non Aexp statement into a integer comparison operation"

compB (EqualsInt (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Equ]
compB (EqualsInt a b) = error $ "Supplied a non Aexp statement into a integer equality operation"

compB (Not (Bexp a)) = (compB a) ++ [Neg]
compB (Not a) = error $ "Supplied a non Bexp statement into a not operation"

compB (EqualsBool (Bexp a) (Bexp b)) = (compB b) ++ (compB a) ++ [Equ]
compB (EqualsBool a b) = error $ "Supplied a non Bexp statement into a bool equality operation"

compB (AndBool (Bexp a) (Bexp b)) = (compB b) ++ (compB a) ++ [And]
compB (AndBool a b) = error $ "Supplied a non Bexp statement into a bool and operation"

compB (Bool True) = [Tru]
compB (Bool False) = [Fals]
compB (VarB a) = [Fetch a]


compStm :: Stm -> Code
compStm (Aex a) = compA a
compStm (Bexp a) = compB a

-- assignment
compStm (Assignment name (Aex value)) = (compA value) ++ [Store name]
compStm (Assignment name (Bexp value)) = (compB value) ++ [Store name] 
compStm (Assignment name a) = error $ "Supplided a non Aexp/Bexp statement into the store variable operation"

-- if
compStm (If (Bexp bexp) stmsThen  stmsElse) = (compB bexp) ++ [Branch (compStms stmsThen) (compStms stmsElse)]
compStm (If a stmsThen  stmsElse) = error $ "Supplided a non Bexp statement into the if operation"

-- while
compStm (While (Bexp bexp) stmsDo) = [Loop (compB bexp) (compStms stmsDo)]
compStm (While a stmsDo) = error $ "Supplided a non Bexp statement into the while operation"




compStms :: [Stm] -> Code
compStms [] = []
compStms (x:xs) = (compStm x) ++ (compStms xs)

compile :: Program -> Code
compile prog = compStms prog 


--- Tokenizer section
data Token = Punctuation Char | Number Integer | Identifier String | Operator String | Keyword String | TAssignment | TBool Bool
  deriving Show


tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (x:xs)
  | x `elem` "+-*" = [Operator [x]] ++ tokenizer xs
  | x `elem` "();" = [Punctuation x] ++ tokenizer xs
  | x `elem` " " = tokenizer xs
  | isDigit x = let (num, rest) = span isDigit (x:xs)
                in [Number (read num :: Integer)] ++ tokenizer rest
  | isAlpha x = case (takeWhile (\x -> not (x `elem` " +-*();=:")) (x:xs)) of 
    "while" -> [Keyword "while"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "if" -> [Keyword "if"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "else" -> [Keyword "else"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "not" -> [Keyword "not"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "do" -> [Keyword "do"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "then" -> [Keyword "then"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "and" -> [Operator "and"] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "True" -> [TBool True] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    "False" -> [TBool False] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
    otherwise -> [Identifier otherwise] ++ tokenizer (dropWhile(\x -> not (x `elem` " +-*();=:")) (x:xs))
  | x `elem` ":" = let (next:rest) = xs
                   in case next of '=' -> [TAssignment] ++ tokenizer rest
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
      Just (expr, restTokens2) -> Nothing -- TODO: add close parenthesis not found return nothing
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
parseTvalOrVar (Punctuation '(': restTokens) =
  case (parseBexp restTokens) of
    Just(exp, (Punctuation ')':restTokens1)) -> Just(exp, restTokens1)
    Just(exp, restTokens) -> Nothing -- missing Close parenthesis
    Nothing -> Nothing
parseTvalOrVar tokens = Nothing

-- Aexp derived Bexp parsers
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
  case (parseLteOrAexp tokens) of
    Just(Bexp a, restTokens) -> Just(Bexp a, restTokens)
    otherwise ->
      case (parseEqIntOrAexp tokens) of
        Just(Bexp a, restTokens) -> Just(Bexp a, restTokens)
        Just(a,b) -> Nothing -- Aexp or something else inside boolean expression
        Nothing -> Nothing

-- rest of Bexps

parseBexpTValOrAexpDerivedBexp :: [Token] -> Maybe (Stm, [Token])
parseBexpTValOrAexpDerivedBexp tokens = 
  case (parseAexpDerivedBexp tokens) of
    Just(exp, restTokens) -> Just(exp, restTokens)
    otherwise -> 
      case (parseTvalOrVar tokens) of
        Just(exp1, restTokens1) -> Just(exp1, restTokens1)
        Nothing -> Nothing


-- test: (parseBexpNotOrRest . tokenizer) "not (i == 1)"
parseBexpNotOrRest :: [Token] -> Maybe (Stm, [Token])
parseBexpNotOrRest (Keyword "not": restTokens) =
  case (parseBexpTValOrAexpDerivedBexp restTokens) of
    Just(exp, restTokens2) -> Just(Bexp (Not exp), restTokens2)
    Nothing -> Nothing
parseBexpNotOrRest tokens = parseBexpTValOrAexpDerivedBexp tokens

-- test: (parseBexpEqBoolOrRest . tokenizer) " 2 <= 5 = 3 == 4"
parseBexpEqBoolOrRest :: [Token] -> Maybe (Stm, [Token])
parseBexpEqBoolOrRest tokens = 
  case (parseBexpNotOrRest tokens) of
    Just(exp1, (Operator "=": restTokens)) ->
      case(parseBexpEqBoolOrRest restTokens) of 
        Just (exp2, restTokens2) -> Just (Bexp (EqualsBool exp1 exp2), restTokens2)
        Nothing -> Nothing
    Just(a,b) -> Just(a,b)
    Nothing -> Nothing

-- test: (parseBexpAndOrRest . tokenizer) "not True and 2 <= 5 = 3 == 4"
parseBexpAndOrRest :: [Token] -> Maybe (Stm, [Token])
parseBexpAndOrRest tokens = 
  case (parseBexpEqBoolOrRest tokens) of
    Just(exp1, (Operator "and": restTokens)) ->
      case(parseBexpAndOrRest restTokens) of 
        Just (exp2, restTokens2) -> Just (Bexp (AndBool exp1 exp2), restTokens2)
        Nothing -> Nothing
    Just(a,b) -> Just(a,b)
    Nothing -> Nothing

parseBexp :: [Token] -> Maybe (Stm, [Token])
parseBexp tokens = parseBexpAndOrRest tokens

-- general

parseAexpOrBexp :: [Token] -> Maybe (Stm, [Token])
parseAexpOrBexp tokens = 
  case (parseAexp tokens) of
    Just(expr, restTokens) -> Just (expr, restTokens)
    Nothing -> case (parseBexp tokens) of
      Just(expr, restTokens) -> Just (expr, restTokens)
      Nothing -> Nothing


-- assignemnt parser

parseAssignment :: [Token] -> Maybe(Stm, [Token])
parseAssignment (Identifier a: TAssignment : restTokens) = 
  case (parseAexpOrBexp restTokens) of
    Just(exp, restTokens1) -> Just (Assignment a exp, restTokens1)
    Nothing -> Nothing
parseAssignment tokens = Nothing

-- IF parser

parseElse :: [Token] -> Maybe([Stm], [Token])
parseElse (Keyword "else": Punctuation '(': restTokens) =
  case (parseStatements restTokens) of
        Just(expList, (Punctuation ')': restTokens2)) -> Just(expList, restTokens2)
        Just(a,b) -> Nothing -- Didn't close parenthesis
        Nothing -> Nothing
parseElse (Keyword "else": restTokens) =
  case (parseStatement restTokens) of
    Just(exp, restTokens1) -> Just([exp], restTokens1)
    Nothing -> Nothing


parseIf :: [Token] -> Maybe(Stm, [Token])
parseIf (Keyword "if": restTokens) =
  case (parseBexp restTokens) of
    Just(exp, (Keyword "then": Punctuation '(' : restTokens1)) -> 
      case (parseStatements restTokens1) of
        Just(expListThen, (Punctuation ')': restTokens2)) -> 
          case (parseElse restTokens2) of
            Just(expListElse, restTokens3) -> Just(If exp expListThen expListElse, restTokens3)
            otherwise -> Nothing
        Just(a,b) -> Nothing -- Didn't close parenthesis
        Nothing -> Nothing
    Just(exp, (Keyword "then": restTokens1)) ->
      case (parseStatement restTokens1) of
        Just(expThen, restTokens2) ->
          case (parseElse restTokens2) of
            Just(expListElse, restTokens3) -> Just(If exp [expThen] expListElse, restTokens3)
            otherwise -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
parseIf tokens = Nothing -- if it doesn't match with if
        
-- parse while

-- NOTE: I still don't know if a while statement can have a do statement without parenthesis
parseWhile :: [Token] -> Maybe(Stm, [Token])
parseWhile (Keyword "while": restTokens) =
  case (parseBexp restTokens) of
    Just(bexp, (Keyword "do": Punctuation '(': restTokens1)) ->
      case (parseStatements restTokens1) of
        Just(expListThen, (Punctuation ')': restTokens2)) -> Just (While bexp expListThen, restTokens2)
        Just(a,b) -> Nothing -- Didn't close parenthesis
        Nothing -> Nothing
    Just(a, b) -> Nothing -- doesn't have do
    Nothing -> Nothing
parseWhile tokens = Nothing -- doesn't match while

parseStatement :: [Token] -> Maybe(Stm, [Token])
parseStatement tokens = 
  -- TODO: add while 
  case(parseIf tokens) of
    Just(stm, restTokens) -> Just(stm, restTokens)
    Nothing ->
      case(parseWhile tokens) of
        Just(stm, (Punctuation ';': restTokens)) -> Just(stm, restTokens)
        Just(a,b) -> Nothing -- missing semi colon
        Nothing -> 
          case(parseAssignment tokens) of
            Just(stm, (Punctuation ';': restTokens)) -> Just(stm, restTokens)
            Just(a,b) -> Nothing -- missing semi colon
            Nothing -> 
              case(parseAexpOrBexp tokens) of
                Just(stm, (Punctuation ';': restTokens)) -> Just(stm, restTokens)
                Just(a,b) -> Nothing -- missing semi colon
                Nothing -> Nothing


parseStatements :: [Token] -> Maybe([Stm], [Token])
parseStatements [] = Nothing
parseStatements tokens = 
  case (parseStatement tokens) of
    Just(stm, restTokens) -> 
      case (parseStatements restTokens) of
        Just(stms, restTokens2) -> Just([stm] ++ stms, restTokens2)
        Nothing -> Just([stm], restTokens)
    Nothing -> Nothing

parse :: String -> Program
parse input = case((parseStatements . tokenizer) input) of
  Just(program, []) -> program
  otherwise -> error $ "failed to parse the program..."

testParser :: String -> (String, String) 
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
