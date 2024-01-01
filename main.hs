import Data.List
import Data.Char
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- The Stack can contain both boolean or integer types and because we need to differenciate between them 
--  (some instructions require for the arguments to be either integer or boolean). Therefore we leverage
--  the data declarations to do it.

data EvaluationData = Boolean Bool | Int Integer
  deriving Show


-- We implement the stack as a list, while we could abstract the list to guarantee that devs use the stack as intended, 
--  it's simpler to implement this as a list. And guarantee the stack behaviour by ourselves.
type Stack = [EvaluationData]

-- The state map, again, could be implemented using the standard Data.Map module, but we implement it as a list and 
--  make the behaviour the same as a Map (except of course, the time complexity which is O(n) instead of O(log n)
--  but it's an unnecessary optimization for now).
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

-- Before converting the state list into a string, we have to first order it alphabetically as required per the spec.
stateSort :: StateData -> StateData -> Ordering
stateSort (keyA, _) (keyB, _) = compare keyA keyB 

state2Str :: State -> String
state2Str state =  intercalate "," (map (\x -> stateData2Str x) (sortBy stateSort state))

-- interpreter
-- We leverage the power of pattern matching to guarantee that the instruction has the required arguments. And throw
--  and error if it's on a invalid configuration.
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
--  because we don't use maps, we have to find if the the key already exists and override it, otherwise we just append it.
runInst (Store key) (code, (x:xs), state) = case (filter (\(skey, _) -> skey == key) state) of [] -> (code, xs, state ++ [(key, x)])
                                                                                               (l: _) -> (code, xs, (filter (\(skey, _) -> skey /= key) state) ++ [(key, x)])

-- fetch op
--  because we don't use maps, we have to filter the whole list to find the corresponding variable.
runInst (Fetch key) (code, stack, state) = case (filter (\(skey, _) -> skey == key) state) of [] -> error $ "Run-time error"
                                                                                              ((_, value):_) -> (code, [value] ++ stack,state)
-- branch op
runInst (Branch c1 c2) (code, (Boolean a: xs), state)
  | a == True = (c1 ++ code, xs, state)
  | a == False = (c2 ++ code, xs, state)

runInst (Branch c1 c2) (code, stack, state) = ([], stack, state) -- according to the spec, it should halt instead of throwing a error?

-- loop op
runInst (Loop c1 c2) (code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

-- in the run function we pop the current instruction and give the rest to the runInst function, while there are still
--  instructions left.
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


-- An Aexp represents an arithmetric expression, that can have integer or variables as leaf elements.
--  To simplify the logic of the parser, the operations have the Stm data type instead of Aexp. However,
--  They only support having other Aexps as a child (this is guareented by the compiler functions.) 
data Aexp = Sum Stm Stm | Subs Stm Stm| Multi Stm Stm | IntLit Integer | Var String
  deriving Show

-- A Bexp represents a boolean operation. The operations Lte and EqualsInt support only Aexps as child elements.
--  The rest only support Bexps as child elements. This is guaranteed by the compiller.compA
--  We also "duplicate" the variable because it can be applied in shorthands like: if (x) then y:=1; else y:=2;
--  We also support the boolean literals here.
data Bexp = Lte Stm Stm | EqualsInt Stm Stm | Not Stm | EqualsBool Stm Stm | AndBool Stm Stm | Bool Bool | VarB String
  deriving Show

-- We also support having the raw Aexps or Bexps because the spec isn't explicit about it.
-- Assignment: consists of the variable name and a Bexp or a Aexp
-- If: consists of the Bexp, the list of statements if it's true and finally the list of statements if it's false
-- While: consists of the Bexp, and the body
data Stm = Aex Aexp | Bexp Bexp | Assignment String Stm | If Stm [Stm] [Stm] | While Stm [Stm]
  deriving Show

type Program = [Stm]

-- The compiler functions check if the arguments correspond either the correct statement type (Stm).
-- If we think the Aexp as the tree, it will compile first the right side and then it compiles the left side, because of non
-- commutative operations (eg.: subtraction)
compA :: Aexp -> Code
compA (IntLit a) = [Push a]
compA (Var a) = [Fetch a]

compA (Multi (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Mult]
compA (Multi a b) = error $ "Supplied a non Aexp statement into a multiplication operation"

compA (Subs (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Sub]
compA (Subs a b) = error $ "Supplied a non Aexp statement into a subtraction operation"

compA (Sum (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Add]
compA (Sum a b) = error $ "Supplied a non Aexp statement into a addition operation"

-- If we think the Bexp as the tree, it will compile first the right side and then it compiles the left side. Because of non
-- commutative operations (eg.: Lte)
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

-- compStm generalizes Aexps and Bexps to a unique function to agreggate to the orther three types of Stms.
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



-- we loop through all Stm trees until there's none left, and append the output code to the final program.
compStms :: [Stm] -> Code
compStms [] = []
compStms (x:xs) = (compStm x) ++ (compStms xs)

compile :: Program -> Code
compile prog = compStms prog 


-- Tokenizer section
-- The tokenizer is reponsible for spliting the input program (which is a string) to lexical tokens that belong to a number of categories
--  to be passed to the other steps of the parser. Simplifying then, the logic of the parser itself.
-- We have 7 types of tokens: Punctuation represents the delimeters of the language "();", Number represents all number literals,
--  the identifier represents every word that isn't a keyword (there are no sting support in this language so we don't have to worry about that),
--  the operators, the keywords (which are strings that are reserved for the language itself, eg.: while, if, else, not...),
--  the assignment "operator" and the Boolean literal.
data Token = Punctuation Char | Number Integer | Identifier String | Operator String | Keyword String | TAssignment | TBool Bool
  deriving Show

delims = " +-*();=:"

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (x:xs)
  | x `elem` "+-*" = [Operator [x]] ++ tokenizer xs
  | x `elem` "();" = [Punctuation x] ++ tokenizer xs
  | x `elem` " " = tokenizer xs
  | isDigit x = let (num, rest) = span isDigit (x:xs)
                in [Number (read num :: Integer)] ++ tokenizer rest
  | isAlpha x = case (takeWhile (\x -> not (x `elem` delims)) (x:xs)) of 
    "while" -> [Keyword "while"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "if" -> [Keyword "if"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "else" -> [Keyword "else"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "not" -> [Keyword "not"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "do" -> [Keyword "do"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "then" -> [Keyword "then"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "and" -> [Operator "and"] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "True" -> [TBool True] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    "False" -> [TBool False] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
    otherwise -> [Identifier otherwise] ++ tokenizer (dropWhile(\x -> not (x `elem` delims)) (x:xs))
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
-- In other to parse an arithmetric expression, we need to consider the levels of precedence of all operations:
--    An expression surrounded by parenthesis, a variable or a number literal have the higher level of precedence and
--     have to processed first. If it's a expression surrounded by parenthesis, we call the parser of the lowest level
--     of precence to parse the rest of the expression;
--    Next, we have multiplication;
--    And finally, we have addition and subtraction (which is implemented differently as you can see below why).

parseIntOrVarOrParen :: [Token] -> Maybe (Stm, [Token])
parseIntOrVarOrParen (Operator "-": Number a: restTokens) = Just (Aex (IntLit (-1 * a)), restTokens) -- handle negative
parseIntOrVarOrParen (Number a: restTokens) = Just (Aex (IntLit a), restTokens)
parseIntOrVarOrParen (Identifier a: restTokens) = Just (Aex (Var a), restTokens)
parseIntOrVarOrParen (Punctuation '(': restTokens)
  = case (parseAexp restTokens) of 
      Just (expr, (Punctuation ')': restTokens2)) -> Just (expr, restTokens2)
      Just (expr, restTokens2) -> Nothing -- TODO: add close parenthesis not found return nothing
      Nothing -> Nothing 
parseIntOrVarOrParen tokens = Nothing

-- AFAIK multiplication can be right associative (in this case) because there isn't anything with the same "priority"
--  In case that we want to implement division we need to change the logic of prod to be as the logic
--  Of addition and subtraction 

parseProdOrRest :: [Token] -> Maybe (Stm, [Token])
parseProdOrRest tokens =
  case (parseIntOrVarOrParen tokens) of
    Just (expr1, (Operator "*": restTokens1)) -> 
      case (parseIntOrVarOrParen restTokens1) of
        Just (expr2, restTokens2) -> Just (Aex (Multi expr1 expr2), restTokens2)
        Nothing -> Nothing
    Just (expr1, restTokens1) -> Just (expr1, restTokens1) 
    Nothing -> Nothing


-- Because Addition and Subtraction must be left associative, therefore we can't just make "left recursion to parse it".
-- Instead of parsing the first expression, matching the operator and recur on the rest of the tokens, we match the first
--  and second expressions (with the operator on the middle ofc), and then we try to group it if there's still operators
--  of the same precedence to the right and recur on it. For example: 1+2+3+4+5 = (((1+2)+3)+4)+5 instead of  1+(2+(3+(4+5))),
--  which only makes a difference on subtraction eg: (-1)-(-1)-1 which depending of using left or right association can give 
--  -1 or 1 as a result, respectively.

parseSumOrSubOrRestHelper :: Aexp -> [Token] -> Maybe(Stm, [Token])
parseSumOrSubOrRestHelper currAexp (Operator "+": tokens) =
  case(parseProdOrRest tokens) of
    Just (expr1, restTokens1) -> parseSumOrSubOrRestHelper (Sum (Aex currAexp) expr1) restTokens1
    Nothing -> Nothing -- couldn't find nothing after a plus means that it probably is an error
parseSumOrSubOrRestHelper currAexp (Operator "-": tokens) =
  case(parseProdOrRest tokens) of
    Just (expr1, restTokens1) -> parseSumOrSubOrRestHelper (Subs (Aex currAexp) expr1) restTokens1
    Nothing -> Nothing -- couldn't find nothing after a minus means that it probably is an error
parseSumOrSubOrRestHelper currAexp tokens = Just(Aex currAexp, tokens)


parseSumOrSubOrRest :: [Token] -> Maybe (Stm, [Token])
parseSumOrSubOrRest tokens = 
  case (parseProdOrRest tokens) of
    Just (expr1, (Operator "+": restTokens1)) -> 
      case (parseProdOrRest restTokens1) of
        Just (expr2, restTokens2) -> parseSumOrSubOrRestHelper (Sum expr1 expr2) restTokens2
        Nothing -> Nothing
  
    Just (expr1, (Operator "-": restTokens1)) -> 
      case (parseProdOrRest restTokens1) of
        Just (expr2, restTokens2) -> parseSumOrSubOrRestHelper (Subs expr1 expr2) restTokens2
        Nothing -> Nothing
  
    Just (expr1, restTokens1) -> Just (expr1, restTokens1)
    Nothing -> Nothing

-- Just a wrapper to simplify the naming of functions further on.
parseAexp :: [Token] -> Maybe (Stm, [Token])
parseAexp tokens = parseSumOrSubOrRest tokens

-- Bexp
-- On boolean expressions, we have the following levels of precedence, on decending order:
--      - Lte, EqInt, Boolean literals or exp with parenthesis; (Lte is parsed first then EqInt) 
--        (and they are parsed first then Bool lits or exp with parenthesis).
--      - not
--      - EqBool
--      - And
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
-- A wrapper that tries to parse a Aexp first and if it does not suceed, tries to parse a Bexp. Useful for Assignment operations.
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
-- On the if parser, we have to handle the parenthesis because we can have multiple statements inside a if/else body but
--  we can also have only one statement that doesn't require parenthesis.
-- We parse the else seperately to make the if parser function smaller. 
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
-- While statements must always have parenthesis on the body so it's easier to handle compared to the if statements.
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


-- On parseStatement we combine all parsers and try to consume the semicolon delimiter (if applicable).
-- The combination of parsers uses backtracking (if a parser fails, it backtracks and tries the next one until it succeeds or fails all parsers)
-- which might not be the most efficient way to parse this language but the performance is good enough.
parseStatement :: [Token] -> Maybe(Stm, [Token])
parseStatement tokens = 
  case(parseIf tokens) of
    Just(stm, (Punctuation ';': restTokens)) -> Just(stm, restTokens)
    Just(stm, restTokens) -> Just(stm, restTokens) -- Depending on the if else body, it can not require the semicolon delimeter.
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

-- we try to parse the list of tokens until the the list is empty or it cannot parse the rest of the token list
--  (useful for the statements inside of if and while statements.)
parseStatements :: [Token] -> Maybe([Stm], [Token])
parseStatements [] = Nothing
parseStatements tokens = 
  case (parseStatement tokens) of
    Just(stm, restTokens) -> 
      case (parseStatements restTokens) of
        Just(stms, restTokens2) -> Just([stm] ++ stms, restTokens2)
        Nothing -> Just([stm], restTokens)
    Nothing -> Nothing

-- we combine the tokenizer and the parseStatements to generate the program however, we throw if there are
--  tokens left (which means the input is not a valid program in this language.)
parse :: String -> Program
parse input = case((parseStatements . tokenizer) input) of
  Just(program, []) -> program
  otherwise -> error $ "failed to parse the program..."

testParser :: String -> (String, String) 
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")