# PFL - 2nd Project Assignment

Group **T11_G10**:
 - Luís Miguel Mota Duarte, up202108734 (Contribution: 50%)
 - Ana Rita Varanda Cachaldora Pereira, up202108798 (Contribution: 50%)

# Introduction

In this project, we were asked to build, what's commonly called, an interpreter in two parts:
 - A low-level Stack Machine with configurations of form $(c,e,s)$ where $c$ is a list of instructions, $e$ is the evaluation stack and $s$ is the storage (where variables are stored). The evaluation stack only consists of integer or boolean values. This Stack Machine implements all basic boolean and integer operations and also branches and loops.
 - A compiler for a simple imperative language consisting of assignments, if-then-else statements, while loops, arithmetic expressions, and boolean expressions that compile to the Stack Machine's instructions.


# Implementation


Firstly, we defined the Evaluation Stack (the evaluation stack can both accept integers or booleans and it's important to retain the type information to detect invalid configurations) and the State storage:

```haskell
data EvaluationData = Boolean Bool | Int Integer
  deriving Show

type Stack = [EvaluationData]

type StateData = (String, EvaluationData)

type State = [StateData]
```


We implemented both as Lists, while this approach doesn't inherently guarantee the behavior of the Stack or the Map, it's more simple to implement but gives us the responsibility of following the behavior of each data structure. While converting the Stack into a String (for testing purposes) is trivial, the State must have its elements ordered alphabetically by the variable name:

```haskell
stateData2Str :: StateData -> String
stateData2Str (key,value) = key ++ "=" ++ evaluation2Str value

-- Before converting the state list into a string, we have to first order it alphabetically as required per the spec.
stateSort :: StateData -> StateData -> Ordering
stateSort (keyA, _) (keyB, _) = compare keyA keyB 

state2Str :: State -> String
state2Str state =  intercalate "," (map (\x -> stateData2Str x) (sortBy stateSort state))
```


The Stack Machine has all of the following instructions: push-n, add, mult, sub, true, false, eq, le,
and, neg, fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2). They can be found in the `runInst` function which encapsulates the logic of all of them and more importantly, we leverage the _Pattern Matching_ features of _Haskell_ to ensure the valid state of the stack/state for example:

```haskell 
runInst :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- add
runInst Add (code, (Int a: Int b: xs), state) = (code, (Int (a+b):xs), state)
runInst Add (code, stack, state) = error $ "Run-time error"
```


As you can see above, when the Add instruction is given as an argument it checks if the evaluation stack contains two integers at the top. If it has, it returns a new stack containing the result of the sum and the rest of the stack. Otherwise, if the first two elements at the top are not integers the interpreter will throw a runtime error.

Because we don't use the standard `Map` from the module `Data.Map` the fetch and store instructions requires filtering the state list, which while it can be a bit slower (O(n) vs O(log n)), the implementation is simple:


```haskell
-- store op
--  because we don't use maps, we have to find if the the key already exists and override it, otherwise we just append it.
runInst (Store key) (code, (x:xs), state) = 
    case (filter (\(skey, _) -> skey == key) state) of 
        [] -> (code, xs, state ++ [(key, x)])
        (l: _) -> (code, xs, (filter (\(skey, _) -> skey /= key) state) ++ [(key, x)])

-- fetch op
--  because we don't use maps, we have to filter the whole list to find the corresponding variable.
runInst (Fetch key) (code, stack, state) = 
    case (filter (\(skey, _) -> skey == key) state) of 
        [] -> error $ "Run-time error"
        ((_, value):_) -> (code, [value] ++ stack,state)                                                        
```


Before each instruction, we pop it from the instruction list and call `runInst` which returns the new VM state until there are no instructions left:


```haskell
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) --when there's no code left leave
run ((x:xs), stack, state) = run(runInst x (xs, stack, state))
```


---


Finally, we have the second part of the assignment, the compiler. A compiler takes a program string parses it according to a _grammar_ and compiles it into some representation of machine code.

First, we take the program string and divide it into "_lexical tokens_" using a _lexer/tokenizer_ that simplifies the rest of the parser logic as follows:

```haskell
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
```

We define a data type, that contains all tokens relevant to the language:
 - Punctuation (it includes the chars `();`);
 - Number (it contains all number literals);
 - Identifier (it's always assumed to be a variable because the language doesn't support char/string types);
 - Operator (`<=`, `=`, `==`, `+`, `-`, `*`);
 - Keyword (contains almost all reserved words for the language: `not`, `while`, `if`, `and`, `do`, `then`, `not`, `else`);
 - Assignment (`:=`);
 - Boolean (`True` or `False`, case sensitive).

If something doesn't comply with the lexical analysis, the parser will throw an error. 

After _lexing_ the program, we analyze the tokens _semantically_ outputting an AST (Abstract Syntax Tree). To generate the AST we have two main methods: bottom-up parsing and top-down parsing, in this case, we use top-down parsing (for almost everything) because it's easier to implement in a functional language. Firstly, we define all types to generate the respective tree: 

```haskell
data Aexp = Sum Stm Stm | Subs Stm Stm| Multi Stm Stm | IntLit Integer | Var String
  deriving Show

data Bexp = Lte Stm Stm | EqualsInt Stm Stm | Not Stm | EqualsBool Stm Stm | AndBool Stm Stm | Bool Bool | VarB String
  deriving Show

data Stm = Aex Aexp | Bexp Bexp | Assignment String Stm | If Stm [Stm] [Stm] | While Stm [Stm]
  deriving Show

type Program = [Stm]
```

Note that, in Aexp or Bexp we accept always a `Stm` as a child element because it's easier to code the parser for (because the return types of the parser functions can be practically the same.). We check if the child has a correct type in the compiler step (eg.: having an if on an assignment statement.) 

To make a top-down parser or a _recursive descent parser_ we need to first implement the operations which have the most precedence. On Aexps, or _arithmetic expressions_ we have in descending order:
 - Number literals, Variables, or Aexps that have parentheses around them (these have the same precedence);
 - Multiplication;
 - Addition and subtraction.

```haskell
parseIntOrVarOrParen :: [Token] -> Maybe (Stm, [Token])
parseIntOrVarOrParen (Operator "-": Number a: restTokens) = Just (Aex (IntLit (-1 * a)), restTokens) -- handle negative numbers
parseIntOrVarOrParen (Number a: restTokens) = Just (Aex (IntLit a), restTokens)
parseIntOrVarOrParen (Identifier a: restTokens) = Just (Aex (Var a), restTokens)
parseIntOrVarOrParen (Punctuation '(': restTokens)
  = case (parseAexp restTokens) of 
      Just (expr, (Punctuation ')': restTokens2)) -> Just (expr, restTokens2)
      Just (expr, restTokens2) -> Nothing 
      Nothing -> Nothing 
parseIntOrVarOrParen tokens = Nothing


parseProdOrRest :: [Token] -> Maybe (Stm, [Token])
parseProdOrRest tokens =
  case (parseIntOrVarOrParen tokens) of
    Just (expr1, (Operator "*": restTokens1)) -> 
      case (parseIntOrVarOrParen restTokens1) of
        Just (expr2, restTokens2) -> Just (Aex (Multi expr1 expr2), restTokens2)
        Nothing -> Nothing
    Just (expr1, restTokens1) -> Just (expr1, restTokens1) -- if it cannot parse a product but parses something of higher precedence return it
    Nothing -> Nothing
```

As you can see from the snippet above, the operation with lower precedence will recursively call the parser with higher precedence depending on the return of the above parsers. After returning something, or nothing, we can try to parse the lower precedence operators (if we cannot parse the lower precedence operators we return the result from the parsers "_above_" ). Note that the parser returns a `Maybe` type because the parser can fail and return `Nothing` otherwise, it returns the parsed statement and the tokens that haven't been consumed yet. 

Also, note that multiplication is technically _right-associative_ but it doesn't make a difference in this case because division is not implemented in this language.


To finalize the Aexps, we have the addition and the subtraction left to implement. Because subtraction is not commutative, these operations must be _left-associative_ otherwise the parsed tree might be wrong and lead to the wrong result (eg.: $-1-(-1)+1 $ is equal to $-1$ but if subtraction is parsed in a _right-associative_ way it outputs $1$). To solve this, we use a technique from _bottom-up parsing_: 

```haskell
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
```

The Bexps, or _Boolean Expressions_ are easier to implement and have the following levels of precedence:

- `<=` 
- `==` 
- boolean literals, variables, and Bexps with parenthesis
- `not`
- `=`
- `and`


Contrary to the Aexps precedence levels, boolean literals variables are not parsed first because, `<=` and `==` depend on Aexps and therefore it makes more sense for them to be parsed first.

The assignment statement is fairly trivial to implement, having an identifier and after the operator having an Aexp or a Bexp. 
The if statement firstly has a Bexp and then two lists of statements (the _then_ body and the _else_ body) it might have a semi-colon at the end depending if the _else_ body contains more than one statement. The while statement firstly has a Bexp and then the loop body which must be parenthesized and must have a semi-colon at the end.

All statements must end with a semi-colon and the final parser is implemented as follows:
```haskell 
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
```


For an input to be parsed successfully, it must reach the end of the parser with no tokens left and a valid program. After being parsed, the program can be compiled or translated to the Stack Machine instructions. 

On Aexps and Bexps, we first compile the right-hand of the AST and then we compile the left side and append to the right side instructions (to accommodate non-commutative operators like subtraction and the integer comparison). Finally, we append the desired operation to the final list: 
```haskell
compA (Multi (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Mult]
compA (Multi a b) = error $ "Supplied a non Aexp statement into a multiplication operation"

compA (Subs (Aex a) (Aex b)) = (compA b) ++ (compA a) ++ [Sub]
compA (Subs a b) = error $ "Supplied a non Aexp statement into a subtraction operation"
```


Note that we check if the `Stm` child is of the correct type. Finally, we can compile the other three statements and append them into a big list of instructions that the Stack Machine can run: 

```haskell 
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

```

# Conclusions

After this assignment, we successfully implemented all the required objectives and learned how an interpreter works, and how a _Stack Machine_ can be implemented. On the imperative language side, we learned about the fundamentals of _Compiler Design_ and in our case, how to write a _recursive descent parser_ from scratch without using other existing modules like _Parsec_ which contributes to the essential objective of this assignment: learning how a functional language works and gain experience on how to solve real problems with it.  