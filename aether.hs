import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.Char (isDigit)

-- 1. ТИПЫ ДАННЫХ ЭФИРА
data Value 
    = VStr String 
    | VInt Int 
    | VBool Bool
    | VList [Value]
    | VCode [Instruction]
    deriving (Show, Eq)

data Instruction 
    = Push Value          
    | LoadVar String      
    | SaveVar String      
    | ActionTell          
    | ActionAdd           
    | ActionSub           
    | ActionMul           
    | ActionDiv           
    | ActionGt
    | ActionEq            
    | ActionLt        
    | ActionNeq           
    | ActionAnd           
    | ActionOr            
    | ActionGe            
    | ActionLe    
    | ActionNot
    | ActionDup           
    | ActionDrop          
    | ActionSwap          
    | ActionIf [Instruction] [Instruction] 
    | ActionWhile [Instruction] [Instruction]
    | ActionAppend        
    | ActionAt            
    | ActionLen           
    | ActionRead          
    | ActionToInt
    | ActionRemove        
    | ActionSet          
    | ActionWeave
    | ActionReadFile
    | ActionEval
    | ActionWriteFile
    | ActionArgs
    | ActionAtStr   
    | ActionLenStr  
    | ActionSlice   
    deriving (Show, Eq)

type Memory = Map.Map String Value
type Stack = [Value]

isNumeric :: String -> Bool
isNumeric ('-':ds) = not (null ds) && all isDigit ds
isNumeric ds       = not (null ds) && all isDigit ds

tokenize :: String -> [String]
tokenize [] = []

tokenize ('[':'[':cs) = "[[" : tokenize cs
tokenize (']':']':cs) = "]]" : tokenize cs
tokenize ('[':']':cs) = "[]" : tokenize cs  


tokenize str@('!':'!':_) = tokenize (dropWhile (\c -> c /= '\n' && c /= '\r') str)

tokenize (c:cs) | c `elem` " \t\n\r" = tokenize cs

tokenize ('\"':cs) = 
    let (str, rest) = span (/= '\"') cs
    in ("\"" ++ str ++ "\"") : tokenize (drop 1 rest)

tokenize str = 
    let (word, rest) = span (\c -> not (c `elem` " \t\n\r\"()[]")) str 
    in if null word 
       then case str of
            ('(':cs) -> "(" : tokenize cs
            (')':cs) -> ")" : tokenize cs
            ('[':cs) -> "[" : tokenize cs 
            (']':cs) -> "]" : tokenize cs 
            (c:cs)   -> [c] : tokenize cs 
       else word : tokenize rest

getBlockTokens :: String -> String -> [String] -> Int -> ([String], [String])
getBlockTokens _ _ [] _ = ([], [])
getBlockTokens open close (tok:toks) depth
    | tok == open  = let (inner, rest) = getBlockTokens open close toks (depth + 1)
                     in (tok : inner, rest)
    | tok == close = if depth == 0
                     then ([], toks)
                     else let (inner, rest) = getBlockTokens open close toks (depth - 1)
                          in (tok : inner, rest)
    | otherwise    = let (inner, rest) = getBlockTokens open close toks depth
                     in (tok : inner, rest)


parseTokens :: [String] -> [Instruction]
parseTokens [] = []
parseTokens (tok:toks) = case tok of
    "tll"  -> ActionTell                : parseTokens toks
    "+"    -> ActionAdd                 : parseTokens toks
    "-"    -> ActionSub                 : parseTokens toks
    "*"    -> ActionMul                 : parseTokens toks
    "/"    -> ActionDiv                 : parseTokens toks
    ">"    -> ActionGt                  : parseTokens toks
    "<"    -> ActionLt                  : parseTokens toks
    "=="   -> ActionEq                  : parseTokens toks
    "!="   -> ActionNeq                 : parseTokens toks
    "&&"   -> ActionAnd                 : parseTokens toks
    "||"   -> ActionOr                  : parseTokens toks
    ">="   -> ActionGe                  : parseTokens toks
    "<="   -> ActionLe                  : parseTokens toks
    "not"  -> ActionNot                 : parseTokens toks
    "dup"  -> ActionDup                 : parseTokens toks
    "drop" -> ActionDrop                : parseTokens toks
    "swap" -> ActionSwap                : parseTokens toks
    "[]"     -> Push (VList [])         : parseTokens toks
    "append" -> ActionAppend            : parseTokens toks
    "at"     -> ActionAt                : parseTokens toks
    "len"    -> ActionLen               : parseTokens toks
    "read"   -> ActionRead              : parseTokens toks
    "toInt"  -> ActionToInt             : parseTokens toks
    "remove" -> ActionRemove            : parseTokens toks
    "set"    -> ActionSet               : parseTokens toks
    "weave"  -> ActionWeave             : parseTokens toks
    "eval" -> ActionEval                : parseTokens toks
    "readfile" -> ActionReadFile        : parseTokens toks
    "writefile" -> ActionWriteFile      : parseTokens toks
    "args" -> ActionArgs                : parseTokens toks
    "atStr" -> ActionAtStr              : parseTokens toks
    "lenStr" -> ActionLenStr            : parseTokens toks
    "slice" -> ActionSlice              : parseTokens toks
    "?"    -> parseTokens toks


    "[[" -> 
        let (innerTokens, rest) = getBlockTokens "[[" "]]" toks 0
        in Push (VCode (parseTokens innerTokens)) : parseTokens rest
    
    
    "->"  -> 
        case toks of
            (name:":":"v":rest) -> SaveVar name : parseTokens rest
            (name:rest)         -> SaveVar name : parseTokens rest
            []                  -> []
            
    
    "-->>" -> 
        let (trueBlockTokens, rest1) = getBlockTokens "-->>" "<<--" toks 0
            trueBranch = parseTokens trueBlockTokens
        in case rest1 of
            ("--<<":rest2) -> 
                let (falseBlockTokens, rest3) = getBlockTokens "--<<" ">>--" rest2 0
                    falseBranch = parseTokens falseBlockTokens
                in ActionIf trueBranch falseBranch : parseTokens rest3
            _ -> 
                ActionIf trueBranch [] : parseTokens rest1

   
    "(" -> 
        let (condTokens, rest1) = getBlockTokens "(" ")" toks 0
           
        in case rest1 of
            ("~~" : "-->>" : rest2) -> 
                let (bodyTokens, rest3) = getBlockTokens "-->>" "<<--" rest2 0
                in ActionWhile (parseTokens condTokens) (parseTokens bodyTokens) : parseTokens rest3
            _ -> parseTokens condTokens ++ parseTokens rest1 

    _ | head tok == '"' -> Push (VStr (init (tail tok))) : parseTokens toks
      | isNumeric tok   -> Push (VInt (read tok)) : parseTokens toks
      | otherwise       -> LoadVar tok : parseTokens toks

parseAether :: String -> [Instruction]
parseAether code = parseTokens (tokenize code)


runAether :: [Instruction] -> Stack -> Memory -> IO ()
runAether [] _ _ = return () 

runAether (Push val : rest) stack mem = runAether rest (val : stack) mem

runAether (SaveVar name : rest) (val : stack) mem = 
    runAether rest stack (Map.insert name val mem)

runAether (LoadVar name : rest) stack mem = 
    case Map.lookup name mem of
        Just (VCode body) -> runAether (body ++ rest) stack mem
        Just val          -> runAether rest (val : stack) mem
        Nothing           -> do
            putStrLn $ "[Aether Error]: Пустота '" ++ name ++ "'"
            runAether rest stack mem

runAether (ActionWeave : rest) (VStr name : VCode body : stack) mem =
    runAether rest stack (Map.insert name (VCode body) mem)

runAether (ActionTell : rest) (val : stack) mem = do
    let output = case val of
            VStr s -> s
            VInt n -> show n
            VBool b -> if b then "Истина" else "Ложь"
            VList l -> show l 
    putStrLn output
    runAether rest stack mem

runAether (ActionAdd : rest) (v2 : v1 : stack) mem = 
    let newVal = case (v1, v2) of
            (VInt a, VInt b) -> VInt (a + b)
            (VStr a, VStr b) -> VStr (a ++ b)
            (VStr a, VInt b) -> VStr (a ++ show b)
            (VInt a, VStr b) -> VStr (show a ++ b)
            _                -> VStr (show v1 ++ show v2)
    in runAether rest (newVal : stack) mem

runAether (ActionSub : rest) (VInt b : VInt a : stack) mem = runAether rest (VInt (a - b) : stack) mem
runAether (ActionMul : rest) (VInt b : VInt a : stack) mem = runAether rest (VInt (a * b) : stack) mem
runAether (ActionDiv : rest) (VInt b : VInt a : stack) mem = runAether rest (VInt (a `div` b) : stack) mem
runAether (ActionGt : rest)  (VInt b : VInt a : stack) mem = runAether rest (VBool (a > b) : stack) mem
runAether (ActionLt : rest)  (VInt b : VInt a : stack) mem = runAether rest (VBool (a < b) : stack) mem
runAether (ActionEq : rest)  (b : a : stack) mem = runAether rest (VBool (a == b) : stack) mem
runAether (ActionNeq : rest) (b : a : stack) mem = runAether rest (VBool (a /= b) : stack) mem
runAether (ActionGe : rest)  (VInt b : VInt a : stack) mem = runAether rest (VBool (a >= b) : stack) mem
runAether (ActionLe : rest)  (VInt b : VInt a : stack) mem = runAether rest (VBool (a <= b) : stack) mem
runAether (ActionAnd : rest) (VBool b : VBool a : stack) mem = runAether rest (VBool (a && b) : stack) mem
runAether (ActionOr : rest)  (VBool b : VBool a : stack) mem = runAether rest (VBool (a || b) : stack) mem
runAether (ActionNot : rest) (VBool a : stack) mem = runAether rest (VBool (not a) : stack) mem

runAether (ActionDup : rest) (v : stack) mem = runAether rest (v : v : stack) mem
runAether (ActionDrop : rest) (_ : stack) mem = runAether rest stack mem
runAether (ActionSwap : rest) (v1 : v2 : stack) mem = runAether rest (v2 : v1 : stack) mem

runAether (ActionIf trueBranch falseBranch : rest) stack mem = 
    case stack of
        (VBool cond : stack') -> 
            if cond 
            then runAether (trueBranch ++ rest) stack' mem
            else runAether (falseBranch ++ rest) stack' mem
        _ -> do
            putStrLn "[Fatal Error]: Ожидалось логическое значение для проверки."
            runAether rest stack mem

runAether (ActionWhile cond body : rest) stack mem = do
   
    let loopInternal = cond ++ [ActionIf (body ++ [ActionWhile cond body]) []]
    runAether (loopInternal ++ rest) stack mem

runAether (ActionRead : rest) stack mem = do
    putStr "> " 
    input <- getLine
    runAether rest (VStr input : stack) mem

runAether (ActionToInt : rest) (VStr s : stack) mem = 
    
    runAether rest (VInt (read s) : stack) mem

runAether (ActionToInt : rest) (VInt i : stack) mem = 
    
    runAether rest (VInt i : stack) mem

runAether (ActionAppend : rest) (v1 : VList lst : stack) mem = 
    runAether rest (VList (lst ++ [v1]) : stack) mem

runAether (ActionAt : rest) (VInt idx : VList lst : stack) mem = 
    if idx >= 0 && idx < length lst
    then runAether rest ((lst !! idx) : stack) mem
    else do
        putStrLn $ "[Fatal Error]: Индекс " ++ show idx ++ " за пределами списка!"
        runAether rest stack mem

runAether (ActionLen : rest) (VList lst : stack) mem = 
    runAether rest (VInt (length lst) : stack) mem

runAether (ActionRemove : rest) (VInt idx : VList lst : stack) mem = 
    if idx >= 0 && idx < length lst
    then 
        let (before, _:after) = splitAt idx lst
        in runAether rest (VList (before ++ after) : stack) mem
    else do
        putStrLn $ "[Fatal Error]: Не могу удалить элемент " ++ show idx ++ ", список слишком мал!"
        runAether rest stack mem

runAether (ActionSet : rest) (newVal : VInt idx : VList lst : stack) mem = 
    if idx >= 0 && idx < length lst
    then 
        let (before, _:after) = splitAt idx lst
        in runAether rest (VList (before ++ (newVal : after)) : stack) mem
    else do
        putStrLn $ "[Fatal Error]: Не могу заменить элемент по индексу " ++ show idx
        runAether rest stack mem

runAether (ActionReadFile : rest) (VStr path : stack) mem = do
    content <- readFile path
    runAether rest (VStr content : stack) mem
    
runAether (ActionEval : rest) (VStr code : stack) mem = 
    let newInstructions = parseAether code
    in runAether (newInstructions ++ rest) stack mem

runAether (ActionArgs : rest) stack mem = do
    allArgs <- getArgs 
 
    let cleanArgs = if null allArgs then [] else drop 1 allArgs
    let vArgs = VList (map VStr cleanArgs)
    runAether rest (vArgs : stack) mem

runAether (ActionWriteFile : rest) (VStr content : VStr path : stack) mem = do
    writeFile path content
    runAether rest stack mem

runAether (ActionAtStr : rest) (VInt idx : VStr s : stack) mem =
    runAether rest (VStr [s !! idx] : stack) mem

runAether (ActionLenStr : rest) (VStr s : stack) mem =
    runAether rest (VInt (length s) : stack) mem

runAether (ActionSlice : rest) (VInt end : VInt start : VStr s : stack) mem =
    runAether rest (VStr (take (end - start) (drop start s)) : stack) mem



runAether (instr : _) stack _ = 
    putStrLn $ "[Fatal Error]: Невозможно схлопнуть " ++ show instr ++ ". Стек: " ++ show stack


main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Использование: runhaskell aether.hs <файл.ath> [аргументы...]"
        else do
            let fileName = head args
            
            content <- readFile fileName
            let instructions = parseAether content
            runAether instructions [] Map.empty