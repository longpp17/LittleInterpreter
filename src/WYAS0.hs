{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Typeable
import Data.Char (isNumber, isSymbol)
import Control.Monad.Except

{-|

Adapted from the wikibook https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

Corresponds to the progress made by the end of Chapter 3, Evaluation Part 1.

-}
data LispVal = Atom   String
             | List   [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Str    String
             | Bool   Bool
             deriving (Eq)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where
  show = renderVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ Str x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         "else" -> Bool True
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) $ many1 digit

{-readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: `" ++ (show val) ++"'"-}

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

renderVal :: LispVal -> String
renderVal (Str contents)         = "\"" ++ contents ++ "\""
renderVal (Atom name)            = name
renderVal (Number contents)      = show contents
renderVal (Bool True)            = "#t"
renderVal (Bool False)           = "#f"
renderVal (List contents)        = "(" ++ unwordsList contents ++ ")"
renderVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ renderVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map renderVal

eval :: LispVal -> ThrowsError LispVal
eval val@(Str _)    = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise  -> eval conseq
eval (List (Atom "cond" : clauses)) = evalClauses clauses
    where 
        evalClauses (List [pred, conseq] : xs) = do
            result <- eval pred
            case result of
                Bool False -> evalClauses xs
                Bool True  -> eval conseq
        evalClauses [] = throwError $ BadSpecialForm "No true clause in cond expression: " (List clauses)        
        evalClauses _ = throwError $ BadSpecialForm "malformed cond expression" $ List (Atom "cond" : clauses)   
eval (List [Atom "cond" , List [Atom "else", alt]]) = eval alt
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs]  = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]      = return $ DottedList [x1] x2
cons badArgList    = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Str arg1), (Str arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]     = return $ Bool $ (length arg1 == length arg2) &&
                                                      (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err         -> False
                               Right (Bool val) -> val
eqv [_, _]                         = return $ Bool False
eqv badArgList                     = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
      do
          unpacked1 <- unpacker arg1
          unpacked2 <- unpacker arg2
          return $ unpacked1 == unpacked2
      `catchError` const (return False)
     `catchError` const (return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)




apply :: String -> [LispVal] ->ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "unRecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError  LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("%", numericBinop rem),
              ("string?", typeCheckop valueIsString),
              ("number?", typeCheckop valueIsNumber),
              ("symbol?", typeCheckop valueIsSymbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

valueIsString :: LispVal -> Bool
valueIsString (Str _) = True
valueIsString _       = False

valueIsNumber :: LispVal -> Bool
valueIsNumber (Number _) = True
valueIsNumber _          = False

valueIsSymbol :: LispVal -> Bool
valueIsSymbol (Atom _) = True
valueIsSymbol _        = False

typeCheckop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typeCheckop op [x] = return $ Bool $ op x
typeCheckop _ _ = throwError $ NumArgs 1 []

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                      left <- unpacker $ args !! 0
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (Str s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (Str n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ Str n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

main :: IO ()
--main = do 
--         (expr:_) <- getArgs
--         putStrLn (readExpr expr)
main = do
            args <- getArgs
            evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
            putStrLn $ extractValue $ trapError evaled
