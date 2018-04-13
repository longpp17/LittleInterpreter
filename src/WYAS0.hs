import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
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

instance Show LispVal where
  show = renderVal
             
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
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

{-readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: `" ++ (show val) ++"'"-}

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> Str $ "No match: " ++ show err
    Right val -> val

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

eval :: LispVal -> LispVal
eval val@(Str _)    = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("%", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (Str n)    = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
--main = do 
--         (expr:_) <- getArgs
--         putStrLn (readExpr expr)
main = getArgs >>= print . eval . readExpr . head
