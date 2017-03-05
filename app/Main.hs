module Main where

import Scheme
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import GHC.Real
import Data.Complex

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | Character Char
             | Ratio Rational
             | Complex (Complex Double)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"")
                 char '"'
                 return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
           <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head value

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> oneOf "\\\"nrt"
                  return $ case x of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                 't' -> Bool True
                 'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 $ oneOf "10"
              return $ Number (bin2dig x)

oct2dig x = fst $ head $ readOct x
hex2dig x = fst $ head $ readHex x
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                           bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x++ "." ++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio (read x % read y)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- try parseFloat <|> parseNumber
                  char '+'
                  y <- try parseFloat <|> parseNumber
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do char '`'
                      x <- parseInQuasiQuoted
                      return $ List [Atom "backQuote", x]

parseInQuasiQuoted :: Parser LispVal
parseInQuasiQuoted = try parseUnquoteSpliced
                     <|> try parseUnquoted
                     <|> try parseInQuasiQuotedList
                     <|> parseExpr

parseInQuasiQuotedList :: Parser LispVal
parseInQuasiQuotedList = do char '('
                            x <- List <$> sepBy parseInQuasiQuoted spaces
                            char ')'
                            return x

parseUnquoted :: Parser LispVal
parseUnquoted = do char ','
                   x <- parseExpr
                   return $ List [Atom "unquote", x]

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do string ",@"
                         x <- parseExpr
                         return $ List [Atom "unquote-spliced", x]

parseExpr :: Parser LispVal
parseExpr =     parseAtom
            <|> parseString
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharacter
            <|> try parseQuoted
            <|> try parseQuasiQuoted
            <|> do char '('
                   x <- try parseList <|>  parseDottedList
                   char ')'
                   return x

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Character contents)   = "#\"" ++ show contents
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Float contents)       = show contents
showVal (Ratio (x :% y))       = show x ++ "/" ++ show y
showVal (Complex (r :+ i))     = show r ++ "+" ++ show i ++ "i"
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Character _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

isSymbol :: [LispVal] -> LispVal
isSymbol [] = Bool False
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom x] = String x
symbolToString xs = error $ "ERROR: wrong number of arguments (required 1, got " ++ show (length xs) ++ ")"

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [String x] = Atom x
stringToSymbol xs = error $ "ERROR: wrong number of arguments (required 1, got " ++ show (length xs) ++ ")"

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
