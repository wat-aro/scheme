{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Scheme()
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import GHC.Real
import Data.Char
import Data.Complex
import Data.IORef
import Data.Maybe
import Control.Monad.Except
import System.IO

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~,"

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)
               ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

-- Type

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                     body :: [LispVal],   closure :: Env}
             | Macro {params :: [String], vararg :: Maybe String,
                     body :: [LispVal],   closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Eq LispVal where
  Atom x == Atom x' = x == x'
  Atom _ == _ = False
  List xs == List xs' = xs == xs'
  List _ == _ = False
  DottedList xs x == DottedList xs' x' = xs == xs' && x == x'
  DottedList _ _ == _ = False
  Number n == Number n' = n == n'
  Number _ == _ = False
  Float x == Float x' = x == x'
  Float _ == _ = False
  Bool b == Bool b' = b == b'
  String s == String s' = s == s'
  String _ == _ = False
  Bool _ == _ = False
  Character c == Character c' = c == c'
  Character _ == _ = False
  Ratio r == Ratio r' = r == r'
  Ratio _ == _ = False
  Complex c == Complex c' = c == c'
  Complex _ == _ = False
  PrimitiveFunc _ == _ = False
  Func params vararg body closure == Func params' vararg' body' closure' =
    params == params' && vararg == vararg' && body == body' && closure == closure'
  Func {} == _ = False

-- Parser

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
            <|> try parseQuasiQuoted
            <|> try parseQuoted
            <|> parseString
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharacter
            <|> do char '('
                   x <- try parseList <|>  parseDottedList
                   char ')'
                   return x

-- Show

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Character contents)   = "#\\" ++ show contents
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Float contents)       = show contents
showVal (Ratio (x :% y))       = show x ++ "/" ++ show y
showVal (Complex (r :+ i))     = show r ++ "+" ++ show i ++ "i"
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++ ") ...)"
showVal Macro {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Ratio _) = return val
eval _ val@(Complex _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "backQuote", val]) =
  case val of
    List [Atom "unquote", _] -> evalUnquote val
    List vals -> List <$> mapM evalUnquote vals
    _ -> evalUnquote val
  where
    evalUnquote (List [Atom "unquote", val]) = eval env val
    evalUnquote val = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       _ -> eval env conseq
eval env (List (Atom "cond" : expr : rest)) = eval' expr rest
  where eval' (List [cond, value]) (x : xs) = do
          result <- eval env cond
          case result of
            Bool False -> eval' x xs
            _          -> eval env value
        eval' (List [Atom "else", value]) [] = eval env value
        eval' (List [cond, value]) [] = do
          result <- eval env cond
          case result of
            Bool False -> return $ Atom "#<undef>"
            _          -> eval env value
eval env form@(List (Atom "case":key:clauses)) =
  if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
           List (Atom "else" : exprs) -> fmap last (mapM (eval env) exprs)
           List (List datums : exprs) -> do
             result <- eval env key
             equality <- liftThrows $ mapM (\x -> eqv [result, x]) datums
             if Bool True `elem` equality
               then fmap last (mapM (eval env) exprs)
               else eval env $ List (Atom "case" : key : tail clauses)
           _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var:params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "define-macro" : List (Atom var:params) : body)) =
  makeNormalMacro env params body >>= defineVar env var
eval env (List (Atom "define-macro" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgsMacro varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
-- bindings = [List LispVal]
eval env (List (Atom "let" : List bindings : body)) =
  eval env (List (List (Atom "lambda" : List (fmap fst' bindings) : body) : fmap snd' bindings))
  where fst' :: LispVal -> LispVal
        fst' (List (x:xs)) = x
        snd' :: LispVal -> LispVal
        snd' (List (x:y:xs)) = y
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Macro params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO  (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalMacro
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalMacro env = do
          evaled <- mapM (eval env) body
          last <$> mapM (eval env) evaled
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO  (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSym),
              ("symbol->string", symbolToString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string?", isString),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("string-ref", stringLef),
              ("string->symbol", stringToSymbol),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", strCiBinop (==)),
              ("string-ci<?", strCiBinop (<)),
              ("string-ci>?", strCiBinop (>)),
              ("string-ci<=?", strCiBinop (<=)),
              ("string-ci>=?", strCiBinop (>=)),
              ("substring", subString),
              ("string-append", stringAppend),
              ("string->list", stringList),
              ("list->string", listString),
              ("string-copy", stringCopy),
              ("string-fill!", stringFill),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[args] = throwError $ NumArgs 2 singleVal
numericBinop op params = fmap (Number . foldl1 op) (mapM unpackNum params)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal]  -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

strCiBinop :: (String -> String -> Bool) -> [LispVal]  -> ThrowsError LispVal
strCiBinop op [String x, String y] = return $ Bool $ map toLower x `op` map toLower y
strCiBinop _ [notStr, String y] = throwError $ TypeMismatch "string" notStr
strCiBinop _ [String x, notStr] = throwError $ TypeMismatch "string" notStr
strCiBinop _ argList = throwError $ NumArgs 2 argList

subString :: [LispVal] -> ThrowsError LispVal
subString [String str, Number start, Number end]
  | start <= end = return $ String $ take (fromIntegral (end - start)) $ drop (fromIntegral start) str
  | start > end = throwError $ Otherwise $ "end argument (" ++ show end ++ ") must be greater than or equal to the start argument (" ++ show start ++ ")"
subString [notStr, Number _, Number _] = throwError $ TypeMismatch "string" notStr
subString [String _, notNum, _] = throwError $ TypeMismatch "number" notNum
subString [_, _, notNum] = throwError $ TypeMismatch "number" notNum
subString argList = throwError $ NumArgs 3 argList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [] = return $ String ""
stringAppend args = foldM stringAppend' (String "") args

stringAppend' :: LispVal -> LispVal -> ThrowsError LispVal
stringAppend' (String x) (String y) = return $ String $ x ++ y
stringAppend' (String _) notStr = throwError $ TypeMismatch "string" notStr
stringAppend' notStr _ = throwError $ TypeMismatch "string" notStr

stringList :: [LispVal] -> ThrowsError LispVal
stringList [String s] = return $ List $ fmap Character s
stringList [notStr] = throwError $ TypeMismatch "string" notStr
stringList argList = throwError $ NumArgs 1 argList

listString :: [LispVal] -> ThrowsError LispVal
listString [list@(List xs)] = if all isCharacter xs
                       then return $ String $ fmap (\(Character c) -> c) xs
                       else throwError $ TypeMismatch "character list" list
  where isCharacter (Character _) = True
        isCharacter _ = False
listString argList = throwError $ NumArgs 1 argList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String str] = return $ String $ foldr (:) [] str
stringCopy [notStr] = throwError $ TypeMismatch "string" notStr
stringCopy argList = throwError $ NumArgs 1 argList

stringFill :: [LispVal] -> ThrowsError LispVal
stringFill [String str, Character c] = return $ String $ stringFill' str c
  where stringFill' [] c = []
        stringFill' str c = c:stringFill' (tail str) c
stringFill [String _, notChar] = throwError $ TypeMismatch "character" notChar
stringFill [notStr, _] = throwError $ TypeMismatch "string" notStr
stringFill argList = throwError $ NumArgs 2 argList

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n
                       in if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

isSym :: [LispVal] -> ThrowsError LispVal
isSym [Atom _] = return $ Bool True
isSym xs = case length xs of
             1 -> return $ Bool False
             _ -> throwError $ NumArgs 1 xs

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom x] = return $ String x
symbolToString [notSym] = throwError $ TypeMismatch "symbol" notSym
symbolToString xs = throwError $ NumArgs 1 xs

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String x] = return $ Atom x
stringToSymbol [notString] = throwError $ TypeMismatch "string" notString
stringToSymbol xs = throwError $ NumArgs 1 xs

isString :: [LispVal]  -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString badArgList = throwError $ NumArgs 1 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number n] = makeString [Number n, Character ' ']
makeString [notNumber] = throwError $ TypeMismatch "number" notNumber

makeString [Number n, Character c] = return $ String $ replicate (fromIntegral n) c
makeString [notNumber, Character _] = throwError $ TypeMismatch "number" notNumber
makeString [Number _, notChar] = throwError $ TypeMismatch "char" notChar
makeString [notNumber, _] = throwError $ TypeMismatch "number" notNumber
makeString badArgList = throwError $ NumArgs 2 badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ fromIntegral . length $ s
stringLength [notString] = throwError $ TypeMismatch "string" notString
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringLef :: [LispVal] -> ThrowsError LispVal
stringLef [String s, Number n] = return $ Character $ s !! fromIntegral n
stringLef [notString, Number _] = throwError $ TypeMismatch "string" notString
stringLef [String _, notNumber] = throwError $ TypeMismatch "number" notNumber
stringLef [notString, _] = throwError $ TypeMismatch "string" notString
stringLef badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] =return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Left _ -> False
          Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] = return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
          Left _ -> False
          Right (Bool val) -> val

-- Error

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Otherwise String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args: found valued " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Otherwise message) = message

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

-- Env

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runExceptT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
                         (liftIO . readIORef)
                         (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
                               (liftIO . flip writeIORef value)
                               (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

makeMacro varargs env params body = return $ Macro (map showVal params) varargs body env
makeNormalMacro = makeMacro Nothing
makeVarArgsMacro = makeMacro . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
                                ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

main :: IO ()
main = do args <- getArgs
          if null args
            then runRepl
            else runOne args
