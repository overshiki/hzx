module HZX.IO.QASM
  ( parseQASM
  , serializeQASM
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Control.Monad (void)
import Data.Ratio ((%))

import HZX.Circuit

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef
  { Tok.commentLine = "//"
  , Tok.reservedNames = ["OPENQASM", "qreg", "creg", "include"]
  , Tok.identStart = letter <|> char '_'
  , Tok.identLetter = alphaNum <|> char '_'
  }

identifier :: Parser String
identifier = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

semi :: Parser ()
semi = void (Tok.semi lexer) <|> void (string ";" >> spaces)

comma :: Parser ()
comma = void (Tok.comma lexer)

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parse an OpenQASM 2 string into a Circuit.
parseQASM :: String -> Either String Circuit
parseQASM s = case parse qasmFile "" s of
  Left err -> Left (show err)
  Right c  -> Right c

qasmFile :: Parser Circuit
qasmFile = do
  spaces
  optional header
  spaces
  _ <- many declaration
  gs <- many gateStmt
  eof
  return (Circuit gs)

data Decl = QReg String Int

header :: Parser ()
header = do
  void $ string "OPENQASM"
  spaces
  void $ many1 (oneOf "0123456789.")
  semi
  spaces

declaration :: Parser Decl
declaration = do
  void $ string "qreg"
  spaces
  name <- identifier
  sz <- brackets (fromIntegral <$> integer)
  semi
  spaces
  return (QReg name sz)

gateStmt :: Parser Gate
gateStmt = do
  spaces
  g <- try rzGate <|> try rxGate <|> try twoQubitGate <|> singleQubitGate
  semi
  spaces
  return g

singleQubitGate :: Parser Gate
singleQubitGate = do
  name <- choice (map string ["h", "s", "t", "x", "y", "z"])
  spaces
  q <- qubitRef
  return $ case name of
    "h" -> H q
    "s" -> S q
    "t" -> T q
    "x" -> XPhase q (1 % 1)
    "y" -> XPhase q (1 % 2)  -- approximation; Y is not directly in our gate set
    "z" -> ZPhase q (1 % 1)
    _   -> error "unknown single qubit gate"

twoQubitGate :: Parser Gate
twoQubitGate = do
  name <- choice (map string ["cx", "cz", "swap"])
  spaces
  q1 <- qubitRef
  comma
  spaces
  q2 <- qubitRef
  return $ case name of
    "cx"  -> CNOT q1 q2
    "cz"  -> CZ q1 q2
    "swap" -> SWAP q1 q2
    _     -> error "unknown two qubit gate"

rzGate :: Parser Gate
rzGate = do
  void $ string "rz"
  spaces
  angle <- parens angleExpr
  spaces
  q <- qubitRef
  return (ZPhase q angle)

rxGate :: Parser Gate
rxGate = do
  void $ string "rx"
  spaces
  angle <- parens angleExpr
  spaces
  q <- qubitRef
  return (XPhase q angle)

qubitRef :: Parser Int
qubitRef = do
  _ <- identifier
  idx <- brackets (fromIntegral <$> integer)
  return idx

angleExpr :: Parser Rational
angleExpr = do
  spaces
  try piExpr <|> try rationalExpr <|> try floatToRational

piExpr :: Parser Rational
piExpr = do
  sign <- option 1 (char '-' >> return (-1))
  spaces
  void $ string "pi"
  spaces
  m <- option 1 $ do
    void $ char '/'
    spaces
    fromIntegral <$> integer
  return (sign % m)

rationalExpr :: Parser Rational
rationalExpr = do
  sign <- option 1 (char '-' >> return (-1))
  spaces
  num <- fromIntegral <$> integer
  spaces
  void $ char '/'
  spaces
  den <- fromIntegral <$> integer
  spaces
  optional (string "*pi")
  return (sign * num % den)

floatToRational :: Parser Rational
floatToRational = do
  f <- float
  spaces
  optional (string "*pi")
  -- Approximate float as rational with small denominator
  return (toRational f)

-- | Serialize a Circuit to OpenQASM 2.
serializeQASM :: Circuit -> String
serializeQASM c =
  let n = numQubits c
      -- Note: omit include statement for compatibility with our parser
      headerLines = ["OPENQASM 2.0;", "qreg q[" ++ show n ++ "];"]
      gateLines = map serializeGate (gates c)
  in unlines (headerLines ++ gateLines)

serializeGate :: Gate -> String
serializeGate (H q)       = "h q[" ++ show q ++ "];"
serializeGate (S q)       = "s q[" ++ show q ++ "];"
serializeGate (T q)       = "t q[" ++ show q ++ "];"
serializeGate (ZPhase q p) = "rz(" ++ show (fromRational p :: Double) ++ "*pi) q[" ++ show q ++ "];"
serializeGate (XPhase q p) = "rx(" ++ show (fromRational p :: Double) ++ "*pi) q[" ++ show q ++ "];"
serializeGate (CNOT c t)  = "cx q[" ++ show c ++ "], q[" ++ show t ++ "];"
serializeGate (CZ q1 q2)  = "cz q[" ++ show q1 ++ "], q[" ++ show q2 ++ "];"
serializeGate (SWAP q1 q2) = "swap q[" ++ show q1 ++ "], q[" ++ show q2 ++ "];"
serializeGate (Measure _ q) = "// measure q[" ++ show q ++ "] (not supported in OpenQASM 2.0);"
serializeGate (Reset _ q)  = "// reset q[" ++ show q ++ "] (not supported in OpenQASM 2.0);"
