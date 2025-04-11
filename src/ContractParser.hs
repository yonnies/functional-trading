module ContractParser where
    
import ContractsDSL
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Control.Monad.Identity (Identity)

-- Define a language with Haskell-style comment and whitespace handling
def :: Tok.LanguageDef ()
def = emptyDef
  { Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.reservedNames   = ["None", "One", "Give", "And", "Or", "AcquireOn", 
                           "AcquireOnBefore", "Scale", "AcquireWhen", "Konst", 
                           "StockPrice", "GBP", "USD", "EUR", "BGN", "DIS", 
                           "TSLA", "NVDA", "MSFT", "RACE", "AAPL", "BAdd", "BSub", 
                           "BMul", "UNegate", "UAbs", "USignum", "CLT", 
                           "CLE", "CEQ", "CGE", "CGT", "MaxObs", "LiftD",
                           "Lift2D", "Lift2B", "GrainYield"]
  , Tok.reservedOpNames = ["%>", "%>=", "%<", "%<=", "%="]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser def

-- Shortcuts for common parsers with automatic whitespace handling
symbol     = Tok.symbol lexer     -- For parsing symbols with trailing spaces
parens     = Tok.parens lexer     -- Handles "(...)" with automatic spaces
reserved   = Tok.reserved lexer   -- For reserved names (keywords)
reservedOp = Tok.reservedOp lexer -- For reserved operators


parseContract :: String -> Either String Contract
parseContract s = case parse contractParser "" s of
  Left err -> Left $ "Parse error: " ++ show err
  Right c  -> Right c

contractParser :: Parser Contract
contractParser =  
        parens contractParser 
    <|> noneParser
    <|> oneParser
    <|> giveParser
    <|> andParser
    <|> orParser
    <|> acquireOnParser
    <|> acquireOnBeforeParser
    <|> scaleParser
    <|> acquireWhenParser


noneParser :: Parser Contract
noneParser = reserved "None" >> return None

oneParser :: Parser Contract
oneParser = do
  reserved "One"
  currency <- currencyParser
  return $ One currency

giveParser :: Parser Contract
giveParser = do
  reserved "Give"
  c <- contractParser
  return $ Give c

andParser :: Parser Contract
andParser = do
  reserved "And"
  c1 <- contractParser
  c2 <- contractParser
  return $ And c1 c2

orParser :: Parser Contract
orParser = do
  reserved "Or"
  c1 <- contractParser
  c2 <- contractParser
  return $ Or c1 c2

acquireOnParser :: Parser Contract
acquireOnParser = do
  reserved "AcquireOn"
  d <- dateParser
  c <- contractParser
  return $ AcquireOn d c

acquireOnBeforeParser :: Parser Contract
acquireOnBeforeParser = do
  reserved "AcquireOnBefore"
  d <- dateParser
  c <- contractParser
  return $ AcquireOnBefore d c

scaleParser :: Parser Contract
scaleParser = do
  reserved "Scale"
  obs <- obsParser
  c <- contractParser
  return $ Scale obs c

acquireWhenParser :: Parser Contract
acquireWhenParser = do
  reserved "AcquireWhen"
  obs <- obsBoolParser
  c <- contractParser
  return $ AcquireWhen obs c

obsParser :: Parser (Obs Double)
obsParser = buildExpressionParser opTable term

unaryNegation :: Obs Double -> Obs Double
unaryNegation = LiftD UNegate

term :: Parser (Obs Double)
term =  parens obsParser
     <|> try liftDParser
     <|> try lift2DParser
     <|> try maxObsParser
     <|> try konstParser
     <|> try stockPriceParser
     <|> try (unaryNegation <$> (reservedOp "-" *> term)) 

liftDParser :: Parser (Obs Double)
liftDParser = do
  reserved "LiftD"
  op <- unaryOpParser
  o <- term
  return $ LiftD op o

lift2DParser :: Parser (Obs Double)
lift2DParser = do
  reserved "Lift2D"
  op <- binaryOpParser
  o1 <- parens obsParser
  o2 <- parens obsParser
  return $ Lift2D op o1 o2

unaryOpParser :: Parser UnaryOp
unaryOpParser = choice
  [ reserved "UNegate" >> return UNegate
  ]

binaryOpParser :: Parser BinaryOp
binaryOpParser = choice
  [ reserved "BAdd" >> return BAdd
  , reserved "BSub" >> return BSub
  , reserved "BMul" >> return BMul
  ]

-- Operator precedence table (highest to lowest)
opTable :: [[Operator String () Identity (Obs Double)]]
opTable =
  [ [ Prefix (reservedOp "-" >> return unaryNegation) ]  
  , [ Infix (reservedOp "*" >> return (\x y -> Lift2D BMul x y)) AssocLeft ]
  , [ Infix (reservedOp "+" >> return (\x y -> Lift2D BAdd x y)) AssocLeft
    , Infix (reservedOp "-" >> return (\x y -> Lift2D BSub x y)) AssocLeft
    ]
  ]

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity (Obs Double)
binary name op assoc = Infix (reservedOp name >> return (\x y -> Lift2D op x y)) assoc

unary :: String -> (Obs Double -> Obs Double) -> Operator String () Identity (Obs Double)
unary name f = Prefix (reservedOp name >> return f)

maxObsParser :: Parser (Obs Double)
maxObsParser = do
  reserved "MaxObs"
  o1 <- obsParser
  o2 <- obsParser
  return $ MaxObs o1 o2


konstParser :: Parser (Obs Double)
konstParser = do
  reserved "Konst"
  value <- parens signedValue <|> signedValue
  return $ Konst value
  where
    signedValue = do
      sign <- option id (char '-' >> return negate)
      value <- Tok.float lexer <|> try (fromIntegral <$> Tok.integer lexer) 
      return $ sign value

stockPriceParser :: Parser (Obs Double)
stockPriceParser = do
  reserved "StockPrice"
  stock <- stockParser
  return $ StockPrice stock

obsBoolParser :: Parser (Obs Bool)
obsBoolParser = 
        parens obsBoolParser  
    <|> do
        o1 <- obsParser
        op <- comparisonOperatorParser
        o2 <- obsParser
        return $ Lift2B op o1 o2
    <|> do
        reserved "Lift2B"
        op <- comparisonOperatorParser
        o1 <- parens obsParser
        o2 <- parens obsParser
        return $ Lift2B op o1 o2


currencyParser :: Parser Currency
currencyParser = choice $ map (\(s, c) -> reserved s >> return c)
  [ ("GBP", GBP), ("USD", USD), ("EUR", EUR), ("BGN", BGN) ]

stockParser :: Parser Stock
stockParser = choice $ map (\(s, st) -> reserved s >> return st)
  [ ("DIS", DIS), ("TSLA", TSLA), ("NVDA", NVDA)
  , ("MSFT", MSFT), ("RACE", RACE), ("AAPL", AAPL) ]

dateParser :: Parser Date
dateParser = do
  day <- Tok.natural lexer
  _ <- symbol "-"
  month <- Tok.natural lexer
  _ <- symbol "-"
  year <- Tok.natural lexer
  let dateString = pad day ++ "-" ++ pad month ++ "-" ++ show year
  case parseTimeM True defaultTimeLocale "%d-%m-%Y" dateString of
    Just d  -> return d
    Nothing -> do
        let dateString2 = show day ++ "-" ++ pad month ++ "-" ++ pad year
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString2 of 
            Just d -> return d
            Nothing -> fail $ "Invalid date format: " ++ dateString2 ++ ". Expected format is DD-MM-YYYY or YYYY-MM-DD."
  where
    pad n = if n < 10 then "0" ++ show n else show n


comparisonOperatorParser :: Parser CompareOp
comparisonOperatorParser = choice
  [ reservedOp "%>"  >> return CGT
  , reservedOp "%>=" >> return CGE
  , reservedOp "%<"  >> return CLT
  , reservedOp "%<=" >> return CLE
  , reservedOp "%="  >> return CEQ
  , reserved "CLT"   >> return CLT
  , reserved "CLE"   >> return CLE
  , reserved "CEQ"   >> return CEQ
  , reserved "CGE"   >> return CGE
  , reserved "CGT"   >> return CGT
  ]

