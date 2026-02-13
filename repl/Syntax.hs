module Syntax where

import Data.Bifunctor
import Data.Functor
import Data.HashSet qualified as HS
import Data.List
import Data.Proxy
import Data.Reflection (give)
import Data.Void

import Control.Applicative (empty)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Text.Megaparsec qualified as MPC
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC hiding (space)
import Text.Megaparsec.Char.Lexer qualified as MPC
import Text.Megaparsec.Error qualified as MPC
import Text.Read (reads)

import Poly.Fields
import Poly.Polynomial
import Poly.Monomial.Order
import Poly.Variables

import Ctx


data Expr f
  = Const f
  | Var String
  | Binop (forall f v o . (PolynomialConstraint (Polynomial f v o))
                      => Polynomial f v o
                      -> Polynomial f v o
                      -> Polynomial f v o) (Expr f) (Expr f)
  | Pow (Expr f) Int
  | Neg (Expr f)
  | App (Expr f) [Expr f]

add = Binop (+)
sub = Binop (-)
mul = Binop (*)

data Stmt f
  = Assign String [String] (Expr f)
  | AppBuiltin String [Expr f]
  | Eval (Expr f)

data Action where
  Quit :: Action
  ShowHelp :: Action
  SwitchOrder :: forall o. MonomialOrder o => o -> Action
  SwitchField :: forall f . (Fractional f, Eq f, Read f, Show f)
              => FieldType f -> Action

data Command
  = forall f . (Fractional f, Show f, Eq f) => Run (FieldType f) (Stmt f)
  | Do Action


type Parser = MPC.Parsec Void String

readMP :: Read a => Parser a
readMP = MPC.label "field coefficient" do
  input  <- MPC.getInput
  offset <- MPC.getOffset
  choice ((\(a, input') -> a <$ MPC.setInput input'
                       <* MPC.setOffset (offset + length input - length input'))
    <$> reads input) <* space

stmt :: Read f => Parser (Stmt f)
stmt = MPC.label "statement" $ assign <|> builtin <|> eval
  where
    assign = MPC.try $ do
      (n, a) <- app name name
      symbol "="
      e <- expr
      MPC.eof
      pure (Assign n a e)
    builtin = MPC.try $ do
      (n, a) <- app (MPC.choice $ map symbol ["S", "GroebnerBasis", "AutoReduce", "GCD"]) expr
      MPC.eof
      pure (AppBuiltin n a)
    eval = do
      e <- expr
      MPC.eof
      pure (Eval e)

expr :: Read f => Parser (Expr f)
expr = MPC.label "expression" $ makeExprParser term
  [ [ Prefix (Neg <$ symbol "-")
    ]
  , [ InfixR (mul <$ symbol "*")
    ]
  , [ InfixR (add <$ symbol "+")
    , InfixR (sub <$ symbol "-")
    ]
  ]

app :: Parser a -> Parser b -> Parser (a, [b])
app p a = MPC.try $ do
  n <- p
  args <- parens (a `sepBy1` symbol "," <?> "argument list") <?> "application"
  pure (n, args)

atom :: Read f => Parser (Expr f)
atom = MPC.label "atom" $ MPC.choice
  [ parens expr
  , Var <$> name
  ]

term :: Read f => Parser (Expr f)
term = MPC.label "term" $ MPC.choice
  [ Const <$> readMP
  , uncurry App <$> app atom expr
  , MPC.try $ Pow <$> atom <*> (symbol "^" *> int)
  , atom
  ]

symbol = MPC.symbol space
shortSymbol s = MPC.choice $ map (MPC.try . symbol) $ reverse (tail $ inits s)
name = (:) <$> MPC.letterChar <*> many MPC.alphaNumChar <* space <?> "name"
int = MPC.decimal <* space <?> "int"
number = MPC.choice
  [ MPC.try MPC.float
  , fromIntegral <$> int
  ] <* space <?> "number"
space = MPC.space MPC.space1 empty empty
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

command :: forall f . (Fractional f, Show f, Read f, Eq f) => FieldType f -> Parser Command
command f = MPC.try $ Run f <$> stmt @f <|>
  MPC.string ":" *> MPC.choice
  [ Do Quit <$ shortSymbol "quit"
  , Do ShowHelp <$ shortSymbol "help"
  , Do <$> (shortSymbol "order" *>
    MPC.choice
      [ MPC.string' "Lex" $> SwitchOrder Lex
      , MPC.string' "RevLex" $> SwitchOrder RevLex
      , MPC.string' "DegLex" $> SwitchOrder DegLex
      , MPC.string' "DegRevLex" $> SwitchOrder DegRevLex
      ])
  , Do <$> (shortSymbol "field" *>
    MPC.choice
      [ MPC.string' "Double" $> SwitchField FDouble
      , MPC.string' "Rational" $> SwitchField FRational
      , do
          i <- MPC.string' "GF " *> int
          Just (SomePrimeW w@(PrimeW @p)) <- pure $ isPrime $ fromIntegral i
          give w $ pure (SwitchField (FGF @p))
      ])
  , pure (Do ShowHelp)]


parseStmt :: Read f => String -> Either String (Stmt f)
parseStmt = first MPC.errorBundlePretty . MPC.runParser (space *> stmt) ""

parseCommand :: forall f . (Fractional f, Show f, Read f, Eq f)
             => FieldType f -> String -> Either String Command
parseCommand f = first MPC.errorBundlePretty . MPC.runParser (space *> command f) ""


freeVars :: Expr f -> HS.HashSet String
freeVars (Const _)  = HS.empty
freeVars (Var s)    = HS.singleton s
freeVars (App a bs) = HS.unions (freeVars a : map freeVars bs)
freeVars (Binop _ a b) = HS.union (freeVars a) (freeVars b)
freeVars (Neg a)    = freeVars a
freeVars (Pow a _)  = freeVars a
