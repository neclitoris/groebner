{-# LANGUAGE PatternSynonyms #-}
module Syntax where

import Data.Bifunctor
import Data.HashSet qualified as HS
import Data.List
import Data.Void

import Control.Applicative (empty)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Text.Megaparsec qualified as MPC
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC hiding (space)
import Text.Megaparsec.Char.Lexer qualified as MPC
import Text.Megaparsec.Error qualified as MPC

import Poly.Polynomial
import Poly.Monomial.Order
import Poly.Variables


data Expr
  = Const Double
  | Var String
  | Binop (forall f v o . (PolynomialConstraint (Polynomial f v o))
                      => Polynomial f v o
                      -> Polynomial f v o
                      -> Polynomial f v o) Expr Expr
  | Pow Expr Int
  | Neg Expr
  | App Expr [Expr]

add lhs rhs = Binop (+) lhs rhs
sub lhs rhs = Binop (-) lhs rhs
mul lhs rhs = Binop (*) lhs rhs

data Stmt
  = Assign String [String] Expr
  | Eval Expr

data Action where
  Quit :: Action
  ShowHelp :: Action
  SwitchOrder :: forall o. MonomialOrder o => o -> Action

data Command
  = Run Stmt
  | Do Action


type Parser = MPC.Parsec Void String

stmt :: Parser Stmt
stmt = MPC.label "statement" $ assign <|> eval
  where
    assign = MPC.try $ do
      (n, a) <- app name name
      symbol "="
      e <- expr
      MPC.eof
      return (Assign n a e)
    eval = do
      e <- expr
      MPC.eof
      return (Eval e)

expr :: Parser Expr
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
  args <- parens (a `sepBy1` (symbol ",") <?> "argument list") <?> "application"
  return (n, args)

atom :: Parser Expr
atom = MPC.label "atom" $ MPC.choice
  [ parens expr
  , Var <$> name
  ]

term :: Parser Expr
term = MPC.label "term" $ MPC.choice
  [ Const <$> number
  , uncurry App <$> app atom expr
  , MPC.try $ Pow <$> atom <*> (symbol "^" *> int)
  , atom
  ]

symbol = MPC.symbol space
shortSymbol s = MPC.choice $ map (MPC.try . symbol) $ (reverse $ tail $ inits s)
name = (:) <$> MPC.letterChar <*> many MPC.alphaNumChar <* space <?> "name"
int = MPC.decimal <* space <?> "int"
number = MPC.choice
  [ MPC.try MPC.float
  , fromIntegral <$> int
  ] <* space <?> "number"
space = MPC.space MPC.space1 empty empty
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

command = MPC.try $ Run <$> stmt <|>
  MPC.string ":" *> MPC.choice
  [ Do Quit <$ shortSymbol "quit"
  , Do ShowHelp <$ shortSymbol "help"
  , (\a -> Do a) <$> (shortSymbol "order" *>
    MPC.choice
      [ MPC.string' "Lex" *> pure (SwitchOrder Lex)
      , MPC.string' "RevLex" *> pure (SwitchOrder RevLex)
      , MPC.string' "DegLex" *> pure (SwitchOrder DegLex)
      , MPC.string' "DegRevLex" *> pure (SwitchOrder DegRevLex)
      ])
  , pure (Do ShowHelp)]
parseStmt = bimap MPC.errorBundlePretty id . MPC.runParser stmt ""
parseCommand = bimap MPC.errorBundlePretty id . MPC.runParser command ""


freeVars :: Expr -> HS.HashSet String
freeVars (Const _)  = HS.empty
freeVars (Var s)    = HS.singleton s
freeVars (App a bs) = HS.unions (freeVars a : map freeVars bs)
freeVars (Binop _ a b) = HS.union (freeVars a) (freeVars b)
freeVars (Neg a)    = freeVars a
freeVars (Pow a _)  = freeVars a
