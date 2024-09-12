{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpret where

import Control.Monad

import System.Console.Haskeline

import Data.Bifunctor
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List
import Data.List.Singletons
import Data.Maybe
import Data.Vector qualified as V
import Data.Vector.Sized qualified as VN
import Data.Text qualified as T
import Data.Text (Text)
import Data.Singletons
import Data.Singletons.Decide

import Polysemy
import Polysemy.State
import Polysemy.Error

import Poly.Algorithms
import Poly.Monomial.Order
import Poly.Point
import Poly.Polynomial

import Ctx
import Syntax

-- data Expr
  -- = Const Double
  -- | Var String
  -- | Add Expr Expr
  -- | Sub Expr Expr
  -- | Mul Expr Expr
  -- | Neg Expr
  -- | App Expr [Expr]
  -- deriving (Show)

applyList :: MonomialOrder o
          => (forall v . SingI v => [Polynomial Double v o] -> r) -> [Value o] -> r
applyList f l = case un of SomeSing sun -> withSingI sun $ f (map (wkn sun) l)
  where
    getSing = \(Value (p :: Polynomial Double v o)) -> SomeSing (sing @v)
    sings = map getSing l
    un = foldr (\(SomeSing v1) (SomeSing v2) -> SomeSing (v1 `sUnion` v2))
               (SomeSing (sing @'[]))
               sings
    wkn :: forall (v' :: Vars) o . MonomialOrder o
        => Sing v'
        -> Value o
        -> Polynomial Double v' o
    wkn sun (Value (p :: Polynomial Double v o)) =
      case sing @v of
        s -> fromJust $ ifSubset s sun $ withSingI sun $ weaken @v' p

interpretExpr :: forall o r . (MonomialOrder o, Members '[State Ctx, Error String] r)
              => Expr -> Sem r (Value o)
interpretExpr (Const v) =
  return $ Value (toPolynomial v :: Polynomial Double '[] o)
interpretExpr (Var v)   = do
  Ctx ord ctx <- get
  case M.lookup v ctx of
    Nothing       -> throw $ "Undefined variable: " <> v
    Just (Value w) -> return $ withOrder order $ Value w
interpretExpr (App f as) = do
  Ctx ord ctx <- get
  exps <- mapM (interpretExpr @o) as
  v <- interpretExpr f
  case v of
    Value (p :: Polynomial Double v o) -> do
      applyList
        (\case
          (V.fromList ->
            VN.SomeSized (v :: VN.Vector n (Polynomial Double v1 o))) ->
              case sing @n %~ sLength (sing @v) of
                Proved Refl -> return $ Value $ lift p @. v
                Disproved _ ->
                  throw "Invalid number of arguments in variable substitution"
        )
        exps
interpretExpr (Binop op e1 e2) = do
  lhs <- interpretExpr e1
  rhs <- interpretExpr e2
  applyList (\[x, y] -> return $ Value $ x `op` y) [lhs, rhs]
interpretExpr (Neg e) = do
  Value p <- interpretExpr e
  return $ Value (-p)
interpretExpr (Pow e n) = do
  Value lhs <- interpretExpr e
  return $ Value (lhs^n)

interpretStmt :: Members '[State Ctx, Error String] r
              => Stmt -> Sem r (Maybe String)
interpretStmt (Assign name args expr) = do
  st@(Ctx (o :: o) _) <- get
  when (nub args /= args) $ throw "Variable names must be distinct"
  let upd = withVariables (map T.pack args)
              (\vs -> M.fromList $ zip args (map Value vs))
  modify (updateCtx upd)
  res <- interpretExpr @o expr
  put st
  modify (updateCtx (M.singleton name res))
  return Nothing
interpretStmt (AppBuiltin "S" exps) = do
  st@(Ctx (o :: o) _) <- get
  res <- mapM (interpretExpr @o) exps
  put st
  return $ applyList
             (\case
               [x, y] -> Just $ show $ sPolynomial x y
               _ -> Just "Error: 'S' only accepts two arguments"
             )
             res
interpretStmt (AppBuiltin "GroebnerBasis" exps) = do
  st@(Ctx (o :: o) _) <- get
  res <- mapM (interpretExpr @o) exps
  put st
  return $ applyList
             (\l -> Just $ show $ groebnerBasis l)
             res
interpretStmt (AppBuiltin "AutoReduce" exps) = do
  st@(Ctx (o :: o) _) <- get
  res <- mapM (interpretExpr @o) exps
  put st
  return $ applyList
             (\l -> Just $ show $ autoReduce l)
             res
interpretStmt (Eval expr) = do
  st@(Ctx (o :: o) _) <- get
  let fv = S.toList $ freeVars expr
  let upd = withVariables (map T.pack fv)
              (\vs -> M.fromList $ zip fv (map Value vs))
  modify (updateCtxNoShadow upd)
  Value res <- interpretExpr @o expr
  put st
  return $ Just $ show res
