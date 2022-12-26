{-# LANGUAGE AllowAmbiguousTypes #-}
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

applyList :: (forall v . SingI v => [Polynomial Double v Lex] -> r) -> [WrappedPolynomial] -> r
applyList f l = case un of SomeSing sun -> withSingI sun $ f (map (wkn sun) l)
  where
    getSing = \(WrappedPolynomial (p :: Polynomial Double v Lex)) -> SomeSing (sing @v)
    sings = map getSing l
    un = foldr (\(SomeSing v1) (SomeSing v2) -> SomeSing (v1 `sUnion` v2))
               (SomeSing (sing @'[]))
               sings
    wkn :: forall (v' :: Vars) . Sing v'
        -> WrappedPolynomial
        -> Polynomial Double v' Lex
    wkn sun (WrappedPolynomial (p :: Polynomial Double v Lex)) =
      case sing @v of
        s -> fromJust $ ifSubset s sun $ withSingI sun $ weaken @v' p

onlyPoly :: Member (Error String) r => String -> RetValue -> Sem r WrappedPolynomial
onlyPoly s (RPoly w) = return w
onlyPoly s _         = throw $ "Expected polynomial argument for " <> s

onlyPolys :: Member (Error String) r => String -> [RetValue] -> Sem r [WrappedPolynomial]
onlyPolys s = mapM (onlyPoly s)

interpretExpr :: Members '[State Ctx, Error String] r => Expr -> Sem r RetValue
interpretExpr (Const v) =
  return $ RPoly (WrappedPolynomial (toPolynomial v :: Polynomial Double '[] Lex))
interpretExpr (Var v)   = do
  ctx <- gets ctxVars
  case M.lookup v ctx of
    Nothing       -> throw $ "Undefined variable: " <> v
    Just (Poly w) -> return $ RPoly w
    Just Builtin  -> return $ RBuiltin v
interpretExpr (App f as) = do
  ctx <- gets ctxVars
  WrappedOrder ord <- gets ctxOrder
  exps <- mapM interpretExpr as
  v <- interpretExpr f
  case v of
    RBuiltin "S" -> do
      exps' <- onlyPolys "S" exps
      applyList
        (\case
          [p1, p2] ->
            return $ RPoly $ WrappedPolynomial $
              withOrder Lex $ sPolynomial (withOrder ord p1) (withOrder ord p2)
          _ -> throw "'S' only accepts 2 arguments"
        )
        exps'
    RBuiltin "GroebnerBasis" -> do
      exps' <- onlyPolys "GroebnerBasis" exps
      return $ RList $ applyList (map WrappedPolynomial . groebnerBasis) exps'
    RBuiltin "Autoreduce" -> do
      exps' <- onlyPolys "Autoreduce" exps
      return $ RList $ applyList (map WrappedPolynomial . autoReduce) exps'
    RPoly (WrappedPolynomial (p :: Polynomial Double v Lex)) -> do
      exps' <- onlyPolys "variable substitution" exps
      applyList
        (\case
          (V.fromList ->
            VN.SomeSized (v :: VN.Vector n (Polynomial Double v1 Lex))) ->
              case sing @n %~ sLength (sing @v) of
                Proved Refl -> return $ RPoly $ WrappedPolynomial $ lift p @. v
                Disproved _ ->
                  throw "Invalid number of arguments in variable substitution"
        )
        exps'
    _ -> throw ("Lists can only exist on top level")
interpretExpr (Binop op e1 e2) = do
  lhs <- interpretExpr e1
  rhs <- interpretExpr e2
  case (lhs, rhs) of
    (RPoly p1, RPoly p2) -> applyList
      (\[p1, p2] -> return $ RPoly $ WrappedPolynomial $ p1 `op` p2)
      [p1, p2]
    _ -> throw "Arithmetic operations are defined for polynomials only"
interpretExpr (Neg e) = do
  p <- interpretExpr e
  case p of
    RPoly p  -> applyList
      (\[p] -> return $ RPoly $ WrappedPolynomial $ (-p))
      [p]
    _ -> throw "Arithmetic operations are defined for polynomials only"
interpretExpr (Pow e n) = do
  lhs <- interpretExpr e
  case lhs of
    RPoly p -> applyList
      (\[p] -> return $ RPoly $ WrappedPolynomial $ p^n)
      [p]
    _ -> throw "Arithmetic operations are defined for polynomials only"

interpretStmt :: Members '[State Ctx, Error String] r
              => Stmt -> Sem r (Maybe RetValue)
interpretStmt (Assign name args expr) = do
  (st :: Ctx) <- get
  when (nub args /= args) $ throw "Variable names must be distinct"
  let upd = withVariables (map T.pack args)
              (\vs -> M.fromList $ zip args (map (Poly . WrappedPolynomial) vs))
  modify (updateCtx upd)
  res <- interpretExpr expr
  put st
  case res of
    RPoly w -> modify (updateCtx (M.singleton name (Poly w)))
    _       -> throw "Can't assign list value to a variable"
  return Nothing
interpretStmt (Eval expr) = do
  (st :: Ctx) <- get
  let fv = S.toList $ freeVars expr
  let upd = withVariables (map T.pack fv)
              (\vs -> M.fromList $ zip fv (map (Poly . WrappedPolynomial) vs))
  modify (updateCtxNoShadow upd)
  res <- interpretExpr expr
  put st
  return (Just res)

