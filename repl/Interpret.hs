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
import Data.Type.Equality
import Data.Singletons
import Data.Singletons.Decide

import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State

import Poly.Algorithms
import Poly.Monomial.Order
import Poly.Point
import Poly.Polynomial

import Ctx
import Syntax

import Prelude hiding (gcd)

-- data Expr
  -- = Const Double
  -- | Var String
  -- | Add Expr Expr
  -- | Sub Expr Expr
  -- | Mul Expr Expr
  -- | Neg Expr
  -- | App Expr [Expr]
  -- deriving (Show)

applyList :: forall f o r . (Fractional f, Show f, MonomialOrder o)
          => (forall v . SingI v => [Polynomial f v o] -> r) -> [Value f o] -> r
applyList f l = case un of SomeSing sun -> withSingI sun $ f (map (wkn sun) l)
  where
    getSing (Value (p :: Polynomial f v o)) = SomeSing (sing @v)
    sings = map getSing l
    un = foldr (\(SomeSing v1) (SomeSing v2) -> SomeSing (v1 `sUnion` v2))
               (SomeSing (sing @'[]))
               sings
    wkn :: forall (v' :: Vars) o . MonomialOrder o
        => Sing v'
        -> Value f o
        -> Polynomial f v' o
    wkn sun (Value (p :: Polynomial f v o)) =
      case sing @v of
        s -> fromJust $ ifSubset s sun $ withSingI sun $ weaken @v' p

interpretExpr :: forall f o r . (Eq f, Fractional f, Show f
                         , MonomialOrder o, Members '[State Ctx, Error String, Fail] r)
              => FieldType f -> Expr f -> Sem r (Value f o)
interpretExpr f (Const v) =
  return $ Value (toPolynomial v :: Polynomial f '[] o)
interpretExpr f (Var v)   = do
  Ctx f' ord ctx <- get
  Just Refl <- pure $ testEquality f f'
  case M.lookup v ctx of
    Nothing       -> throw $ "Undefined variable: " <> v
    Just (Value w) -> return $ withOrder order $ Value w
interpretExpr f (App a as) = do
  Ctx f' ord ctx <- get
  Just Refl <- pure $ testEquality f f'
  exps <- mapM (interpretExpr @f @o f) as
  val <- interpretExpr f a
  case val of
    Value (p :: Polynomial f v o) -> do
      applyList
        (\case
          (V.fromList ->
            VN.SomeSized (v :: VN.Vector n (Polynomial f v1 o))) ->
              case sing @n %~ sLength (sing @v) of
                Proved Refl -> return $ Value $ lift p @. v
                Disproved _ ->
                  throw "Invalid number of arguments in variable substitution"
        )
        exps
interpretExpr f (Binop op e1 e2) = do
  lhs <- interpretExpr f e1
  rhs <- interpretExpr f e2
  applyList (\[x, y] -> return $ Value $ x `op` y) [lhs, rhs]
interpretExpr f (Neg e) = do
  Value p <- interpretExpr f e
  return $ Value (-p)
interpretExpr f (Pow e n) = do
  Value lhs <- interpretExpr f e
  return $ Value (lhs^n)

interpretStmt :: forall f r . (Eq f, Fractional f, Show f
                 , Members '[State Ctx, Error String, Fail] r)
              => FieldType f -> Stmt f -> Sem r (Maybe String)
interpretStmt f (Assign name args expr) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  when (nub args /= args) $ throw "Variable names must be distinct"
  let upd = withVariables @f (map T.pack args)
              (M.fromList . zip args . map Value)
  modify (updateCtx f upd)
  res <- interpretExpr @f @o f expr
  put st
  modify (updateCtx f (M.singleton name res))
  return Nothing
interpretStmt f (AppBuiltin "S" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  return $ applyList
             (\case
               [x, y] -> Just $ show $ sPolynomial x y
               _ -> Just "Error: 'S' only accepts two arguments"
             )
             res
interpretStmt f (AppBuiltin "GCD" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  return $ applyList
             (\case
               [x, y] -> Just $ show $ gcd x y
               _ -> Just "Error: 'GCD' only accepts two arguments"
             )
             res
interpretStmt f (AppBuiltin "GroebnerBasis" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  return $ applyList (Just . show . groebnerBasis) res
interpretStmt f (AppBuiltin "AutoReduce" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  return $ applyList (Just . show . autoReduce) res
interpretStmt f (Eval expr) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  let fv = S.toList $ freeVars expr
  let upd = withVariables @f (map T.pack fv) (M.fromList . zip fv . map Value)
  modify (updateCtxNoShadow f upd)
  Value res <- interpretExpr @f @o f expr
  put st
  return $ Just $ show res
