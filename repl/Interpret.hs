module Interpret where

import Control.Monad

import System.Console.Haskeline

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

data SomePolys f o = forall v . SingI v => SomePolys [Polynomial f v o]

weakenVals :: forall f o . MonomialOrder o => [Value f o] -> SomePolys f o
weakenVals vals =
  case un of SomeSing (sun :: Sing u) ->
              withSingI sun $ SomePolys $ map (wkn sun) vals
  where
    getSing (Value (p :: Polynomial f v o)) = SomeSing (sing @v)
    sings = map getSing vals
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

applyList :: forall f o r . (Fractional f, Show f, MonomialOrder o)
          => (forall v . SingI v => [Polynomial f v o] -> r) -> [Value f o] -> r
applyList f l = case weakenVals l of SomePolys ps -> f ps

interpretExpr :: forall f o r . (Eq f, Fractional f, Show f
                         , MonomialOrder o, Members '[State Ctx, Error String, Fail] r)
              => FieldType f -> Expr f -> Sem r (Value f o)
interpretExpr f (Const v) =
  pure $ Value (toPolynomial v :: Polynomial f '[] o)
interpretExpr f (Var v)   = do
  Ctx f' ord ctx <- get
  Just Refl <- pure $ testEquality f f'
  case M.lookup v ctx of
    Nothing       -> throw $ "Undefined variable: " <> v
    Just (Value w) -> pure $ withOrder order $ Value w
interpretExpr f (App a as) = do
  Ctx f' ord ctx <- get
  Just Refl <- pure $ testEquality f f'
  exps <- mapM (interpretExpr @f @o f) as
  Value (p :: Polynomial f v o) <- interpretExpr f a
  SomePolys (V.fromList -> VN.SomeSized (v :: VN.Vector n (Polynomial f v1 o)))
    <- pure $ weakenVals exps
  case sing @n %~ sLength (sing @v) of
    Proved Refl -> pure $ Value $ lift p @. v
    Disproved _ -> throw "Invalid number of arguments in variable substitution"
interpretExpr f (Binop op e1 e2) = do
  lhs <- interpretExpr f e1
  rhs <- interpretExpr f e2
  applyList (\[x, y] -> pure $ Value $ x `op` y) [lhs, rhs]
interpretExpr f (Neg e) = do
  Value p <- interpretExpr f e
  pure $ Value (-p)
interpretExpr f (Pow e n) = do
  Value lhs <- interpretExpr f e
  pure $ Value (lhs^n)

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
  pure Nothing
interpretStmt f (AppBuiltin "S" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  case weakenVals res of
    SomePolys [x, y] -> pure $ Just $ show $ sPolynomial x y
    _                -> throw "Error: 'S' only accepts two arguments"
interpretStmt f (AppBuiltin "GCD" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  case weakenVals res of
    SomePolys [x, y] -> pure $ Just $ show $ gcd x y
    _                -> throw "Error: 'GCD' only accepts two arguments"
interpretStmt f (AppBuiltin "GroebnerBasis" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  pure $ applyList (Just . show . groebnerBasis) res
interpretStmt f (AppBuiltin "AutoReduce" exps) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  res <- mapM (interpretExpr @f @o f) exps
  put st
  pure $ applyList (Just . show . autoReduce) res
interpretStmt f (Eval expr) = do
  st@(Ctx f' (o :: o) _) <- get
  Just Refl <- pure $ testEquality f f'
  let fv = S.toList $ freeVars expr
  let upd = withVariables @f (map T.pack fv) (M.fromList . zip fv . map Value)
  modify (updateCtxNoShadow f upd)
  Value res <- interpretExpr @f @o f expr
  put st
  pure $ Just $ show res
