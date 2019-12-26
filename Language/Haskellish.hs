module Language.Haskellish where

import Language.Haskell.Exts
import Control.Applicative
import Control.Monad
import Data.Either (isRight)
import Data.Maybe (catMaybes)


data Haskellish a = Haskellish { runHaskellish :: Exp SrcSpanInfo -> Either String a }


instance Functor Haskellish where
  fmap f x = Haskellish (fmap f . runHaskellish x)


instance Applicative Haskellish where
  pure x = Haskellish (const (Right x))
  f <*> x = Haskellish (\e -> do -- ie. in (Either String) monad
    (e1,e2) <- applicationExpressions e
    f' <- runHaskellish f e1
    x' <- runHaskellish x e2
    return (f' x')
    )


applicationExpressions :: Exp SrcSpanInfo -> Either String (Exp SrcSpanInfo,Exp SrcSpanInfo)
applicationExpressions (Paren _ x) = applicationExpressions x
applicationExpressions (App _ e1 e2) = Right (e1,e2)
applicationExpressions (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = Right (e1,e2)
applicationExpressions (InfixApp l e1 (QVarOp _ (UnQual _ (Symbol _ x))) e2) = Right (App l x' e1,e2)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions (LeftSection l e1 (QVarOp _ (UnQual _ (Symbol _ x)))) = Right (x',e1)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions _ = Left ""


instance Alternative Haskellish where
  empty = Haskellish (const (Left ""))
  a <|> b = Haskellish (\e -> do
    let a' = runHaskellish a e
    if isRight a' then a' else runHaskellish b e
    )


instance Monad Haskellish where
  x >>= f = Haskellish (\e -> do
    x' <- runHaskellish x e
    runHaskellish (f x') e
    )

instance MonadPlus Haskellish where
  mzero = empty
  mplus = (<|>)


identifier :: Haskellish String -- note: we don't distinguish between identifiers and symbols
identifier = Haskellish f
  where f (Paren _ x) = f x
        f (Var _ (UnQual _ (Ident _ x))) = Right x
        f (Var _ (UnQual _ (Symbol _ x))) = Right x
        f _ = Left ""

reserved :: String -> Haskellish ()
reserved x = Haskellish (\e -> do -- in (Either String)
   e' <- runHaskellish identifier e
   if e' == x then Right () else Left ""
   )

string :: Haskellish String
string = Haskellish f
  where f (Paren _ x) = f x
        f (Lit _ (String _ x _)) = Right x
        f _ = Left ""

integer :: Haskellish Integer
integer = Haskellish f
  where f (Paren _ x) = f x
        f (NegApp _ (Lit _ (Int _ x _))) = Right (x * (-1))
        f (Lit _ (Int _ x _)) = Right x
        f _ = Left ""

rational :: Haskellish Rational
rational = Haskellish f
  where f (Paren _ x) = f x
        f (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1))
        f (Lit _ (Frac _ x _)) = Right x
        f _ = Left ""

rationalOrInteger :: Haskellish Rational
rationalOrInteger = Haskellish f
  where f (Paren _ x) = f x
        f (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1))
        f (Lit _ (Frac _ x _)) = Right x
        f (NegApp _ (Lit _ (Int _ x _))) = Right (fromIntegral $ x * (-1))
        f (Lit _ (Int _ x _)) = Right $ fromIntegral x
        f _ = Left ""

list :: Haskellish a -> Haskellish [a]
list p = Haskellish (\e -> f e >>= mapM (runHaskellish p))
  where
    f (Paren _ x) = f x
    f (List _ xs) = Right xs
    f _ = Left ""

tuple :: Haskellish a -> Haskellish b -> Haskellish (a,b)
tuple p1 p2 = Haskellish (\e -> do
  (a,b) <- f e
  a' <- runHaskellish p1 a
  b' <- runHaskellish p2 b
  return (a',b')
  )
  where
    f (Paren _ x) = f x
    f (Tuple _ Boxed (a:b:[])) = Right (a,b)
    f _ = Left ""

asRightSection :: Haskellish (a -> b -> c) -> Haskellish b -> Haskellish (a -> c)
asRightSection opP bP = Haskellish (\e -> do
  (opExp,bExp) <- f e
  op' <- runHaskellish opP opExp
  b <- runHaskellish bP bExp
  return $ flip op' b
  )
  where
    f (Paren _ x) = f x
    f (RightSection _ (QVarOp l (UnQual _ (Symbol _ x))) e1) = Right (g l x,e1)
    f _ = Left ""
    g l x = (Var l (UnQual l (Ident l x)))


collectDoStatements :: Exp SrcSpanInfo -> [Exp SrcSpanInfo]
collectDoStatements (Do _ xs) = catMaybes $ fmap f xs
  where
    f (Qualifier _ e) = Just e
    f _ = Nothing
collectDoStatements _ = []

listOfDoStatements :: Haskellish a -> Haskellish [a]
listOfDoStatements p = Haskellish (\e -> mapM (runHaskellish p) $ collectDoStatements e)

type Span = ((Int,Int),(Int,Int))

askSpan :: Haskellish Span
askSpan = Haskellish $ return . expToSpan

expToSpan :: Exp SrcSpanInfo -> Span
expToSpan (Var x _) = srcSpanInfoToSpan x
expToSpan (Paren x _) = srcSpanInfoToSpan x
expToSpan (App x _ _) = srcSpanInfoToSpan x
expToSpan (InfixApp x _ _ _) = srcSpanInfoToSpan x
expToSpan (LeftSection x _ _) = srcSpanInfoToSpan x
expToSpan (NegApp x _) = srcSpanInfoToSpan x
expToSpan (Lit x _) = srcSpanInfoToSpan x
expToSpan (List x _) = srcSpanInfoToSpan x
expToSpan (RightSection x _ _) = srcSpanInfoToSpan x
expToSpan (Tuple x _ _) = srcSpanInfoToSpan x
expToSpan (Do x _) = srcSpanInfoToSpan x
expToSpan _ = ((0,0),(0,0))

srcSpanInfoToSpan :: SrcSpanInfo -> Span
srcSpanInfoToSpan x = ((bx,by),(ex,ey))
  where
    bx = srcSpanStartColumn $ srcInfoSpan x
    by = srcSpanStartLine $ srcInfoSpan x
    ex = srcSpanEndColumn $ srcInfoSpan x
    ey = srcSpanEndLine $ srcInfoSpan x

reverseApplication :: Haskellish a -> Haskellish (a -> b) -> Haskellish b
reverseApplication x f = Haskellish (\e -> do
  (e1,e2) <- applicationExpressions e
  x' <- runHaskellish x e1
  f' <- runHaskellish f e2
  return (f' x')
  )
