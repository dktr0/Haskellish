{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Haskellish where

import Language.Haskell.Exts as Exts
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Either (isRight)
import Data.Maybe (catMaybes)


data Haskellish st a = Haskellish { runHaskellish :: st -> Exp SrcSpanInfo -> Either String (a,st) }

exp :: Haskellish st (Exp SrcSpanInfo)
exp = Haskellish (\st e -> return (e,st))

haskellishError :: String -> Haskellish st a
haskellishError x = Haskellish (\_ _ -> Left x)

instance Functor (Haskellish st) where
  fmap f x = Haskellish (\st e -> do
    (x',st') <- runHaskellish x st e
    Right (f x',st')
    )


instance Applicative (Haskellish st) where
  pure x = Haskellish (\st _ -> Right (x,st))
  f <*> x = Haskellish (\st e -> do
    (e1,e2) <- applicationExpressions e
    (f',st') <- runHaskellish f st e1
    (x',st'') <- runHaskellish x st' e2
    Right (f' x',st'')
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


instance Alternative (Haskellish st) where
  empty = Haskellish (\_ _ -> Left "")
  a <|> b = Haskellish (\st e -> do
    let a' = runHaskellish a st e
    if isRight a' then a' else runHaskellish b st e
    )


instance Monad (Haskellish st) where
  x >>= f = Haskellish (\st e -> do
    (x',st') <- runHaskellish x st e
    runHaskellish (f x') st' e
    )


instance MonadPlus (Haskellish st) where
  mzero = empty
  mplus = (<|>)


instance MonadState st (Haskellish st) where
  get = Haskellish (\st _ -> return (st,st))
  put st = Haskellish (\_ _ -> return ((),st))


instance MonadError String (Haskellish st) where
  throwError x = Haskellish (\_ _ -> Left x)
  catchError x f = Haskellish (\st e -> do
    let x' = runHaskellish x st e
    case x' of
      Left err -> runHaskellish (f err) st e
      Right (x'',st') -> Right (x'',st')
    )


identifier :: Haskellish st String -- note: we don't distinguish between identifiers and symbols
identifier = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (Var _ (UnQual _ (Ident _ x))) = Right (x,st)
        f st (Var _ (UnQual _ (Symbol _ x))) = Right (x,st)
        f _ _ = Left ""

reserved :: String -> Haskellish st ()
reserved x = Haskellish (\st e -> do
   (e',_) <- runHaskellish identifier st e
   if e' == x then Right ((),st) else Left ""
   )

string :: Haskellish st String
string = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (Lit _ (String _ x _)) = Right (x,st)
        f _ _ = Left ""

integer :: Haskellish st Integer
integer = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (NegApp _ (Lit _ (Int _ x _))) = Right (x * (-1),st)
        f st (Lit _ (Int _ x _)) = Right (x,st)
        f _ _ = Left ""

rational :: Haskellish st Rational
rational = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1),st)
        f st (Lit _ (Frac _ x _)) = Right (x,st)
        f _ _ = Left ""

rationalOrInteger :: Haskellish st Rational
rationalOrInteger = rational <|> fromIntegral <$> integer

list :: Haskellish st a -> Haskellish st [a]
list p = Haskellish (\st e -> do
  xs <- listExpressions e
  foldM f ([],st) xs
  )
  where
    f (ys,st) x = do
      (y,st') <- runHaskellish p st x
      return (ys ++ [y],st')

listExpressions :: Exp SrcSpanInfo -> Either String [Exp SrcSpanInfo]
listExpressions (Paren _ x) = listExpressions x
listExpressions (List _ xs) = Right xs
listExpressions _ = Left ""

tuple :: Haskellish st a -> Haskellish st b -> Haskellish st (a,b)
tuple p1 p2 = Haskellish (\st e -> do
  (a,b) <- f e
  (a',st') <- runHaskellish p1 st a
  (b',st'') <- runHaskellish p2 st' b
  return ((a',b'),st'')
  )
  where
    f (Paren _ x) = f x
    f (Tuple _ Boxed (a:b:[])) = Right (a,b)
    f _ = Left ""

asRightSection :: Haskellish st (a -> b -> c) -> Haskellish st b -> Haskellish st (a -> c)
asRightSection opP bP = Haskellish (\st e -> do
  (opExp,bExp) <- f e
  (op',st') <- runHaskellish opP st opExp
  (b,st'') <- runHaskellish bP st' bExp
  return (flip op' b,st'')
  )
  where
    f (Paren _ x) = f x
    f (RightSection _ (QVarOp l (UnQual _ (Symbol _ x))) e1) = Right (g l x,e1)
    f _ = Left ""
    g l x = (Var l (UnQual l (Ident l x)))

ifThenElse :: Haskellish st a -> Haskellish st b -> Haskellish st c -> Haskellish st (a,b,c)
ifThenElse aP bP cP = Haskellish (\st e -> do
  (aExp,bExp,cExp) <- f e
  (a,st') <- runHaskellish aP st aExp
  (b,st'') <- runHaskellish bP st' bExp
  (c,st''') <- runHaskellish cP st'' cExp
  return ((a,b,c),st''')
  )
  where
    f (Paren _ x) = f x
    f (If _ x y z) = Right (x,y,z)
    f _ = Left ""

collectDoStatements :: Exp SrcSpanInfo -> [Exp SrcSpanInfo]
collectDoStatements (Do _ xs) = catMaybes $ fmap f xs
  where
    f (Qualifier _ e) = Just e
    f _ = Nothing
collectDoStatements _ = []

listOfDoStatements :: Haskellish st a -> Haskellish st [a]
listOfDoStatements p = Haskellish (\st e -> do
  let xs = collectDoStatements e
  foldM f ([],st) xs
  )
  where
    f (ys,st) x = do
      (y,st') <- runHaskellish p st x
      return (ys ++ [y],st')


type Span = ((Int,Int),(Int,Int))

span :: Haskellish st Span
span = Haskellish (\st e -> return (expToSpan e,st))

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

reverseApplication :: Haskellish st a -> Haskellish st (a -> b) -> Haskellish st b
reverseApplication x f = Haskellish (\st e -> do
  (e1,e2) <- applicationExpressions e
  (x',st') <- runHaskellish x st e1
  (f',st'') <- runHaskellish f st' e2
  return (f' x',st'')
  )

-- | binaryApplication targets the specific situation of parsing a function that is applied to two
-- arguments, given parsers for the function and each of the two arguments. This is intended for rare
-- cases - in most cases, Haskellish's Applicative instance will be a preferred way of parsing function
-- application. Unlike the applicative instance, this function returns the three components (function
-- and two arguments) separately, ie. the function is not actually applied to its arguments in the return type.

binaryApplication :: Haskellish st f -> Haskellish st a -> Haskellish st b -> Haskellish st (f,a,b)
binaryApplication fP aP bP = Haskellish (\st e -> do
  (x,bE) <- applicationExpressions e
  (fE,aE) <- applicationExpressions x
  (f,st') <- runHaskellish fP st fE
  (a,st'') <- runHaskellish aP st' aE
  (b,st''') <- runHaskellish bP st'' bE
  return ((f,a,b),st''')
  )
