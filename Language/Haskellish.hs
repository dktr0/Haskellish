{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskellish where

import Language.Haskell.Exts as Exts
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Either (isRight)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T


type Span = ((Int,Int),(Int,Int))

data ParseError = NonFatal Span Text | Fatal Span Text

data Haskellish st a = Haskellish { _run :: st -> Exp SrcSpanInfo -> Either ParseError (a,st) }

-- runHaskellish, introduced in this form in 0.2.4 and considered *deprecated* in favour of parseExp
-- is primarily meant to avoid breaking changes with projects built against 0.2.3 and earlier

runHaskellish :: Haskellish st a -> st -> Exp SrcSpanInfo -> Either String (a,st)
runHaskellish h st e =
  case _run h st e of
    Right (a,s) -> Right (a,s)
    Left (NonFatal ((a,b),_) t) -> Left $ show a ++ ":" ++ show b ++ " " ++ T.unpack t
    Left (Fatal ((a,b),_) t) -> Left $ show a ++ ":" ++ show b ++ " " ++ T.unpack t

-- parseExp replaces runHaskellish and is intended as the main top-level entry point for
-- running a Haskellish parser. It uses haskell-src-exts to parse Text into a Haskell AST
-- that is then parsed by the Haskellish parser.

parseExp :: Haskellish st a -> st -> Text -> Either (Span,Text) (a,st)
parseExp h st x = do
  case Exts.parseExp (T.unpack x) of
    Exts.ParseOk e -> do
      case _run h st e of
        Right (a,st) -> Right (a,st)
        Left (NonFatal s t) -> Left (s,t)
        Left (Fatal s t) -> Left (s,t)
    Exts.ParseFailed loc err -> Left (((a,b),(a,b)),T.pack err)
      where
        a = Exts.srcLine loc
        b = Exts.srcColumn loc


exp :: Haskellish st (Exp SrcSpanInfo)
exp = Haskellish (\st e -> return (e,st))

fatal :: Text -> Haskellish st a
fatal m = Haskellish (\st e -> Left $ Fatal (expToSpan e) m)

nonFatal :: Text -> Haskellish st a
nonFatal m = Haskellish (\st e -> Left $ NonFatal (expToSpan e) m)

(<?>) :: Haskellish st a -> Text -> Haskellish st a
h <?> msg = h <|> nonFatal msg

(<?!>) :: Haskellish st a -> Text -> Haskellish st a
h <?!> msg = h <|> fatal msg

-- required makes any non-fatal errors into fatal errors

required :: Haskellish st a -> Haskellish st a
required h = Haskellish (\st e -> do
  case _run h st e of
    Right (a,s) -> Right (a,s)
    Left (NonFatal s t) -> Left $ Fatal s t
    Left (Fatal s t) -> Left $ Fatal s t
  )

-- <*!> is a variant of the applicative <*> operator where the argument on the right
-- is implicitly wrapped in 'required' (see above)

(<*!>) :: Haskellish st (a -> b) -> Haskellish st a -> Haskellish st b
f <*!> x = f <*> required x


instance Functor (Haskellish st) where
  fmap f x = Haskellish (\st e -> do
    (x',st') <- _run x st e
    Right (f x',st')
    )


instance Applicative (Haskellish st) where
  pure x = Haskellish (\st _ -> Right (x,st))
  f <*> x = Haskellish (\st e -> do
    (e1,e2) <- applicationExpressions e
    (f',st') <- _run f st e1
    (x',st'') <- _run  x st' e2
    Right (f' x',st'')
    )



applicationExpressions :: Exp SrcSpanInfo -> Either ParseError (Exp SrcSpanInfo,Exp SrcSpanInfo)
applicationExpressions (Paren _ x) = applicationExpressions x
applicationExpressions (App _ e1 e2) = Right (e1,e2)
applicationExpressions (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = Right (e1,e2)
applicationExpressions (InfixApp l e1 (QVarOp _ (UnQual _ (Symbol _ x))) e2) = Right (App l x' e1,e2)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions (LeftSection l e1 (QVarOp _ (UnQual _ (Symbol _ x)))) = Right (x',e1)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions e = Left $ NonFatal (expToSpan e) "expected application expresssion"


instance Alternative (Haskellish st) where
  empty = Haskellish (\_ e -> Left $ NonFatal (expToSpan e) "")
  a <|> b = Haskellish (\st e -> do
    let a' = _run a st e
    case a' of
      Right _ -> a'
      Left (Fatal _ _) -> a'
      Left (NonFatal _ _) -> _run b st e
    )



instance Monad (Haskellish st) where
  x >>= f = Haskellish (\st e -> do
    (x',st') <- _run x st e
    _run (f x') st' e
    )


instance MonadPlus (Haskellish st) where
  mzero = empty
  mplus = (<|>)


instance MonadState st (Haskellish st) where
  get = Haskellish (\st _ -> return (st,st))
  put st = Haskellish (\_ _ -> return ((),st))


instance MonadError Text (Haskellish st) where
  throwError x = fatal x
  catchError x f = Haskellish (\st e -> do
    let x' = _run x st e
    case x' of
      Right (x'',st') -> Right (x'',st')
      Left (Fatal s err) -> _run (f err) st e
      Left (NonFatal s err) -> _run (f err) st e
    )


identifier :: Haskellish st String -- note: we don't distinguish between identifiers and symbols
identifier = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (Var _ (UnQual _ (Ident _ x))) = Right (x,st)
        f st (Var _ (UnQual _ (Symbol _ x))) = Right (x,st)
        f _ e = Left $ NonFatal (expToSpan e) "expected identifier"

reserved :: String -> Haskellish st ()
reserved x = Haskellish (\st e -> do
   (e',_) <- _run identifier st e
   if e' == x then Right ((),st) else Left (NonFatal (expToSpan e) "expected reserved word")
   )

string :: Haskellish st String
string = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (Lit _ (String _ x _)) = Right (x,st)
        f _ e = Left $ NonFatal (expToSpan e) "expected literal String"

integer :: Haskellish st Integer
integer = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (NegApp _ (Lit _ (Int _ x _))) = Right (x * (-1),st)
        f st (Lit _ (Int _ x _)) = Right (x,st)
        f _ e = Left $ NonFatal (expToSpan e) "expected Integer"

rational :: Haskellish st Rational
rational = Haskellish (\st e -> f st e)
  where f st (Paren _ x) = f st x
        f st (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1),st)
        f st (Lit _ (Frac _ x _)) = Right (x,st)
        f _ e = Left $ NonFatal (expToSpan e) "expected Rational"

rationalOrInteger :: Haskellish st Rational
rationalOrInteger = rational <|> (fromIntegral <$> integer) <|> nonFatal "expected Rational or Integer"

list :: Haskellish st a -> Haskellish st [a]
list p = Haskellish (\st e -> do
  xs <- listExpressions e
  foldM f ([],st) xs
  )
  where
    f (ys,st) x = do
      (y,st') <- _run p st x
      return (ys ++ [y],st')

listExpressions :: Exp SrcSpanInfo -> Either ParseError [Exp SrcSpanInfo]
listExpressions (Paren _ x) = listExpressions x
listExpressions (List _ xs) = Right xs
listExpressions e = Left $ NonFatal (expToSpan e) "expected list"

tuple :: Haskellish st a -> Haskellish st b -> Haskellish st (a,b)
tuple p1 p2 = Haskellish (\st e -> do
  (a,b) <- f e
  (a',st') <- _run p1 st a
  (b',st'') <- _run p2 st' b
  return ((a',b'),st'')
  )
  where
    f (Paren _ x) = f x
    f (Tuple _ Boxed (a:b:[])) = Right (a,b)
    f e = Left $ NonFatal (expToSpan e) "expected tuple"

asRightSection :: Haskellish st (a -> b -> c) -> Haskellish st b -> Haskellish st (a -> c)
asRightSection opP bP = Haskellish (\st e -> do
  (opExp,bExp) <- f e
  (op',st') <- _run opP st opExp
  (b,st'') <- _run bP st' bExp
  return (flip op' b,st'')
  )
  where
    f (Paren _ x) = f x
    f (RightSection _ (QVarOp l (UnQual _ (Symbol _ x))) e1) = Right (g l x,e1)
    f e = Left $ NonFatal (expToSpan e) "expected right section"
    g l x = (Var l (UnQual l (Ident l x)))

ifThenElse :: Haskellish st a -> Haskellish st b -> Haskellish st c -> Haskellish st (a,b,c)
ifThenElse aP bP cP = Haskellish (\st e -> do
  (aExp,bExp,cExp) <- f e
  (a,st') <- _run aP st aExp
  (b,st'') <- _run bP st' bExp
  (c,st''') <- _run cP st'' cExp
  return ((a,b,c),st''')
  )
  where
    f (Paren _ x) = f x
    f (If _ x y z) = Right (x,y,z)
    f e = Left $ NonFatal (expToSpan e) "expected if-then-else"


-- *** TODO: the relationship of collectDoStatement and listOfDoStatements to
-- error handling needs to be thought through....

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
      (y,st') <- _run p st x
      return (ys ++ [y],st')



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
srcSpanInfoToSpan x = ((by,bx),(ey,ex))
  where
    bx = srcSpanStartColumn $ srcInfoSpan x
    by = srcSpanStartLine $ srcInfoSpan x
    ex = srcSpanEndColumn $ srcInfoSpan x
    ey = srcSpanEndLine $ srcInfoSpan x

reverseApplication :: Haskellish st a -> Haskellish st (a -> b) -> Haskellish st b
reverseApplication x f = Haskellish (\st e -> do
  (e1,e2) <- applicationExpressions e
  (x',st') <- _run x st e1
  (f',st'') <- _run f st' e2
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
  (f,st') <- _run fP st fE
  (a,st'') <- _run aP st' aE
  (b,st''') <- _run bP st'' bE
  return ((f,a,b),st''')
  )

-- | functionApplication parses most cases where one thing is applied to another. Like binaryApplication, it is
-- is intended for rare cases where one wants to match the pattern of one thing being applied to another, without
-- that application "actually" taking place - the parsed sub-expressions are returned instead.

functionApplication :: Haskellish st a -> Haskellish st b -> Haskellish st (a,b)
functionApplication fP xP = Haskellish (\st e -> do
  (fE,xE) <- applicationExpressions e
  (f,st') <- _run fP st fE
  (x,st'') <- _run xP st' xE
  return ((f,x),st'')
  )
