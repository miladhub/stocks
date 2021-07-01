module Parser where

import Control.Monad.State

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)
  deriving (Eq, Show)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type SYState = (Stack, Output)

isEmpty :: State SYState Bool
isEmpty = null <$> gets fst

notEmpty :: State SYState Bool
notEmpty = not <$> isEmpty

top :: State SYState Token
top = gets (head . fst)

whileNotEmptyAnd :: (Token -> Bool) -> State SYState () -> State SYState ()
whileNotEmptyAnd predicate m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- predicate <$> top
        when b2 (m >> go)
