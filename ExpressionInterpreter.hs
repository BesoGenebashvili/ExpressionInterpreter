module GitHub.ExpressionInterpreter where

import Prelude hiding (exp)
import Data.List      (find)
import Data.Maybe     (fromJust)

type Identifier = String

type Environment = [(Identifier, Value)]

data Expression = Number Int
                | Plus     Expression Expression
                | Minus    Expression Expression
                | Variable Identifier
                | Let      Identifier Expression Expression
                deriving Show

newtype Value = Value Int deriving Show

findValue :: Identifier -> Environment -> Value
findValue i = snd . fromJust . find ((==i) .  fst)

elaborate :: Identifier -> Expression -> Environment -> Environment
elaborate i exp env = (i, evaluate exp env) : deleteVariable i env

deleteVariable :: Identifier -> Environment -> Environment
deleteVariable i = filter ((/=i) . fst)

evaluate :: Expression -> Environment -> Value
evaluate (Number n)   _   = Value n
evaluate (Plus a b)   env = let (Value a', Value b') = (evaluate a env, evaluate b env) in Value (a' + b')
evaluate (Minus a b)  env = case (evaluate a env, evaluate b env) of (Value a', Value b') -> Value (a' - b')
evaluate (Variable i) env = findValue i env
evaluate (Let i a b)  env = evaluate a (elaborate i b env)
