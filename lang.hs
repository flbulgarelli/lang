import Control.Monad.State
import Data.Traversable

data Expr = NumberE Double
          | StringE String
          | SymbolE String 
          | BoolE   Bool
          | MessageE { selector :: Expr, messageArgs :: [Expr] }
          | VarE Expr
          | VarDefE Expr Expr
          deriving Show


data Context = Context [(String, Expr)] deriving Show


eval :: Expr -> State Context Expr
eval (MessageE selector args) = do {s <- eval selector ; a <- mapWithState eval args;  return  $ MessageE s a  }
eval n = return n

evalClean = flip runState (Context []).eval


swap (x, y) = (y, x)

mapWithState ::  (a-> State s a) -> [a] -> State s [a]
mapWithState f xs = State $ mapAccumStateL f xs
   where mapAccumStateL f xs s = (swap.mapAccumL (\s' a -> (swap.runState (f a)) s') s) xs












