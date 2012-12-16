import Control.Monad.State
import Data.Traversable
import Data.Maybe

data Expr = NumberE Double
          | StringE String
          | SymbolE String 
          | BoolE   Bool
          | MessageE { selector :: Expr, messageArgs :: [Expr] }
          | VarE Expr
          | VarDefE Expr Expr
          deriving Show


type Context =  [(String, Expr)]


eval :: Expr -> State Context Expr
eval (VarDefE name value) = do n <- eval name
                               v <- eval value
                               putDef n v
eval (VarE name) =  do  n <- eval name
                        getDef n
eval (MessageE selector args) = do s <- eval selector  
                                   a <- mapWithState eval args
                                   return $ MessageE s a 
eval n = return n

evalClean = runClean.eval

runClean = flip runState []

evalAll exprs = mapWithState eval exprs

evalAllClean = runClean.evalAll

putDef (SymbolE n) v = State (\ctx ->  (v,(n, v): ctx))
getDef (SymbolE n) = do
           ctx <- get
           return . fromJust . lookup n $ ctx


swap (x, y) = (y, x)

mapWithState ::  (a-> State s a) -> [a] -> State s [a]
mapWithState f xs = State $ mapAccumStateL f xs
   where mapAccumStateL f xs s = (swap.mapAccumL (\s' a -> (swap.runState (f a)) s') s) xs












