import Control.Monad.State
import Data.Traversable
import Data.Maybe
import Data.Map as Map

data Expr = NumberE Double
          | StringE String
          | SymbolE String 
          | BoolE   Bool
          | MessageE { selector :: Expr, messageArgs :: [Expr] }
          | VarE Expr
          | VarDefE Expr Expr
          | VarAssignE Expr Expr 
          deriving Show


type Context = Map String Expr


eval :: Expr -> State Context Expr
eval (VarAssignE name value) = do n <- eval name
                                  v <- eval value
                                  putDef updateCheck n v                      
eval (VarDefE name value) = do n <- eval name
                               v <- eval value
                               putDef putCheck n v
eval (VarE name) =  do  n <- eval name
                        getDef n
eval (MessageE selector args) = do s <- eval selector  
                                   a <- mapWithState eval args
                                   return $ MessageE s a 
eval n = return n

evalClean = runClean.eval

runClean = flip runState Map.empty

evalAll exprs = mapWithState eval exprs

evalAllClean = runClean.evalAll

type PutCheck = (String -> Context -> Bool, String)
putCheck = (\n -> member n, "already defined")
updateCheck = (\n -> not.member n, "never defined")

putDef check (SymbolE n) v = State put  
                  where put ctx | fst check n ctx = error (snd check)
                                | otherwise = (v, insert n v ctx)    

getDef (SymbolE n) = do
           ctx <- get
           return $ ctx ! n


swap (x, y) = (y, x)

mapWithState ::  (a-> State s a) -> [a] -> State s [a]
mapWithState f xs = State $ mapAccumStateL f xs
   where mapAccumStateL f xs s = (swap.mapAccumL (\s' a -> (swap.runState (f a)) s') s) xs




vdef = VarDefE 
vset = VarAssignE 
v = VarE

sym = SymbolE
str = StringE 
num = NumberE 







