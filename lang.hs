import Control.Monad.State
import Data.Traversable
import Data.Maybe
import Data.Map as Map hiding (map)

data Expr = N Double
          | S String
          | Sym String 
          | B   Bool
          | M { selector :: Expr, messageArgs :: [Expr] }
          | V Expr
          | VDef Expr Expr
          | VSet Expr Expr 
          deriving (Show, Read)


type Context = Map String Expr


eval :: Expr -> State Context Expr
eval (VSet name value) = do n <- eval name
                            v <- eval value
                            putDef updateCheck n v                      
eval (VDef name value) = do n <- eval name
                            v <- eval value
                            putDef putCheck n v
eval (V name) =  do  n <- eval name
                     getDef n
eval (M selector args) = do s <- eval selector  
                            a <- mapWithState eval args
                            return $ M s a 
eval n = return n

evalClean = runClean.eval

runClean = flip runState Map.empty

evalAll exprs = mapWithState eval exprs

evalAllClean = runClean.evalAll

type PutCheck = (String -> Context -> Bool, String)
putCheck = (\n -> member n, "already defined") :: PutCheck
updateCheck = (\n -> not.member n, "never defined") :: PutCheck

putDef check (Sym n) v = State put  
                  where put ctx | fst check n ctx = error (snd check)
                                | otherwise = (v, insert n v ctx)    

getDef (Sym n) = do
           ctx <- get
           return $ ctx ! n


swap (x, y) = (y, x)

mapWithState ::  (a-> State s a) -> [a] -> State s [a]
mapWithState f xs = State $ mapAccumStateL f xs
   where mapAccumStateL f xs s = (swap.mapAccumL (\s' a -> (swap.runState (f a)) s') s) xs


evalFile filename = do 
                    content <- readFile filename
                    return . evalAllClean . map read . lines $ content 
