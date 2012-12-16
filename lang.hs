import Control.Monad.State
import Data.Traversable
import Data.Maybe
import Data.Map as Map hiding (map)

data Expr = U 
          | N Double
          | S String
          | Sym String 
          | B   Bool
          | M Expr [Expr] 
          | V Expr
          | VDef Expr Expr
          | VSet Expr Expr
          | Comment String
          | Send Expr Expr
          deriving (Show, Read)


type Context = Map String Expr

{-- Expression evaluation --}

eval :: Expr -> State Context Expr
eval (Send receptor message) = do r <- eval receptor
                                  m <- eval message
                                  return (Send r m)
eval (Comment _) = return U
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

runClean = flip runState Map.empty

evalClean = runClean.eval

{- Expressions sequence  evaluation --}

evalAll exprs = mapWithState eval exprs

evalAllClean = runClean.evalAll

{- Method lookup -}

--TODO 
{- Context handling -}

type PutCheck = (String -> Context -> Bool, String)
putCheck = (\n -> member n, "already defined") :: PutCheck
updateCheck = (\n -> not.member n, "never defined") :: PutCheck

putDef check (Sym n) v = State put  
                  where put ctx | fst check n ctx = error (snd check)
                                | otherwise = (v, insert n v ctx)    

getDef (Sym n) = do
           ctx <- get
           return $ ctx ! n

{- Generic functions -}

swap (x, y) = (y, x)

mapWithState ::  (a-> State s b) -> [a] -> State s [b]
mapWithState f xs = State $ mapAccumStateL f xs
   where mapAccumStateL f xs s = (swap.mapAccumL (\s' a -> (swap.runState (f a)) s') s) xs

{- File Parsing -}

evalFile filename = do 
                    content <- readFile filename
                    return . evalAllClean . map read . lines $ content 

