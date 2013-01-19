import Control.Monad.State
import Data.Traversable
import Data.Maybe
import Data.Map as Map hiding (map)

data Expr = U                            -- unit value 
          | N Double                     -- a number
          | S String                     -- an string
          | Sym String                   -- a symbol    
          | B   Bool                     -- a boolean 
          | M Expr [Expr]                -- a message with selector and arguments list
          | V Expr                       -- a variable. Notice that its identifier may be an expresion!
          | VDef Expr Expr               -- variable binding - definition, with identifier and value
          | VSet Expr Expr               -- variable destructive assignment
          | Comment String               -- a comment
          | Send Expr Expr               -- message send, with receptor and message 
          | MDef Expr [Expr] [Expr]      -- method definition      
          | CDef Expr [Expr] [Expr]      -- class definition
          | O Expr [Expr]                -- an object, with class and constructor arguments
          deriving (Show, Read)

messageArgs (M _ a) = a
messageSelector (M s _) = s

type Context = Map String Expr

{-- Expression evaluation --}

eval :: Expr -> State Context Expr
eval (O className args) = do cn <- eval className
                             a  <- mapWithState eval args
                             return $ O cn a                          
eval (CDef _ _ _) = error "not implemented"
eval (MDef _ _ _) = error "not implemented"
eval (Send receptor message) = do r <- eval receptor
                                  M s a <- eval message
                                  return $ methodLookup r s a
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

methodLookup receptor (Sym selector) = methodLookup' receptor selector
methodLookup _        _ = error "selector must be a symbol" 

methodLookup' :: Expr -> String -> [Expr] -> Expr
methodLookup' (N n) "+" []     = M (Sym "+") [N n]
methodLookup' (N n) "+" [N n2] = N (n + n2)

methodLookup' (N n) "*" []     = M (Sym "*") [N n]
methodLookup' (N n) "*" [N n2] = N (n * n2)

-- TODO must define how to work with lazy evaluation: fixed? configurable? always?
methodLookup' b@(B _)   "ifelse"   []   = M (Sym "ifelse")  [b]
methodLookup' b@(B _)   "ifelse"  [e]   = M (Sym "ifelse")  [b, e]
methodLookup' (B True)  "ifelse" [e, _] = e
methodLookup' (B False) "ifelse" [_, e] = e
methodLookup' e1        "ifelse" [b@(B _)]  = methodLookup' b "ifelse" [e1] 
methodLookup' e2        "ifelse" [b@(B _), e1] = methodLookup'  b "ifelse" [e1, e2]      


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

