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
          | PM Expr Expr [Expr]          -- a partial message, with original receptor, selector, and partial arguments list
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
                                  m <- eval message
                                  return $ messageResolve r m
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
messageResolve r (M s a) =  methodLookup r s a 
messageResolve r' (PM r s a) = methodLookup r s (a ++ [r'])

methodLookup receptor (Sym selector) = methodLookup' receptor selector receptor selector
methodLookup _        _ = error "selector must be a symbol" 

methodLookup' :: Expr -> String -> Expr -> String -> [Expr] -> Expr
methodLookup' (N n) "+" = run1 (\(N n2) ->  N (n + n2))
methodLookup' (N n) "*" = run1 (\(N n2) ->  N (n * n2))
methodLookup' (N n) "-" = run1 (\(N n2) ->  N (n - n2))

-- TODO must define how to work with lazy evaluation: fixed? configurable? always?
methodLookup' (B True)  "ifelse"   = run2 (\e1 _ -> e1)
methodLookup' (B False) "ifelse"   = run2 (\_ e2 -> e2)


run1 :: (Expr -> Expr) -> Expr -> String -> [Expr] -> Expr
run1 impl _ _ [a1]     = impl a1
run1 _ r s as          = runPartial 1 r s as

run2 impl _ _ [a1, a2] = impl a1 a2
run2 _    r s as       = runPartial 2 r s as

run3 impl _ _ [a1, a2, a3] = impl a1 a2 a3
run3 _    r s as           = runPartial 3 r s as

runPartial arity r s as | length as < arity = PM r (Sym s) as
                        | otherwise = error "too much arguments"

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

--TODO debug = ... show, format, print line number, print expression, print expression result
