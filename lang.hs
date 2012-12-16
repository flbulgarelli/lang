data Expr = NumberE Double
          | StringE String
          | SymbolE String 
          | BoolE   Bool
          | MessageE { selector :: Expr, messageArgs :: [Expr] }
          deriving Show

reduce (MessageE selector messageArgs) = MessageE (reduce selector) (map reduce messageArgs)
reduce n = n




