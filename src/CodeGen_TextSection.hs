module CodeGen_TextSection where

import AST
import CodeGen_Declarations
import CodeGen_Statements
import CodeGen_Helper

-- generateTextSection returns a list of tuples containing subroutine calls and their definitions
generateTextSection :: [Stmt] -> TypeMap -> [(String, String)]
generateTextSection stmts typeMap = map (\stmt -> uncurry3 generateStmtSr stmt typeMap) indexedStmts
  where
    -- Creates list of empty strings the same length as stmts
    emptyStrings = replicate (length stmts) ""
    indexedStmts = zip3 emptyStrings [0..] stmts   -- ("", i, stmt)

    -- uncurry for 3 args
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z




