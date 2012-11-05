module WHILE
       ( WProg 
       , wparse
       , executeProgram
       , executeFile
       ) where 

import WhileLang
import WhileLexer
import WhileParser


wparse = whileProgramParsing . whileLexing

executeProgram str = case (wparse str) 
                     of Ok (prog, vars) -> let s = execute prog [] 
                                           in map (valueOf s) vars 
                        _ -> []
  where valueOf s x = x ++ " = " ++ case getValue x s of Just (Wnat n) -> show n
                                                         Just (Wbool b) -> show b
                                                         _ -> "Nav"

executeFile fn = do str <- readFile fn 
                    mapM print (executeProgram str)

