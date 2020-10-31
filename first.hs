-- from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps
-- this is attempt#2 at learning haskell, I have completey forgotten the one before
module Main where
import System.Environment

-- main :: IO ()
-- main = do
--     -- in haskell do blocks consist of format
--     -- name <- action1
--     (args) <- getArgs -- ["one","two"] or ["1","2"]
--     -- action2
--     -- putStrLn("Hello, " ++ args !! 0 ++ args !! 1)

--     -- you can use let statements or you can directly call show as I have done below
--     -- let a1 = read (args!!0):: Int
--     -- let a2 = read (args!!1):: Int
--     -- let a3 = a1 + a2
--     -- putStrLn("Sum, " ++ show a3)
    
--     putStrLn("Sum, " ++ show ((read (args!!0):: Int)+(read (args!!1):: Int)))

main :: IO ()
main = do
    -- this is second approach that uses getLine
    print ("Hello, what is you name?")
    name <- getLine
    putStrLn ("Nice to see you, " ++ name)
