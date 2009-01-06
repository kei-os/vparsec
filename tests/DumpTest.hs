module DumpTest where

import Control.Monad(forM_)

data Foo = FStr String
         | FInt Integer
         | FBool Bool
         | FNil
         | FObj [(String, Foo)]
         | FBar Bar
           deriving (Show)

data Bar = BStr String
         | BInt Integer
           deriving (Show)

printBar :: Bar -> IO ()
printBar (BStr s) = putStrLn $ show s
printBar (BInt i) = putStrLn $ show i

printValue :: Foo -> IO ()
printValue (FBar f) = printBar f
printValue (FStr s) = putStrLn $ show s
printValue (FInt i) = putStrLn $ show i
printValue (FBool True) = putStrLn "true"
printValue (FBool False) = putStrLn "false"
printValue FNil = putStrLn "nil"
printValue (FObj xs) = do
    putStr "{ "   
    case xs of
        [] -> putStrLn ""
        (p:ps) -> do putPair p
                     forM_ ps $ \q -> do putStr ", "
                                         putPair q
    putStrLn "}"
        where
            putPair (k, v) = do putStr $ show k
                                putStr ": "
                                printValue v

-- test data
dat :: Foo
dat = FObj ([("a", FInt 10), ("b", FStr "foo"),
            ("c", FNil), ("d", FBool True), ("e", FBool False),
            ("f", FBar (BStr "bar"))])

