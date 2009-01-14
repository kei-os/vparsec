module DumpTest where

import Control.Monad(forM_)
import System.IO

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

ts = 2

printBar :: Bar -> IO ()
printBar (BStr s) = putStrLn $ show s
printBar (BInt i) = putStrLn $ show i

printValue :: Foo -> Int -> IO ()
printValue (FBar f) _ = printBar f
printValue (FStr s) _ = putStrLn $ show s
printValue (FInt i) _ = putStrLn $ show i
printValue (FBool True) _ = putStrLn "true"
printValue (FBool False) _ = putStrLn "false"
printValue FNil _ = putStrLn "nil"
printValue (FObj xs) i = do
    putStrLn ""
    putSpace i
    putStrLn "{"   
    putSpace i
    case xs of
        [] -> putStrLn ""
        (p:ps) -> do putSpace 2
                     putPair p
                     forM_ ps $ \q -> do putSpace i
                                         putStr ", "
                                         putPair q
    putSpace i
    putStrLn "}"
        where
            putPair (k, v) = do putStr $ show k
                                putStr ": "
                                printValue v (i+ts)

putSpace :: Int -> IO ()
putSpace i = if i > 0 then do {putStr " "; putSpace (i-1) }
                      else putStr ""

-- test data
dat :: Foo
dat = FObj ([   ("a", FInt 10),
                ("b", FStr "foo"),
                ("c", FNil),
                ("d", FBool True),
                ("e", FBool False),
                ("f", FBar (BStr "bar")),
                ("g", FObj[("gg", FBool False), ("hh", FBool True), ("ii", FBool False)])    -- test
            ])

{-
datnest :: Foo
datnest = FObj ([("a", FNil), ("b", FBool True), [FObj ("e", FBool False)],
            ("f", FBar (BStr "bar"))])
-}


-------------------------------------------------------------------------

myDump :: FilePath -> Foo -> IO ()
myDump fname f = do outh <- openFile fname WriteMode
                    hPutStr outh $ myShow f
                    hClose outh

a :: Foo
a = FStr "foo"

myShow :: Foo -> String
--myShow x = show x
myShow = strFoo

strBar :: Bar -> String
strBar (BStr s) = "[b: " ++ s ++ "]"
strBar (BInt i) = "[b: " ++ show i ++ "]"

strFoo' :: String -> String
strFoo' str = "[f: " ++ str ++ "]"

strFoo :: Foo -> String
--strFoo (FStr s) = s       -- initial test version
strFoo (FStr s) = strFoo' s
strFoo (FInt i) = strFoo' $ show i
strFoo (FBool True) = strFoo' "true"
strFoo (FBool False) = strFoo' "false"
strFoo FNil = strFoo' "nil"
strFoo (FBar b) = strBar b
strFoo (FObj xs) = do
    case xs of
        [] -> ""
{-
        (p:ps) -> do { let a = showPair p
                     ; let b = strFoo $ FObj ps
                     ; a ++ b }
-}
        (p:ps) -> showPair p ++ strFoo (FObj ps)        -- this is simpler
            where
                showPair :: (String, Foo) -> String
                showPair (k, v) = "(" ++ k ++ ")" ++ strFoo v ++ "\n"

---------------------------------------------------------------------------------
{-      -- XXX not work...
data Nest = S String
          | LIST [Nest]
            deriving (Show)

nest :: Nest
--nest = LIST ([S "a", S "b", S "c", LIST ([S "d", S "e"])])
nest = LIST ([S "a", S "b", S "c"])

showNest :: Nest -> Int -> IO ()
showNest (S str) i = do { showSpace i; putStrLn str }
showNest (LIST []) _ = putStrLn ""
showNest (LIST (x:xs)) i = do { showNest x (i+1); showNest xs i }

--showNest (LIST (x:xs)) i = do showNest x i 
--                              showNest xs (i+1)

-}

showSpace :: Int -> IO ()
showSpace i = if i > 0 then do { putStr " "; showSpace $ dec i } else putStr ""


inc :: (Num a) => a -> a
inc i = i + 1

dec :: (Num a) => a -> a
dec i = if i == 0 then 0        -- XXX cannot use <= or <
        else i - 1

