module Main where

import Text.CSV
import Syntax
import Helpers
import Helpers2
import Domains
import BasicSemantics
import UISemantics


input :: FilePath -> String -> Dset
input = \x -> \y -> dupForAll (fromRight (parseCSV x y))

helper1 :: [String] -> String
helper1 []     = ""
helper1 [x]    = x
helper1 (x:xs) = x ++ "," ++ helper1 xs

helper2 :: Dset -> [String]
helper2 []     = []
helper2 (x:xs) = helper1 x : helper2 xs

helper3 :: [String] -> String
helper3 [] = ""
helper3 (x:xs) = x ++ "\r\n" ++ helper3 xs

output :: FilePath -> Dset -> IO ()
output x dset = writeFile x (helper3 (helper2 dset))


