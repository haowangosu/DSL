
module Wheels where

import Data.List
import Text.CSV

-- 1. syntax
type D = String -- one cell content
type Dset = [[D]]--whole spread sheet
--2. helper

--reference: https://hackage.haskell.org/package/either-unwrap-1.1/docs/Data-Either-Unwrap.html
-- | The 'fromRight' function extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @Left _@.
fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

--1,5,9
--2,6,10
--3,7,11
--4,8,12

--give a num n and a list, output the n's element of the list, and the list that del. the n's element
moveEleOut :: Int -> [a] -> (a,[a])
moveEleOut 1 (x:xs) = (x,xs)
moveEleOut i (x:xs) = (fst (moveEleOut (i-1) xs), [x] ++ snd (moveEleOut (i-1) xs))

--This a helper funtion for changeValueByPos. It takes a D list, a column order number, and a target string as input, and output a D list that change the specific string to the target string.
moveEleIn :: Int -> (a,[a]) -> [a]
moveEleIn _ (x,[])     = [x]
moveEleIn 1 (x,ys)     = x:ys
moveEleIn i (x,(y:ys)) = y:moveEleIn (i-1) (x,ys)

--give list of list, return list
unfold :: [[a]] -> [a]
unfold []     = []
unfold (x:xs) = x ++ unfold xs


--3. domain, operator
-- one cell: give a string, and target type(int, float, bool, string), then get new value with target type

--semantic
--keyword search. If element not show in dlist, then return false. If false, then ignore this row, move to next value in the list
search :: D -> Dset -> Dset
search d [] = []
search d (dlist:dset) = case elem d dlist of
 True  -> dlist : search d dset
 False -> search d dset

-- This is a helper function for deleteColumn. It takes a Dset                                                                                                                                                                                                                                                                                                                                                                                                 and a column order number as input, and output a Dset that delete the colomn.
deleteColumn1 :: Int -> Dset -> Dset
deleteColumn1 _ []      = []
deleteColumn1 i (s:ss)  = snd (moveEleOut i s) : deleteColumn1 i ss

-- This is a helper function for deleteRow. It takes a Dset and a row order number as input, and output a Dset that delete the row.
deleteRow1 :: Int -> Dset -> Dset
deleteRow1 i s= snd (moveEleOut i s)

-- It takes a Dset and multiple column order numbers as input, and output a Dset that delete the colomns.
deleteColumn :: [Int] -> Dset -> Dset
deleteColumn [] s = s
deleteColumn is s = deleteColumn (tail (reverse (sort is))) (deleteColumn1 (head (reverse (sort is))) s)-- operate the last column first

-- It takes a Dset and multiple row order numbers as input, and output a Dset that delete the rows.
deleteRow :: [Int] -> Dset -> Dset
deleteRow [] s = s
deleteRow is s = deleteRow (tail (reverse (sort is))) (deleteRow1 (head (reverse (sort is))) s)



--[], ["",""] （null x 产生的["",""]）这两种是emptylist, null [] = True, null ["",""]=False
isEmptyList :: [D] -> Bool
isEmptyList []     = True
isEmptyList (x:xs) = if null x then isEmptyList xs else False
--remove the duplicate data and data that only contains empty string.
dupForAll :: Dset -> Dset
dupForAll [] = []
dupForAll x  = if isEmptyList (head (nub x))
               then dupForAll (tail (nub x))
			   else (head (nub x)) : dupForAll (tail (nub x))
--canBeDup ["1","","3","4"]["","2","","4"]=True
--logic error check
canBeDup :: [D] -> [D] -> Bool
canBeDup [] _          = True
canBeDup _ []          = True
canBeDup (x:xs) (y:ys) = (x==y || x=="" || y=="") && canBeDup xs ys
--dupHelper ["1","","3","4"]["","2","","4"]=["1","2","3","4"]
dupHelper :: [D] -> [D] -> [D]
dupHelper x []          = x
dupHelper [] y          = y
dupHelper (x:xs) (y:ys) = if x==""
                             then y : dupHelper xs ys
                             else x : dupHelper xs ys

--let d=[1,2,3,4]
-- d!!0=1; d!!1=2;d!!2=3;d!!3=4

dupForKey2 :: Int -> [D] -> [D] -> [D]
dupForKey2 _ [] _  = []
dupForKey2 _ _ []  = []
dupForKey2 i d1 d2 = if d1!!(i-1) == d2!!(i-1) then
                        case canBeDup d1 d2 of
                          True      -> [d1!!(i-1)] ++ dupHelper (take (i-1) d1 ++ drop i d1) (take (i-1) d2 ++ drop i d2)-- take 2 [1,2,3,4,5]=[1,2]; drop 2 [1,2,3,4,5]=[3,4,5]
                          otherwise -> []
                     else
                        []

dupForKey :: Int -> Dset -> Dset
dupForKey _ []       = []
dupForKey _ [d]      = [d]
dupForKey i (d:dset) = map (dupForKey2 i d) dset ++ dupForKey i dset



--joinforkey2 helper
--This is a helper function for joinForKey2. It takes 2 D lists and their corresponding key order number as input, and output a Dset that contains only one merged data when the key value is the same, or a Dset that contains two separated data when the key value is different

keymerge2 :: Int -> [D] -> Int -> [D] -> Dset
keymerge2 i1 d1 i2 d2 = if d1!!(i1-1) == d2!!(i2-1) then
                           [[d1!!(i1-1)] ++ take (i1-1) d1 ++ drop i1 d1 ++ take (i2-1) d2 ++ drop i2 d2]
                        else
                           [[d1!!(i1-1)] ++ take (i1-1) d1 ++ drop i1 d1 ++ replicate ((length d2)-1) "", 
                            [d2!!(i2-1)] ++ replicate ((length d1)-1) "" ++ take (i2-1) d2 ++ drop i2 d2]


joinForKey2 :: (Dset,Int) -> (Dset,Int) -> Dset
joinForKey2 ([],_) _                 = []
joinForKey2 _ ([],_)                 = []
joinForKey2 (d1:dset1,i1) (dset2,i2) = dupForAll (dupForKey 1 ((dupForAll (unfold (map (keymerge2 i1 d1 i2) (dupForAll dset2))) ++ joinForKey2 (dupForAll dset1,i1) (dupForAll dset2,i2))))


joinForKey :: [(Dset,Int)] -> Dset
joinForKey [] = []
joinForKey (d:[]) = fst d
joinForKey (d1:d2:dset) = joinForKey ((joinForKey2 d1 d2,1):dset)

--changeHelperByPos 3 "zzz" ["abc","def","hig","dig"]=["abc","def","zzz","dig"]
changeHelperByPos :: Int -> D -> [D] -> [D]
changeHelperByPos i d dlist = moveEleIn i (d, snd (moveEleOut i dlist))

--This a helper funtion for changeValueBySearch. It takes a D list, an original string, and a target string as input, and output a D list that change the original string to the target string.
--changeHelperBySearch "dig" "change"["abc","def","hig","dig"]=["abc","def","hig","change"]

changeHelperBySearch :: D -> D -> [D] -> [D]
changeHelperBySearch _ _ []          = []
changeHelperBySearch d d1 (d0:dlist) = if d == d0
then d1 : changeHelperBySearch d d1 dlist
else d0 : changeHelperBySearch d d1 dlist

--This a helper function for changeValueByPos. It takes a Dset, a position (row,column), and a target string as input, and output a Dset that change the specific string to the target string.
changeValueByPos1 :: (Int,Int) -> D -> Dset -> Dset
changeValueByPos1 (1,i) d (dlist:dset)  = changeHelperByPos i d dlist : dset
changeValueByPos1 (i0,i) d (dlist:dset) = dlist : changeValueByPos1 ((i0-1),i) d dset

--This a helper function for changeValueBySearch. It takes a Dset, an original string, and a target string as input, and output a Dset that change the original string to the target string.
changeValueBySearch1 :: D -> D -> Dset -> Dset
changeValueBySearch1 _ _ []            = []
changeValueBySearch1 d d1 (dlist:dset) = changeHelperBySearch d d1 dlist : changeValueBySearch1 d d1 dset

--It takes a Dset, multiple positions (row,column), and multiple target strings as input, and output a Dset that change the specific strings to the target strings for the target positions.
changeValueByPos :: [(Int,Int)] -> [D] -> Dset -> Dset
changeValueByPos [] _ dset          = dset
changeValueByPos _ [] dset          = dset
changeValueByPos (p:ps) (d:ds) dset = changeValueByPos ps ds (changeValueByPos1 p d dset)

--It takes a Dset, multiple original strings, and multiple target strings as input, and output a Dset that change the original strings to the corresponding target strings.
changeValueBySearch :: [D] -> [D] -> Dset -> Dset
changeValueBySearch [] _ dset = dset
changeValueBySearch _ [] dset = dset
changeValueBySearch (d:ds) (d1:d1s) dset = changeValueBySearch ds d1s (changeValueBySearch1 d d1 dset)


--main
-- convert the input csv file to type Dset
input :: FilePath -> String -> Dset
input = \x -> \y -> dupForAll (fromRight (parseCSV x y))-- parseCSV: either error or dset
--input = \x -> \y -> fromRight (parseCSV x y)

-- a helper function for output. It adds commas for output csv file.
helper1 :: [String] -> String --everyline add ,
helper1 []     = ""
helper1 [x]    = x
helper1 (x:xs) = x ++ "," ++ helper1 xs

-- a helper function for output.
helper2 :: Dset -> [String] -- repect to all set
helper2 []     = []
helper2 (x:xs) = helper1 x : helper2 xs

-- a helper funciton for output. It adds newlines for output csv file.
helper3 :: [String] -> String -- everline add /n
helper3 [] = ""
helper3 (x:xs) = x ++ "\r\n" ++ helper3 xs

-- convert the Dset to an output csv file.
output :: FilePath -> Dset -> IO ()
output x dset = writeFile x (helper3 (helper2 dset))
-- example:
sample :: IO ()
sample = do
 d1 <- readFile "w1.csv"
 let dset1 = input "w1.csv" d1
 d2 <- readFile "w2.csv"
 let dset2 = input "w2.csv" d2
 let join = joinForKey [(dset1,1), (dset2,1)]
 output "showJoin.csv" join
 let dcol = deleteColumn [26,27] join
 output "showDeleteColumn.csv" dcol
 let cval = changeValueBySearch ["4217","2218","TPP WHOLESALE PTY LTD."] ["4219","4217","CRAZY DOMAINS FZ-LLC"] dcol
 print cval
 output "showChangeValue.csv" cval
