module UISemantics where

import Data.List
import Syntax
import Helpers
import Helpers2
import Domains
import BasicSemantics

isEmptyList :: [D] -> Bool
isEmptyList []     = True
isEmptyList (x:xs) = if null x then isEmptyList xs else False

dupForAll :: Dset -> Dset
dupForAll [] = []
dupForAll x  = if isEmptyList (head (nub x)) 
                  then dupForAll (tail (nub x))
                  else (head (nub x)) : dupForAll (tail (nub x))
                  
canBeDup :: [D] -> [D] -> Bool
canBeDup [] _          = True
canBeDup _ []          = True
canBeDup (x:xs) (y:ys) = (x==y || x=="" || y=="") && canBeDup xs ys

dupHelper :: [D] -> [D] -> [D]
dupHelper x []          = x
dupHelper [] y          = y
dupHelper (x:xs) (y:ys) = if x==""
                             then y : dupHelper xs ys
                             else x : dupHelper xs ys

dupForKey2 :: Int -> [D] -> [D] -> [D]
dupForKey2 _ [] _  = []
dupForKey2 _ _ []  = []
dupForKey2 i d1 d2 = if d1!!(i-1) == d2!!(i-1) then
                        case canBeDup d1 d2 of
                          True      -> [d1!!(i-1)] ++ dupHelper (take (i-1) d1 ++ drop i d1) (take (i-1) d2 ++ drop i d2)
                          otherwise -> []
                     else
                        []

dupForKey :: Int -> Dset -> Dset
dupForKey _ []       = []
dupForKey _ [d]      = [d]
dupForKey i (d:dset) = map (dupForKey2 i d) dset ++ dupForKey i dset

joinForAll2 :: Dset -> Dset -> Dset
joinForAll2 dset1 dset2 = map (++ replicate (length (head dset2)) "") (dupForAll dset1) ++ map (replicate (length (head dset1)) "" ++) (dupForAll dset2)

joinForAll :: [Dset] -> Dset
joinForAll []           = []
joinForAll (d:[])       = d
joinForAll (d1:d2:dset) = joinForAll (joinForAll2 d1 d2 : dset)

keymerge2 :: Int -> [D] -> Int -> [D] -> Dset
keymerge2 i1 d1 i2 d2 = if d1!!(i1-1) == d2!!(i2-1) then
                           [[d1!!(i1-1)] ++ take (i1-1) d1 ++ drop i1 d1 ++ take (i2-1) d2 ++ drop i2 d2]
                        else
                           [[d1!!(i1-1)] ++ take (i1-1) d1 ++ drop i1 d1 ++ replicate ((length d2)-1) "", 
                            [d2!!(i2-1)] ++ replicate ((length d1)-1) "" ++ take (i2-1) d2 ++ drop i2 d2]

joinForKey2 :: (Dset,Int) -> (Dset,Int) -> Dset
--joinForKey2 (dset1,i1) (dset2,i2) = merge2 (doubleListRotate [fstList (map (moveEleOut i1) (dupForAll dset1)) ++ fstList (map (moveEleOut i2) (dupForAll dset2))]) (joinForAll2 (sndList (map (moveEleOut i1) (dupForAll dset1))) (sndList (map (moveEleOut i2) (dupForAll dset2))))
joinForKey2 ([],_) _                 = []
joinForKey2 _ ([],_)                 = []
joinForKey2 (d1:dset1,i1) (dset2,i2) = dupForAll (dupForKey 1 ((dupForAll (unfold (map (keymerge2 i1 d1 i2) (dupForAll dset2))) ++ joinForKey2 (dupForAll dset1,i1) (dupForAll dset2,i2))))

joinForKey :: [(Dset,Int)] -> Dset
joinForKey [] = []
joinForKey (d:[]) = fst d
joinForKey (d1:d2:dset) = joinForKey ((joinForKey2 d1 d2,1):dset)
