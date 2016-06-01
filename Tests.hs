module Tests where

import Data.List
import Syntax
import Helpers
import Helpers2
import Domains
import BasicSemantics
import UISemantics
import Main

--Syntax test
dset1 :: Dset
dset1 = [["0","1","2","3","4"], ["5","6","7","8","9"], ["10","11","12","13","14"], ["15","16","17","18","19"]]
--dset1 = do 
--          d1 <- readFile "input.csv"
--        input "input1.csv" d1

dset2 :: Dset
dset2 = [["1","1.2","abc","True"], ["2","2.3","def","False"], ["3","3.4","ghi","True"], ["4","4.5","jkl","False"]]

dset3 :: Dset
dset3 = [["Jingyuan","Yes"], ["Partner","Yeah!"], ["Professor","True"]]

dset4 :: Dset
dset4 = [["1.5","1","weifdsoa"], ["2.6","2","fwinidavf"], ["3.7","3","uidhaifif"], ["4.8","4","djsanfaf"], ["3.7","3","uidhaifif"]]

header1 :: Header
header1 = [("mod5=0",INTEGER), ("mod5=1",INTEGER), ("mod5=2",INTEGER), ("mod5=3",INTEGER), ("mod5=4",INTEGER)]

header2 :: Header
header2 = [("Integer",INTEGER), ("Float",FLOAT), ("String",STRING), ("Boolean", BOOLEAN)]

header3 :: Header
header3 = [("Full Name",STRING), ("isInCovallis?",BOOLEAN)]

--Domain test
syndict :: SynDict
syndict = [("Jingyuan Xu",["Jingyuan","Xu, Jingyuan"]), ("Hao Wang",["Partner","Hao","Wang, Hao"]), ("Martin Erwig",["Erwig, Martin","Mr. Erwig","Professor"]), ("True",["Yes","Yeah!"])]

vset1 :: Vset
vset1 = dsetToVset dset1 header1

vset2 :: Vset
vset2 = dsetToVset dset2 header2

syncheck :: Dset
syncheck = synSetReplace syndict dset3

vset3 :: Vset
vset3 = dsetToVset (synSetReplace syndict dset3) header3

dtovtod1 :: Dset
dtovtod1 = vsetToDset vset1

dtovtod2 :: Dset
dtovtod2 = vsetToDset vset2

dtov3 :: Dset
dtov3 = vsetToDset vset3

hset1 :: Hset
hset1 = dsetToHset dset1 header1

hset2 :: Hset
hset2 = dsetToHset dset2 header2

hset3 :: Hset
hset3 = dsetToHset dset1 header2

dtohtod1 :: Dset
dtohtod1 = hsetToDset hset1

dtohtod2 :: Dset
dtohtod2 = hsetToDset hset2

dtohtod3 :: Dset
dtohtod3 = hsetToDset hset3

--BasicSemantics test
dcol1 :: Dset
dcol1 = deleteColumn [2,3] dset2

dcol2 :: Dset
dcol2 = deleteColumn [3] dset2

drow1 :: Dset
drow1 = deleteRow [3] dset1

dval1 :: Vset
dval1 = deleteValue (3, Left "Float") vset2

dval2 :: Vset
dval2 = deleteValue (2, Right 3) vset2

ctcol1 :: Vset
ctcol1 = changeColumnType INTEGER (Left "Float") vset2

ctcol2 :: Vset
ctcol2 = changeColumnType FLOAT (Right 3) vset1

ctrow1 :: Hset
ctrow1 = changeRowType INTEGER 2 hset2

ctval1 :: Vset
ctval1 = changeValueType INTEGER (3, Left "Float") vset2

ctval2 :: Vset
ctval2 = changeValueType INTEGER (2, Right 3) vset2

{-
ccol1 :: Vset
ccol1 = changeColumn (>3) (Left "Float") vset2

ccol2 :: Vset
ccol2 = changeColumn (+5) (Right 3) vset1

crow1 :: Hset
crow1 = changeRow (+2) 2 hset2

cval1 :: Vset
cval1 = changeValue (>3) (3, Left "Float") vset2

cval2 :: Vset
cval2 = changeValue (+5) (2, Right 3) vset1
-}

cvalp1 :: Dset
cvalp1 = changeValueByPos [(3,2), (2,3)] ["12.34",""] dset2

cvals1 :: Dset
cvals1 = changeValueBySearch ["4.5","abc","xyz"] ["12.34","feuianfnf","opq"] dset2

--UISemantics test
joinForAll1 :: Dset
joinForAll1 = joinForAll [dset1, dset3, dset4]

joinForKey1 :: Dset
joinForKey1 = joinForKey [(dset2,1), (dset4,2)]
