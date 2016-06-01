module Samples where

import Data.List
import Syntax
import Helpers
import Helpers2
import Domains
import BasicSemantics
import UISemantics
import Main

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
            output "showChangeValue.csv" cval
