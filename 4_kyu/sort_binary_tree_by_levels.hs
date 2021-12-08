-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc

module TreeByLevels where

import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           TreeByLevels.TreeNode

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels n = reverse . map (value . fromJust) $ filter isJust $ concatMap reverse $ getLevels [[n]]

getLevels :: [[Maybe (TreeNode a)]] -> [[Maybe (TreeNode a)]]
getLevels []         = []
getLevels ns@(n : _) = if isEmptyLevel nextLevel then ns else getLevels $ nextLevel : ns
  where nextLevel = concatMap getNextLevel n

getNextLevel :: Maybe (TreeNode a) -> [Maybe (TreeNode a)]
getNextLevel Nothing     = [Nothing]
getNextLevel (Just node) = [left node, right node]

isEmptyLevel :: [Maybe a] -> Bool
isEmptyLevel []             = True
isEmptyLevel (Nothing : ns) = isEmptyLevel ns
isEmptyLevel (Just _  : _ ) = False
