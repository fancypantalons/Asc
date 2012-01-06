module Heap (HeapInfo, newHeap, coalesce, allocate, free) where

import Data.List

data HeapInfo = HeapInfo { freelist :: ! [(Integer, Integer)], blocks :: ! [(Integer, Integer)] }
              deriving Show

newHeap :: Integer -> HeapInfo
newHeap size = HeapInfo { freelist = [(size, size)], blocks = [] }

coalesce :: [(Integer, Integer)] -> [(Integer, Integer)]
coalesce [] = []
coalesce [blk] = [blk]
coalesce (ablk@(ablkaddr, ablksize):bblk@(bblkaddr, bblksize):rest) 
  | (ablkaddr - ablksize) == bblkaddr 
    = coalesce ((ablkaddr, ablksize + bblksize):rest)
  | otherwise                         
    = ablk:(coalesce $ bblk:rest)

allocate :: HeapInfo -> Integer -> (HeapInfo, Integer)
allocate heap size = (heap { freelist = newfree, blocks = newblocks }, (blkaddr - size))
  where (head, ((blkaddr, blksz):tail)) = break (\(_, sz) -> sz > size) $ freelist heap
        newfree   = head ++ (((blkaddr - size), (blksz - size)):tail)
        newblocks = (blkaddr, size):(blocks heap)

free :: HeapInfo -> Integer -> HeapInfo
free heap addr = heap { freelist = newfree, blocks = newblocks }
  where Just blksz   = lookup addr $ map (\(baddr, bsize) -> ((baddr - bsize), bsize)) $ blocks heap
        blkaddr      = addr + blksz
        (head, tail) = break (\(a, _) -> a < blkaddr) $ freelist heap
        newfree      = coalesce $ head ++ ((blkaddr, blksz):tail)
        newblocks    = filter (\(a, _) -> a /= blkaddr) $ blocks heap
