{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.Bipartite.AdjacencyMap

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.List.NonEmpty as NE

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Either        (isRight)
import Data.Foldable
import Data.List          (splitAt)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Maybe         (isJust, fromJust)
import System.Random      (mkStdGen)

import Criterion.Main


data Part = LeftPart | RightPart
    deriving (Show, Eq)


otherPart :: Part -> Part
otherPart LeftPart  = RightPart
otherPart RightPart = LeftPart


detectParts_Maybe :: Ord a => AM.AdjacencyMap a -> Maybe (AdjacencyMap a a)
detectParts_Maybe g = let s = AM.symmetricClosure g
                       in build g <$> (foldM (runDfs s) Map.empty $ AM.vertexList s)
    where
        dfs :: Ord a => Part -> AM.AdjacencyMap a -> Map.Map a Part -> a -> Maybe (Map.Map a Part)
        dfs p g m v = foldM (action p g) (Map.insert v p m) $ neighbours v g

        action :: Ord a => Part -> AM.AdjacencyMap a -> Map.Map a Part -> a -> Maybe (Map.Map a Part)
        action p g m v = case v `Map.lookup` m of
                              Nothing -> dfs (otherPart p) g m v
                              Just q  -> if q /= p then Just m else Nothing

        runDfs :: Ord a => AM.AdjacencyMap a -> Map.Map a Part -> a -> Maybe (Map.Map a Part)
        runDfs g m v = (m <$ Map.lookup v m) <|> dfs LeftPart g m v

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> Set.Set a
        neighbours v g = fromJust $ v `Map.lookup` AM.adjacencyMap g

        build :: Ord a => AM.AdjacencyMap a -> Map.Map a Part -> AdjacencyMap a a
        build g m = toBipartite $ AM.gmap (toEither m) g

        toEither :: Ord a => Map.Map a Part -> a -> Either a a
        toEither m v = case fromJust (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v


type PartMap a = Map.Map a (Part, NE.NonEmpty a)
type PartMonad a = MaybeT (State (PartMap a)) [a]


partMonad :: Ord a => AM.AdjacencyMap a -> PartMonad a
partMonad g = let s = AM.symmetricClosure g
               in msum $ map (runDfs s) $ AM.vertexList s
    where
        {-# INLINE action #-}
        action :: Ord a => AM.AdjacencyMap a -> Part -> NE.NonEmpty a -> a -> a -> PartMonad a
        action g p l u v = do m <- get
                              case v `Map.lookup` m of
                                   Just pv -> maybeOddCycle pv (otherPart p, l)
                                   Nothing -> dfs g p (v <| l) v

        dfs :: Ord a => AM.AdjacencyMap a -> Part -> NE.NonEmpty a -> a -> PartMonad a
        dfs g p l v = do modify' $ Map.insert v (p, l)
                         asum $ map (action g (otherPart p) l v) $ neighbours v g

        runDfs :: Ord a => AM.AdjacencyMap a -> a -> PartMonad a
        runDfs g v = do m <- get
                        guard $ v `Map.notMember` m
                        dfs g LeftPart (v :| []) v

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> [a]
        neighbours v = Set.toAscList . fromJust . Map.lookup v . AM.adjacencyMap

        maybeOddCycle :: Ord a => (Part, NE.NonEmpty a) -> (Part, NE.NonEmpty a) -> PartMonad a
        maybeOddCycle (p, lu) (q, lv) | p == q    = return $ oddCycle lu lv
                                      | otherwise = mzero

        oddCycle :: Ord a => NE.NonEmpty a -> NE.NonEmpty a -> [a]
        oddCycle x y = cropHeads (NE.reverse x) (NE.reverse y)

        cropHeads :: Ord a => NE.NonEmpty a -> NE.NonEmpty a -> [a]
        cropHeads (_:|[]) ys = NE.toList ys
        cropHeads xs (_:|[]) = NE.toList xs
        cropHeads xs@(_:|x':xt) (_:|y':yt) | x' == y'  = cropHeads (x':|xt) (y':|yt)
                                           | otherwise = (NE.toList xs) ++ reverse (NE.toList $ y':|yt)


detectParts_PartMonad :: Ord a => AM.AdjacencyMap a -> Either [a] (AdjacencyMap a a)
detectParts_PartMonad = originalDetectParts
    where
        originalDetectParts :: Ord a => AM.AdjacencyMap a -> Either [a] (AdjacencyMap a a)
        originalDetectParts g = case runState (runMaybeT $ partMonad g) Map.empty of
                                     (Nothing, m) -> Right $ build m g
                                     (Just c, _)  -> Left c

        build :: Ord a => PartMap a -> AM.AdjacencyMap a -> AdjacencyMap a a
        build m g = toBipartite $ AM.gmap (toEither m) g

        toEither :: Ord a => PartMap a -> a -> Either a a
        toEither m v = case (fst . fromJust) (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v


detectParts_Either :: Ord a => AM.AdjacencyMap a -> Either [a] (AdjacencyMap a a)
detectParts_Either = originalDetectParts
    where
        originalDetectParts g = let s = AM.symmetricClosure g
                                 in build g <$> (foldM (runDfs s) Map.empty $ AM.vertexList s)

        dfs :: Ord a => Part
                     -> NE.NonEmpty a
                     -> AM.AdjacencyMap a
                     -> PartMap a
                     -> a
                     -> Either [a] (PartMap a)
        dfs p l g m v = foldM (action p l g) (Map.insert v (p, l) m) $ neighbours v g

        action :: Ord a => Part 
                        -> NE.NonEmpty a
                        -> AM.AdjacencyMap a
                        -> PartMap a
                        -> a
                        -> Either [a] (PartMap a)
        action p l g m v = case v `Map.lookup` m of
                                Nothing       -> dfs (otherPart p) (v <| l) g m v
                                Just (q, l')  -> if q /= p
                                                 then Right m
                                                 else Left $ oddCycle l l'

        runDfs :: Ord a => AM.AdjacencyMap a
                        -> PartMap a
                        -> a
                        -> Either [a] (PartMap a)
        runDfs g m v = case Map.lookup v m of
                            Just _  -> Right m
                            Nothing -> dfs LeftPart (v :| []) g m v

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> Set.Set a
        neighbours v g = fromJust $ v `Map.lookup` AM.adjacencyMap g

        build :: Ord a => AM.AdjacencyMap a -> PartMap a -> AdjacencyMap a a
        build g m = toBipartite $ AM.gmap (toEither m) g

        toEither :: Ord a => PartMap a -> a -> Either a a
        toEither m v = case (fst . fromJust) (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v

        oddCycle :: Ord a => NE.NonEmpty a -> NE.NonEmpty a -> [a]
        oddCycle x y = cropHeads (NE.reverse x) (NE.reverse y)

        cropHeads :: Ord a => NE.NonEmpty a -> NE.NonEmpty a -> [a]
        cropHeads (_:|[]) ys = NE.toList ys
        cropHeads xs (_:|[]) = NE.toList xs
        cropHeads xs@(_:|x':xt) (_ :| y':yt) | x' == y'  = cropHeads (x':|xt) (y':|yt)
                                             | otherwise = (NE.toList xs) ++ reverse (NE.toList $ y':|yt)


type PartMap' a = Map.Map a Part
type PartMonad' a = MaybeT (State (PartMap' a)) [a]
type DFSMonad a = MaybeT (State (PartMap' a)) a


partMonad' :: Ord a => AM.AdjacencyMap a -> DFSMonad a
partMonad' g = asum $ map (runDfs g) $ AM.vertexList g
    where
        {-# INLINE action #-}
        action :: Ord a => AM.AdjacencyMap a -> Part -> a -> DFSMonad a
        action g p v = do m <- get
                          case v `Map.lookup` m of
                               Just q  -> if q /= p then return v else mzero
                               Nothing -> dfs g p v

        dfs :: Ord a => AM.AdjacencyMap a -> Part -> a -> DFSMonad a
        dfs g p v = do modify' $ Map.insert v p
                       asum $ map (action g (otherPart p)) $ neighbours v g

        runDfs :: Ord a => AM.AdjacencyMap a -> a -> DFSMonad a
        runDfs g v = do m <- get
                        guard $ v `Map.notMember` m
                        dfs g LeftPart v

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> [a]
        neighbours v = Set.toAscList . fromJust . Map.lookup v . AM.adjacencyMap


oddCycle :: Ord a => a -> AM.AdjacencyMap a -> Maybe [a]
oddCycle v g = evalState (runMaybeT $ dfs g LeftPart v v) Map.empty
    where
        dfs :: Ord a => AM.AdjacencyMap a -> Part -> a -> a -> PartMonad' a
        dfs g p u v | u == v && p /= LeftPart = return []
                    | otherwise = ((:) v) <$> do m <- get
                                                 guard $ v `Map.notMember` m
                                                 modify' $ Map.insert v p
                                                 let q = otherPart p
                                                 asum $ map (dfs g q u) (neighbours v g)

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> [a]
        neighbours u = Set.toAscList . fromJust . Map.lookup u . AM.adjacencyMap


detectParts_PartMonad' :: Ord a => AM.AdjacencyMap a -> Either [a] (AdjacencyMap a a)
detectParts_PartMonad' g = let s = AM.symmetricClosure g
                            in case runState (runMaybeT $ partMonad' s) Map.empty of
                                    (Nothing, m) -> Right $ build m g
                                    (Just c, _)  -> Left $ fromJust $ oddCycle c s
    where
        build :: Ord a => PartMap' a -> AM.AdjacencyMap a -> AdjacencyMap a a
        build m g = toBipartite $ AM.gmap (toEither m) g

        toEither :: Ord a => PartMap' a -> a -> Either a a
        toEither m v = case fromJust (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v


detectParts_Either' :: Ord a => AM.AdjacencyMap a -> Either [a] (AdjacencyMap a a)
detectParts_Either' g = let s = AM.symmetricClosure g
                         in case (eitherMonad s) of
                                 Left c  -> Left  $ fromJust $ oddCycle c s
                                 Right m -> Right $ build m g
    where
        eitherMonad :: Ord a => AM.AdjacencyMap a -> Either a (PartMap' a)
        eitherMonad g = foldM (runDfs g) Map.empty $ AM.vertexList g

        dfs :: Ord a => Part -> AM.AdjacencyMap a -> PartMap' a -> a -> Either a (PartMap' a)
        dfs p g m v = foldM (action p g) (Map.insert v p m) $ neighbours v g

        action :: Ord a => Part -> AM.AdjacencyMap a -> PartMap' a -> a -> Either a (PartMap' a)
        action p g m v = case v `Map.lookup` m of
                              Nothing -> dfs (otherPart p) g m v
                              Just q  -> if q /= p
                                         then Right m
                                         else Left v

        runDfs :: Ord a => AM.AdjacencyMap a -> PartMap' a -> a -> Either a (PartMap' a)
        runDfs g m v = case Map.lookup v m of
                            Just _  -> Right m
                            Nothing -> dfs LeftPart g m v

        neighbours :: Ord a => a -> AM.AdjacencyMap a -> Set.Set a
        neighbours u = fromJust . Map.lookup u . AM.adjacencyMap

        build :: Ord a => PartMap' a -> AM.AdjacencyMap a -> AdjacencyMap a a
        build m g = toBipartite $ AM.gmap (toEither m) g

        toEither :: Ord a => PartMap' a -> a -> Either a a
        toEither m v = case fromJust (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v


btree :: Int -> AM.AdjacencyMap Int
btree d = lefts d + rights d
    where
        lefts  d = AM.edges [(x, 2 * x)     | x <- [1..(shift 1 (d - 1))]]
        rights d = AM.edges [(x, 2 * x + 1) | x <- [1..(shift 1 (d - 1))]]


btreeOddCycles :: Int -> AM.AdjacencyMap Int
btreeOddCycles d = btree d + AM.edges (getZipList ((,) <$> ZipList [p (d - 1)..3 * p (d - 2)] <*> ZipList [3 * p (d - 2)..p d]))
    where
        p d = shift 1 d


ttree :: Int -> AM.AdjacencyMap Int
ttree d = sideEdges d 1 + sideEdges d 2 + sideEdges d 3
    where
        sideEdges :: Int -> Int -> AM.AdjacencyMap Int
        sideEdges d i = AM.edges [ (x, 3 * x + i) | x <- [0..(pow3 d) - 1] ]

        pow3 :: Int -> Int
        pow3 0 = 1
        pow3 d | d `mod` 2 == 0 = let t = pow3 (d `div` 2) in t * t
               | d `mod` 2 == 1 = 3 * pow3 (d - 1)


grid :: Int -> AM.AdjacencyMap Int
grid n = AM.edges $ [ (i * n + j, i * n + j + 1) | i <- [0..n - 1], j <- [0..n - 2] ]
                 ++ [ (i * n + j, i * n + j + n) | i <- [0..n - 2], j <- [0..n - 1] ]


caterpillar :: Int -> AM.AdjacencyMap Int
caterpillar n = AM.edges $ [ (2 * i, 2 * i + 1) | i <- [0..n - 1] ]
                        ++ [ (2 * i, 2 * i + 2) | i <- [0..n - 1] ]

shuffle :: Int -> [a] -> [a]
shuffle _    [] = []
shuffle seed xs = evalRand (run xs) (mkStdGen seed)
    where
        change :: Int -> a -> [a] -> [a]
        change i x xs = let (left, _:right) = splitAt i xs
                         in left ++ [x] ++ right

        run :: RandomGen g => [a] -> Rand g [a]
        run (x:xs) = foldM action [x] xs

        action :: RandomGen g => [a] -> a -> Rand g [a]
        action xs x = do i <- getRandomR (0, (length xs) - 1)
                         let y = xs !! i
                         return $ y:(change i x xs)


evalMaybe :: Ord a => Maybe (AdjacencyMap a a) -> Int
evalMaybe (Just g) = edgeCount g
evalMaybe _        = 0


evalEither :: Ord a => Either [a] (AdjacencyMap a a) -> Int
evalEither (Left xs) = length xs
evalEither (Right g) = edgeCount g


benchmarks :: Ord a => AM.AdjacencyMap a -> [Benchmark]
benchmarks g = [ bench "Maybe"      $ nf (isJust  . detectParts_Maybe)      g
               , bench "PartMonad'" $ nf (isRight . detectParts_PartMonad') g
               , bench "Either'"    $ nf (isRight . detectParts_Either')    g
               , bench "PartMonad"  $ nf (isRight . detectParts_PartMonad)  g
               , bench "Either"     $ nf (isRight . detectParts_Either)     g
               , bench "Maybe + restore"      $ nf (evalMaybe  . detectParts_Maybe)      g
               , bench "PartMonad' + restore" $ nf (evalEither . detectParts_PartMonad') g
               , bench "Either' + restore"    $ nf (evalEither . detectParts_Either')    g
               , bench "PartMonad + restore"  $ nf (evalEither . detectParts_PartMonad)  g
               , bench "Either + restore"     $ nf (evalEither . detectParts_Either)     g
               ]


main :: IO ()
main = defaultMain [ bgroup "clique/2000"          $ benchmarks $ AM.clique $ shuffle 179 [1..2000]
                   , bgroup "path/1000000"         $ benchmarks $ AM.path [1..1000000]
                   , bgroup "btree/20"             $ benchmarks $ btree 20
                   , bgroup "ttree/12"             $ benchmarks $ ttree 12
                   , bgroup "caterpillar/1000000"  $ benchmarks $ caterpillar 500000
                   , bgroup "grid/750"             $ benchmarks $ grid 750
                   , bgroup "biclique/1500"        $ benchmarks $ AM.biclique (Left <$> shuffle 179 [1..1500]) (Right <$> shuffle 239 [1..1500])
                   , bgroup "oddCycle/1000001"     $ benchmarks $ AM.path [1..1000001] + AM.edge 1 1000001
                   , bgroup "pathTriangle/1000001" $ benchmarks $ AM.path [1..999999] + 999999 * 1000000 * 1000001
                   , bgroup "btreeOddCycles/20"    $ benchmarks $ btreeOddCycles 20
                   ]
