
import Data.List(nub)
import Data.Ratio
import Control.Monad
import Control.Monad.Random

import Test.QuickCheck
import Test.QuickCheck.Gen

import System.Random
import System

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.PatriciaTree

import Names
import Topics

numEntities = 1000
followMax   = 7
respectMax  = 10

type ProbTopic = (Topic, Ratio Int)

data Entity = Entity { name      :: String 
                     , interests :: [ProbTopic]
                     , nodeId    :: Int
                     }
                     deriving Show


instance Arbitrary Entity where
  arbitrary = do 
    nm <- elements names
    nt <- resize 5 $ listOf $ elements ([minBound .. maxBound] :: [Topic])
    let ptops = map (\x -> (x, 1 % (length nt))) nt 
    return $ Entity nm (nub ptops) 0

genEntities :: Int -> [Entity]
genEntities seed = unGen arbitrary (mkStdGen seed) 99999 

randLEdge :: (RandomGen g) => Int -> Rand g (LEdge Int)
randLEdge strt = do
  end <- getRandomR (1, numEntities)
  props <- getRandomR (1, respectMax) 
  return (strt, end, props)


randLEdges :: (RandomGen g) => Int -> Rand g [(LEdge Int)]
randLEdges strt = do
  num <- getRandomR (0,followMax) 
  replicateM num $ randLEdge strt


main = do
  let nodes  = zip [1..numEntities] $ genEntities 4242 
      edges = concat $ evalRand (mapM randLEdges [1..numEntities]) (mkStdGen 555)
      graph = mkGraph nodes edges :: Gr Entity Int
  writeFile "./testgraph1.viz" $ graphviz' graph
