import qualified BHeap as BH
import Common
import Control.Exception
import Control.Monad
import Data.List (isInfixOf)
import System.FilePath
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main =
  runTests
    [ bheap_manual,
      bheap_props
    ]

-------------------------------------------------------------------------------

-- | Problem 1 ----------------------------------------------------------------

-------------------------------------------------------------------------------

bheap_manual :: Score -> TestTree
bheap_manual sc =
  testGroup
    "Problem 1"
    [ mkTest
        (uncurry BH.insert)
        (11, BH.Leaf)
        h0
        "insert 11 Leaf",
      mkTest
        (uncurry BH.insert)
        (5, h1)
        h2
        "insert 5 h1",
      mkTest
        BH.isHeapShape
        h1
        True
        "isHeapShape h1",
      mkTest
        BH.isHeapShape
        t1
        False
        "isHeapShape t1",
      mkTest
        BH.isHeapValue
        h1
        True
        "isHeapValue h1",
      mkTest
        BH.isHeapValue
        t2
        False
        "isHeapValue t2",
      mkTest
        (uncurry BH.containsAll)
        ([3, 8, 11], h1)
        True
        "containsAll [3,8,11] h1",
      mkTest
        (uncurry BH.containsAll)
        ([3, 5, 11], h1)
        False
        "containsAll [3,5,11] h1"
    ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (\x -> return (f x))

    h0 = BH.Node 11 True BH.Leaf BH.Leaf
    h1 =
      BH.Node
        11
        True
        (BH.Node 3 True BH.Leaf BH.Leaf)
        (BH.Node 8 True BH.Leaf BH.Leaf)
    h2 =
      BH.Node
        11
        False
        ( BH.Node
            5
            False
            (BH.Node 3 True BH.Leaf BH.Leaf)
            BH.Leaf
        )
        (BH.Node 8 True BH.Leaf BH.Leaf)

    t1 =
      BH.Node
        11
        True
        BH.Leaf
        (BH.Node 8 True BH.Leaf BH.Leaf)
    t2 =
      BH.Node
        11
        True
        (BH.Node 15 True BH.Leaf BH.Leaf)
        (BH.Node 8 True BH.Leaf BH.Leaf)

bheap_props :: Score -> TestTree
bheap_props sc =
  testGroup
    "Problem 1 (quickcheck)"
    [ scoreProp sc ("prop_full_correct", BH.prop_full_correct, 10),
      scoreProp sc ("prop_is_heap_shape", BH.prop_is_heap_shape, 10),
      scoreProp sc ("prop_is_heap_value", BH.prop_is_heap_value, 10),
      scoreProp sc ("prop_contains_elts", BH.prop_contains_elts, 10)
    ]
