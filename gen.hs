import Control.Monad
import Data.List
import Test.QuickCheck.Gen

generateData n = liftM (nub . sort . concat) $ sample' $ resize n $ listOf1 $ resize 20 $ listOf1 $ choose ('a', 'c')

writeIt n = generateData n >>= \l -> writeFile "test" (show l)
