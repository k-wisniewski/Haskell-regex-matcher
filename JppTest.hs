import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad(liftM2, liftM, forM, forM_)
import Control.Applicative((<$>),(<*>))

import Mon
import Reg
import RegExtra

data XYZ = X | Y | Z deriving (Eq,Ord,Show)

instance Arbitrary XYZ where
  arbitrary = oneof $ fmap return [X, Y, Z]

instance (Eq c,Arbitrary c) => Arbitrary (Reg c) where
  arbitrary = sized arb where
    arb 0 = oneof [return Eps, return Empty]
    arb 1 = Lit <$> arbitrary
    arb n = oneof [Many <$> arb2, liftM2 (:>) arb2 arb2, liftM2 (:|) arb2 arb2] where
      arb1 = arb (n-1)
      arb2 = arb (n `div` 2)

arbitraryExp :: Gen (Reg XYZ)
arbitraryExp = arbitrary

arbitraryLet = oneof $ fmap return [X, Y, Z]

getLet = do
            sg <- newStdGen
            return $ unGen arbitraryLet sg

arbitraryWord :: Gen [XYZ]
arbitraryWord = sized gn where
  gn 0 = return $ []
  gn n = liftM2 (:) (arbitraryLet) (gn (n-1))

randomWord size = do
                    sg <- newStdGen
                    return $ unGen arbitraryWord sg size

randomExp size = do
                  sg <- newStdGen
                  return $ unGen arbitraryExp sg size


genRegs size counter eachSizeCount =
  if size == -1 then
    []
  else if counter > 0 then
    ((:) (randomExp size)) (genRegs size (counter -1) eachSizeCount)
  else
    (genRegs (size - 1) eachSizeCount eachSizeCount)

genWords size counter eachSizeCount =
  if size == -1 then
    []
  else if counter > 0 then
    ((:) (randomWord size)) (genWords size (counter -1) eachSizeCount)
  else
    (genWords (size - 1) eachSizeCount eachSizeCount)

expMaxLen = 100
expEachSizeCount = 10

wordMaxLen = 10
wordEachSizeCount = 10

main = jppTest

jppTest :: IO ()
jppTest = do
  setStdGen (mkStdGen 42)

  ls <- sequence $ genRegs expMaxLen expEachSizeCount expEachSizeCount
--  forM_ ls (\x -> print x)

  words <- sequence $ genWords wordMaxLen wordEachSizeCount wordEachSizeCount
--  forM_ words (\x -> print x)

  putStrLn "tests nullable"
  forM_ ls (\x -> print $ nullable x)

  putStrLn "tests empty"
  forM_ ls (\x -> print $ empty x)

  putStrLn "tests accepts"
  forM_ ([(r, w) | r <- ls, w <- words]) (\p -> print $ accepts (fst p) (snd p))

  putStrLn "tests mayStart"
  forM_ ls (\r -> print $ mayStart X r)
  forM_ ls (\r -> print $ mayStart Y r)
  forM_ ls (\r -> print $ mayStart Z r)

  putStrLn "tests match"
  forM_ ([(r, w) | r <- ls, w <- words]) (\p -> print $ match (fst p) (snd p))


  putStrLn "tests search"
  forM_ ([(r, w) | r <- ls, w <- words]) (\p -> print $ search (fst p) (snd p))

  putStrLn "tests findall"
  forM_ ([(r, w) | r <- ls, w <- words]) (\p -> print $ findall (fst p) (snd p))
