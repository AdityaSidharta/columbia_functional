import System.Environment ( getArgs, getProgName )
import Data.Map (fromListWith, toList)
import Data.List ( sortBy )
import System.Exit ( die )
import Data.Char ( isLower, isSpace, toLower )
{-

Problem 1: Word Frequency Counter

Write a program that prints the 40 most common words and the number of
times they appear in the given text file.  Compile this into a
standalone executable that takes a single filename as a command-line
argument.  Print a usage message if the user does not supply exactly
one argument.

To find words, discard all non-alphabetic characters aside from
whitespace and treat what's left as lowercase.  E.g., treat "Stephen's
humor-challenged" as two words: "stephens" and "humorchallenged"

You may use any of the System or Data modules.  Ask permission from
the instructor on Piazza before you use any other modules.  You may
not use the Data.Text.WordCount module.

In my 32-line reference solution, I used Data.Map.fromListWith,
Data.List.sortBy, readFile, System.Exit.die,
System.Environment.getArgs, and System.Environment.getProgName.

In addition to making your solution correct and readable, try to make
your solution go fast, but leave it single-threaded.  We will classify
solutions into three performance categories and assign 10% of the
score based on these categories:

1) Roughly the same as my reference solution (about 2s on the
   Shakespeare example);
2) Noticably faster than the reference solution
3) Noticably slower than the reference solution

E.g., on the Complete Works of Shakespeare

$ stack --resolver lts-18.14 ghc -- --make -Wall -O wordFreq
$ ./wordFreq
Usage: wordFreq <filename>
$ wget http://www.gutenberg.org/files/100/100-0.txt
$ ./wordFreq 100-0.txt

30205 the
28386 and
21949 i
20923 to
18822 of
16183 a
14437 you
13180 my
12232 in
11776 that
9713 is
9066 not
8528 with
8263 me
8195 for
8180 it
7581 his
7370 be
7178 this
7076 your
6819 he
6747 but
6267 have
6176 as
5842 thou
5549 him
5451 so
5287 will
4751 what
4598 her
4359 thy
4207 all
4086 by
4074 no
3911 do
3846 shall
3802 if
3727 are
3551 we
3387 thee
-}

toWords :: String -> [String]
toWords x = words (filter (\y -> isLower y || isSpace y) (map toLower x))

countSort :: (Ord a1, Ord a2, Num a1) => [a2] -> [(a2, a1)]
countSort x = sortBy (flip (\(_, a) (_, b) -> compare a b)) (toList(fromListWith (+) (map (\y -> (y, 1)) x)))


prettyPrint :: Show a => [([Char], a)] -> [[Char]]
prettyPrint = map (\(x, y) -> show y ++ " " ++ x)

main :: IO ()
main = do
   args <- getArgs
   progName <- getProgName
   filename <- case args of
    [p] -> return p
    _ -> die $ progName ++ ": Wrong Number of Arguments"
   contents <- readFile filename
   mapM_ putStrLn (take 40 (prettyPrint $ countSort $ toWords contents))