import System.Environment ( getArgs, getProgName )
import Data.Map (fromListWith, toList)
import Data.List ( sortBy )
import System.Exit ( die )
import Data.Char ( isLower, isSpace, toLower, isAlphaNum )
import Data.Set (fromList, delete, toList, filter, map, take)
import qualified Data.Set as Data.Set.Internal

{-

Problem 2: Word Ladder

Write a program that takes the name of a dictionary file and starting
and ending words and constructs the shortest "word ladder," i.e., a
sequence of dictionary words that differ by exactly one changed
character that transforms the starting word into the ending word.
Unlike some word ladder puzzles, your program should not consider
adding or removing letters.

Your program should read a Unix-style /usr/dict/words file (i.e., a
sorted, newline-separated list of valid words), and only consider
lowercase-only words (i.e., no capitalized proper nouns).  You may
also ignore words with punctuation, e.g., abase is fine, but ignore
aardvark's, ASCII, and Abby.

The user must supply a valid dictionary file and start and end words
that are the same length, all lowercase, and both in the dictionary.
Your program should print an error message and terminate with a
non-zero error code if supplied with erroneous arguments.

Search for ladders of at most 20 words, returning an error message if
one cannot be found with the given dictionary.  For a given pair of
words and dictionary, there is often more than one minimum-length
ladder; you may return any of them.

Here is an example run; your program should work with other
dictionaries, too.

$ wget https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt
$ stack --resolver lts-18.14 ghc -- --make -Wall -O wordLadder.hs
$ ./wordLadder words.txt fool sage
fool
foot
fort
fore
fare
fage
sage
$ ./wordLadder /usr/share/dict/words computer solution
Unable to find a ladder in 20
$ ./wordLadder
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar baz
wordLadder: foo: openFile: does not exist (No such file or directory)
$ ./wordLadder /usr/share/dict/words foo bar
"foo" not in dictionary
$ ./wordLadder /usr/share/dict/words bar foo
"foo" not in dictionary
$ ./wordLadder /usr/share/dict/words bar none
"bar" and "none" must be the same length

As for the wordFreq problem, make your solution correct, readable, and
fast (in order of importance).  Again, we will classify the
performance of your solution into one of three categories: noticably
faster than our reference solution, about the same, and noticably
slower.  For reference, the fool->sage example above took about 220 ms
on my desktop machine.

Our 68-line reference solution uses a BFS-style search implemented
with functions from System.IO, System.Exit, Data.Char,
System.Environment, Data.Set, Control.Monad, and Data.List.

-}

data Node = Node String [Node] deriving (Show, Eq)

toWordDict :: [Char] -> Int -> Data.Set.Internal.Set [Char]
toWordDict x n = Data.Set.fromList (Prelude.filter (\y -> length y == n) (words (Prelude.filter (\y -> isAlphaNum y || isSpace y) (Prelude.map toLower x))))

stringIsLower :: Foldable t => t Char -> Bool
stringIsLower = all isLower

isNeighbours :: Eq a => [a] -> [a] -> Bool
isNeighbours [] [] = False
isNeighbours [] _ = False
isNeighbours _ [] = False
isNeighbours (x:xs) (y:ys) | x /= y = xs == ys
                           | otherwise = isNeighbours xs ys

searchNeighbours :: Eq a => [a] -> Data.Set.Internal.Set [a] -> [[a]]
searchNeighbours x wordDict = Data.Set.toList(Data.Set.filter (\y -> isNeighbours x y) wordDict)

graph :: Data.Set.Internal.Set String -> [Char] -> Node
graph wordDict x = Node x (Prelude.map (graph (Data.Set.delete x wordDict)) (searchNeighbours x wordDict))


main :: IO ()
main = do
  args <- getArgs
  (fileName, fromWord, toWord) <- case args of
    [x, y ,z] | stringIsLower y && stringIsLower z -> return (x,y,z)
    [x, y ,z] | otherwise -> die "Usage: wordLadder <dictionary-filename> <from-word> <to-word>"
    _ -> die "Usage: wordLadder <dictionary-filename> <from-word> <to-word>"
  contents <- readFile fileName
  print (graph(Data.Set.take 40 (toWordDict contents (length fromWord))) "abc")