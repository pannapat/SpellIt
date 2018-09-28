{-=Spellit=-}
{-=webtoaff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Pre-Alpha=-}
{-=Synopsis:  This Haskell Script will query a database,=-}
{-=tuple by tuple, and eventually create a .aff for each unique=-}
{-=session (each time "submit" is clicked on webpage).=-}

{-Imports-}

import Data.List as L
import Data.List.Split as S
import qualified Data.List.Extra as E
import Data.Maybe as M
import Database.CouchDB
import Database.HDBC
import System.Directory
import System.Environment
import System.IO.Temp
import System.IO
import Text.JSON
import Text.PrettyPrint.Boxes

{---------}

{-Custom Data Declarations.-}

data RuleHeader =
    PfxHead String Char Char Int
  | SfxHead String Char Char Int
    deriving (Eq, Ord, Show)

data Rule = 
    Pfx String Char Int String Char 
  | Sfx String Char Int String Char 
    deriving (Eq, Ord, Show)

{-------------------}

{-General Utility Functions.-}

--linefeed -> To feed file into nested list line by line, 
--delimited by newline characters.
linefeed :: String -> [String]
linefeed xs = L.lines xs

--singleunnest
singleunnest :: [a] -> a
singleunnest [xs] = xs

--tripletfst
tripletfst :: (a,b,c) -> a
tripletfst (a,b,c) = a

--tripletsnd
tripletsnd :: (a,b,c) -> b
tripletsnd (a,b,c) = b

--tripletthrd
tripletthrd :: (a,b,c) -> c
tripletthrd (a,b,c) = c

--mtripletfst
mtripletfst :: Maybe (a,b,c) -> a
mtripletfst (Just (a,b,c)) = a

--mtripletsnd
mtripletsnd :: Maybe (a,b,c) -> b
mtripletsnd (Just (a,b,c)) = b

--mtripletthrd 
mtripletthrd :: Maybe (a,b,c) -> c
mtripletthrd (Just (a,b,c)) = c

{----------------------------}


{-Functions operating to take each root and corresponding root paradigm-}
{-and prepare to feed into temporary file for aggregation.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: String -> [String] -> Int -> Int -> [(String,[Maybe (String,String,Int)])]
presufcheck []    []     _          _          = []
presufcheck (_:_) []     _          _          = []
presufcheck x     (y:ys) statestart totalstart = 
    if null (x L.\\ y) 
        then [(x,[(maybetriplet (E.stripInfix x y , 0))])] 
             ++ (presufcheck x ys statestart totalstart)
        else [(x,[maybetriplet (zipwithpaddingdiff zipper , 
              ((L.length x) - (L.length (zipwithpaddingcounter zipper statestart totalstart))))])] 
             ++ (presufcheck x ys statestart totalstart) 
                 where
                     --Local Variable Definition.--
                     zipper = (zipwithpadding "" "" (nester x) (nester y))
                     --Nested Function Definitions.--
                     --nester
                     nester :: String -> [String] 
                     nester [] = []
                     nester (x:xs) = [[x]] ++ (nester xs)
                     --maybetriplet
                     maybetriplet :: (Maybe (String,String),Int) -> Maybe (String,String,Int)
                     maybetriplet (Just (a,b),c) = Just (a,b,c) 
                     --zipwithpadding
                     zipwithpadding :: String -> String -> [String] -> [String] -> [(String,String)]
                     zipwithpadding a b (x:xs) (y:ys) = (x,y) : zipwithpadding a b xs ys
                     zipwithpadding a _ []     ys     = zip (repeat a) ys
                     zipwithpadding _ b xs     []     = zip xs (repeat b)
                     --zipwithpaddingdiff
                     zipwithpaddingdiff :: [(String,String)] -> Maybe (String,String) 
                     zipwithpaddingdiff [] = Nothing
                     zipwithpaddingdiff (x:xs) =
                         if (fst x) /= (snd x)
                             then Just ("",((snd x) ++ (concat (map (snd) xs))))
                             else zipwithpaddingdiff xs
                     --zipwithpaddingcounter
                     zipwithpaddingcounter :: [(String,String)] -> Int -> Int -> [(String,String)]
                     zipwithpaddingcounter [] state total = []
                     zipwithpaddingcounter ((x,y):xs) state total = 
                         if x == y && state == total  
                             then [(x,y)] ++ (zipwithpaddingcounter xs (state+1) (total+1))
                             else zipwithpaddingcounter xs state (total+1)                
                     --------------------------------  


--pretempfile -> Function to prepare to dump result of presufcheck into temporary file.
pretempfile :: [(String,[Maybe (String,String,Int)])] -> [String]
pretempfile [] = []
pretempfile ((a,[Just (b,c,d)]):xs) = [a,b,c,(show d)] ++ pretempfile xs

{----------------------------------------------------------}
{----------------------------------------------------------------------}

{-Final Transformations.-}
{-Functions to infer discrete relationships between root words-}
{-and corresponding paradigms, and group inter-related root words-}
{-and paradigms into the same affix rule.-}

--tempchunker -> To chunk input from temporary file
--back into required format for future transformations.
tempchunker :: [String] -> [[String]]
tempchunker [] = []
tempchunker xs = chunksOf 4 xs 

--reconstitute -> To reform output of tempchunker back into
--presufcheck output format.
reconstitute :: [[String]] -> [(String,[Maybe (String,String,Int)])]
reconstitute [] = [] 
reconstitute ([a,b,c,d]:xs) = [(a,[Just (b,c,(read d))])] ++ reconstitute xs 

--groupandsort
groupandsort :: [(String,[Maybe (String,String,Int)])] -> [[(String,[Maybe (String,String,Int)])]]
groupandsort xs = groupBy (\(a,[Just (b,c,d)])  (e,[Just (f,g,h)]) -> c == g) (L.sortOn (\(a,[Just (b,c,d)]) -> c) xs)
 
{-----------------------------------------}
{-----------------------------------------------------------------}
{--------------------------------------------------------------}
{------------------------} 

{-IO Function.-}

--aggregate -> This function will add output of presufcheck to
--temp file in order to aggregate the entire session of data.
aggregate :: [String] -> (FilePath,Handle) -> IO [String]
aggregate [] (tempnamed,temphd) = return []
aggregate (x:xs) (tempnamed,temphd) = do
   --Add current root and corrresponding
   --affix information to the temporary file.
   hPutStrLn temphd x
   --Peform the above steps on next list of root
   --and corresponding affix information. 
   aggregate xs (tempnamed,temphd)

{--------------}



{-Main Function.-}

main :: IO ()
main = do
    {-Unit testing.-}
    print ("Starting Unit Testing:")

    --example1
    let root1 = "teach"
    let rootforms1 = ["teaches","teaching","taught"]
    let answer1 = pretempfile $ (presufcheck root1 rootforms1 1 1)
    ----------
 
    --example2
    let root2 = "jump"
    let rootforms2 = ["jumps","jumping","jumper","jumped"]
    let answer2 = pretempfile $ (presufcheck root2 rootforms2 1 1)
    ----------   

    --example3
    let root3 = "build"
    let rootforms3 = ["builds","building","built","buildable","unbuildable"] 
    let answer3 = pretempfile $ (presufcheck root3 rootforms3 1 1) 
    -----------  

    --example4
    let root4 = "fly"
    let rootforms4 = ["flies","flying","flew","flown"]
    let answer4 = pretempfile $ (presufcheck root4 rootforms4 1 1)

    --example5
    let root5 = "fight"
    let rootforms5 = ["fights","fighting","fought"]
    let answer5 = pretempfile $ (presufcheck root5 rootforms5 1 1)
    ----------

    print (presufcheck root1 rootforms1 1 1)
    print (presufcheck root2 rootforms2 1 1)
    print (presufcheck root3 rootforms3 1 1) 
    print (presufcheck root4 rootforms4 1 1)
    print (presufcheck root5 rootforms5 1 1)

    print "Unit Testing completed." 
    {-------------}
    
    --Create temporary file.
    (tempfile , temph) <- openTempFile "." "temp.txt"      

    --Work on example1.
    affixinfo1 <- aggregate answer1 (tempfile , temph)
    
    --Work on example2.
    affixinfo2 <- aggregate answer2 (tempfile , temph)
    
    --Work on example3.
    affixinfo3 <- aggregate answer3 (tempfile , temph)
   
    --Work on example4.
    affixinfo4 <- aggregate answer4 (tempfile , temph) 

    --Work on example5.
    affixinfo5 <- aggregate answer5 (tempfile , temph)

    --Seek to the beginning of the temporary file.
    hSeek temph AbsoluteSeek 0

    --Read temporary file into [[String]] 
    fulltempread <- hGetContents temph

    let aggregation = (linefeed fulltempread)
    
    let postaggregation = groupandsort $ (reconstitute $ (tempchunker aggregation))
    
    print postaggregation 

    --Close temporary file.
    hClose temph

{----------------}
