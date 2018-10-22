{-=Spellit=-}
{-=WebtoAff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Pre-Alpha=-}
{-=Synopsis:  This Haskell Script will query a database,=-}
{-=tuple by tuple, and eventually create a .aff for each unique=-}
{-=session (each time "submit" is clicked on webpage).=-}

{-Syntax Extensions.-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

{--------------------}

{-Module-}

module WebtoAff where

{--------}

{-File Import-}

import WebtoAff_JSON

{-------------}

{-Imports-}

import Data.Aeson as A
import Data.Aeson.Types as AT
import Data.Aeson.Internal as ATI
import Data.ByteString.Lazy.Char8 as LBSC
import Data.HashMap.Lazy as HL
import Data.List as L
import Data.List.Split as S
import qualified Data.List.Extra as E
import Data.Maybe as M
import Data.Text as T
import System.Directory as D
import System.Environment as En
import System.IO as IO
import System.IO.Temp as Temp
import Text.PrettyPrint.Boxes

{---------}

{-Custom Data Declarations (after JSON).-}
 
data RuleHeader =
    PfxHead String Char Char Int
  | SfxHead String Char Char Int
    deriving (Eq, Ord, Show)

data Rule = 
    Pfx String Char Int String Char 
  | Sfx String Char Int String Char 
    deriving (Eq, Ord, Show)

{----------------------------------------}

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

--tupletotriplet
tupletotriplet :: String -> (String,String) -> (String,String,String)
tupletotriplet x (y,z) = (x,y,z)

--maptupletotriplet
maptupletotriplet :: String -> [(String,String)] -> [(String,String,String)]
maptupletotriplet [] []        = []
maptupletotriplet _  []        = []
maptupletotriplet x ((y,z):xs) = [tupletotriplet x (y,z)] ++ (maptupletotriplet x xs)

{----------------------------}

{-Functions to grab information from parsed JSON.-}

--grabparadigm -> To grab the paradigm of the json.
grabparadigm :: Maybe Paradigm -> String
grabparadigm  Nothing                                  = []
grabparadigm (Just (Paradigm str1 liststr listinputs)) = str1

--grabslots -> To grab the slots of the json.
grabslots :: Maybe Paradigm -> [String]
grabslots  Nothing                                  = []
grabslots (Just (Paradigm str1 liststr listinputs)) = liststr

--grabwords -> To grab the words of the json.
grabwords :: Maybe Paradigm -> Maybe A.Value
grabwords (Just (Paradigm str1 liststr listvalues)) = Just listvalues

--parseinput -> To grab information out of object.
parseinput :: A.Value -> Parser [Inputs]
parseinput xs = (L.map (\word -> Inputs (HL.toList word)) <$> parseJSON xs)

{-------------------------------------------------}

{-Functions dealing with Reading parsed JSON into original datatypes.-}

--readslots -> To read slots back into proper datatype.
readslots :: String -> [String]
readslots xs = read xs

--readwords -> To read words back into proper datatype.
readwords :: String -> Maybe [Inputs]
readwords xs = read xs

--slotandwordgrabber -> To grab all slotandword fields
--from the parsed Maybe [Inputs].
slotandwordgrab :: Maybe [Inputs] -> [[(String,String)]]
slotandwordgrab (Just []) = []
slotandwordgrab (Just ((Inputs {slotandword = sw}):xs)) = [sw] ++ (slotandwordgrab (Just xs))

--rootgrabber -> Used in prefinalreader.
rootgrabber :: [[(String,String)]] -> [[(String,String,String)]]
rootgrabber [] = []
rootgrabber (x:xs) = [(maptupletotriplet (L.concat (L.map (snd) (L.filter ((=="root").fst) x))) x)] ++ (rootgrabber xs)

--rootremover -> Used in truefinalreader.
rootremover :: [[(String,String,String)]] -> [[(String,String,String)]]
rootremover [] = []
rootremover (x:xs) = [L.filter (not . ((=="root").tripletsnd)) x] ++ (rootremover xs)

--readchunker -> To chunk the input appropriately.
readchunker :: [String] -> [[String]]
readchunker [] = []
readchunker xs = S.chunksOf 3 xs

--beginreader -> To transform elements of readchunker back into correct types.
beginreader :: [[String]] -> [(String,[String],Maybe [Inputs])]
beginreader [] = [] 
beginreader ([x,y,z]:xs) = [(x,(readslots y),(readwords z))] ++ (beginreader xs)

--midreader -> To transform elements of beginreader
--into intermediate format.
midreader :: [(String,[String],Maybe [Inputs])] -> [(String,[String],[[(String,String)]])]
midreader [] = []
midreader ((x,y,Just z):xs) = [(x,y,slotandwordgrab (Just z))] ++ (midreader xs) 

--midfinalreader -> To tranform elements of midreader
--into a further intermeditate format.
midfinalreader :: [(String,[String],[[(String,String)]])] -> [(String,[[(String,String)]])]
midfinalreader [] = [] 
midfinalreader ((x,y,z):xs) = [(x,z)] ++ (midfinalreader xs)

--prefinalreader ->  To transform the elements of midfinalreader
--into a further intermediate format.
prefinalreader :: [(String,[[(String,String)]])] -> [(String,[[(String,String,String)]])]
prefinalreader [] = []
prefinalreader ((x,y):xs) = [(x,rootgrabber y)] ++ (prefinalreader xs)

--truefinalreader -> To tranform the elements of prefinalreader
--into the necessary format of presufcheck.
truefinalreader :: [(String,[[(String,String,String)]])] -> [(String,[[(String,String,String)]])]
truefinalreader [] = []
truefinalreader ((x,y):xs) = [(x,rootremover y)] ++ (truefinalreader xs)

{---------------------------------------------------------------------}

{-Functions operating to take each root and corresponding root paradigm-}
{-and prepare to feed into temporary file for aggregation.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: String -> [String] -> Int -> Int -> [(String,[Maybe (String,String,Int)])]
presufcheck []    []     _          _          = []
presufcheck (_:_) []     _          _          = []
presufcheck x     (y:ys) statestart totalstart = 
    if L.null (x L.\\ y) 
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
                     zipwithpadding a _ []     ys     = L.zip (L.repeat a) ys
                     zipwithpadding _ b xs     []     = L.zip xs (L.repeat b)
                     --zipwithpaddingdiff
                     zipwithpaddingdiff :: [(String,String)] -> Maybe (String,String) 
                     zipwithpaddingdiff [] = Nothing
                     zipwithpaddingdiff (x:xs) =
                         if (fst x) /= (snd x)
                             then Just ("",((snd x) ++ (L.concat (L.map (snd) xs))))
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
tempchunker xs = S.chunksOf 4 xs 

--reconstitute -> To reform output of tempchunker back into
--presufcheck output format.
reconstitute :: [[String]] -> [(String,[Maybe (String,String,Int)])]
reconstitute [] = [] 
reconstitute ([a,b,c,d]:xs) = [(a,[Just (b,c,(read d))])] ++ reconstitute xs 

--groupandsort
groupandsort :: [(String,[Maybe (String,String,Int)])] -> [[(String,[Maybe (String,String,Int)])]]
groupandsort xs = L.groupBy (\(a,[Just (b,c,d)])  (e,[Just (f,g,h)]) -> c == g) (L.sortOn (\(a,[Just (b,c,d)]) -> c) xs)
 
{-----------------------------------------}
{-----------------------------------------------------------------}
{--------------------------------------------------------------}
{------------------------} 

{-IO Functions.-}

--jsonparse -> This function will parse the JSON input files.
jsonparse :: [String] -> (FilePath,Handle) -> IO [String]
jsonparse [] (tempnamed,temphd) = return []
jsonparse (x:xs) (tempnamed,temphd) = do
    --Decode the current json file.
    jsondecoded <- (A.decode <$> (LBSC.readFile x)) :: IO (Maybe WebtoAff_JSON.Paradigm)
    --Grab each part of decoded JSON.
    let paradigm = grabparadigm jsondecoded
    let slots    = grabslots jsondecoded
    let words    = parseMaybe parseinput =<< (grabwords jsondecoded)
    --Add these to a temp file. 
    IO.hPutStrLn temphd paradigm   
    IO.hPutStrLn temphd (show slots)
    IO.hPutStrLn temphd (show words)
    --Recurse through rest of list.
    jsonparse xs (tempnamed,temphd)

--aggregate -> This function will add output of presufcheck to
--temp file in order to aggregate the entire session of data.
aggregate :: [String] -> (FilePath,Handle) -> IO [String]
aggregate [] (tempnamed,temphd) = return []
aggregate (x:xs) (tempnamed,temphd) = do
   --Add current root and corrresponding
   --affix information to the temporary file.
   IO.hPutStrLn temphd x
   --Peform the above steps on next list of root
   --and corresponding affix information. 
   aggregate xs (tempnamed,temphd)

{--------------}



{-Main Function.-}

main :: IO ()
main = do
    --Get Command line arguments.
    cmdargs <- En.getArgs
    case cmdargs of
        [] -> error "No files supplied."
        allargs -> do --Create a temporary file.
                      (tempfile,temph) <- Temp.openTempFile "." "parsed.txt" 
                       
                      --Run jsonparse on all inputs.
                      jsonparse allargs (tempfile,temph)
                      
                      --Seek to the beginning of the temporary file.
                      hSeek temph AbsoluteSeek 0

                      --Read temporary file into [[String]] 
                      fulltempread <- IO.hGetContents temph
                      let aggregation = (linefeed fulltempread)
                      print aggregation
                      let chunkaggregation = readchunker aggregation
                      print chunkaggregation
                      let readerchunkaggregation = beginreader chunkaggregation
                      print readerchunkaggregation
                      let midreaderchunkaggregation = midreader readerchunkaggregation
                      print midreaderchunkaggregation
                      let midfinalreaderchunkaggregation = midfinalreader midreaderchunkaggregation
                      print midfinalreaderchunkaggregation
                      let prefinalreaderchunkaggregation = prefinalreader midfinalreaderchunkaggregation
                      print prefinalreaderchunkaggregation
                      let truefinalreaderchunkaggregation = truefinalreader prefinalreaderchunkaggregation
                      print truefinalreaderchunkaggregation
                      --let postaggregationparadigm = L.head aggregation
                      --let postaggregationwords = readwords (L.last aggregation)
                      --let postaggregationslots = readslots (aggregation L.!! 1)
                      --print postaggregationparadigm
                      --print postaggregationwords
                      --print postaggregationslots


{----------------}
