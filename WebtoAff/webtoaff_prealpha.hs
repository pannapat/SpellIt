{-=Spellit=-}
{-=webtoaff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Alpha=-}
{-=Synopsis:  This Haskell Script will query a database,=-}
{-=tuple by tuple, and eventually create a .aff for each unique=-}
{-=session (each time "submit" is clicked on webpage).=-}

{-Imports-}

import Data.List as L
import qualified Data.List.Extra as E
import Data.Maybe as M
import Database.CouchDB
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment
import Text.PrettyPrint.Boxes

{---------}

{-Custom Data Type.-}

data Rule = 
    Pfx Int Char Int String String 
  | Sfx Int Char Int String String deriving (Show)

{-------------------}

{-General Utility Functions.-}

--singlenest 
singlenest :: a -> [a]
singlenest xs = [xs] 
 
{----------------------------}

{-Functions operating to coerce input into Custom Data Type.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: Eq a => [a] -> [[a]] -> [([a] , [Maybe ([a],[a])])]
presufcheck []    []     = []
presufcheck (_:_) []     = []
presufcheck x     (y:ys) = 
    if null (x L.\\ y) 
        then (singlenest $ (x,(singlenest $ (E.stripInfix x y)))) ++ (presufcheck x ys)
        else if L.length x == L.length y 
            then (singlenest $ (x,(zipcheck zipped))) ++ (presufcheck x ys)
            else (singlenest $ (x,(map (Just) (zip (singlenest $ []) (singlenest $ (y L.\\ x)))))) ++ (presufcheck x ys) 
                where 
                    zipcheck [] = []
                    zipcheck (x:xs) = 
                        if (fst x) /= (snd x)
                            then (singlenest $ (Just ([],(singlenest $ (snd x))))) ++ (zipcheck xs)
                            else zipcheck xs 
                    zipped = L.zip x y 

--rootpresuf -> This function will aggregate the results of presufcheck
--into a single tuple.
rootpresuf :: [([a] , [Maybe ([a],[a])])] -> ([a] , [Maybe ([a],[a])])
rootpresuf [] = ([],[])
rootpresuf (x:xs) = (fst x , ((snd x) ++ (snd (rootpresuf xs))))

--sfxpfx -> This function will run through the result
--of rootpresuf, and add PFX or SFX to each tuple.
sfxpfx :: ([a] , [Maybe ([a],[a])]) -> [([a] , [(Maybe [Char] , Maybe ([a],[a]))])]
sfxpfx ([],[]) = [] 
sfxpfx ((_:_), []) = []
sfxpfx (x,(y:ys)) = 
    if (null (M.fromJust (fmap (snd) y))) && (not (null (M.fromJust (fmap (fst) y))))
        then (singlenest $ (x,(singlenest $ (Just "PFX",y)))) ++ (sfxpfx (x,ys))
        else if (null (M.fromJust (fmap (fst) y))) && (not (null (M.fromJust (fmap (snd) y))))
            then (singlenest $ (x,(singlenest $ (Just "SFX",y)))) ++ (sfxpfx (x,ys))
            else if (not (null (M.fromJust (fmap (fst) y)))) && (not (null (M.fromJust (fmap (snd) y))))
                then (singlenest $ (x,(singlenest $ (Just "PFX/SFX",y)))) ++ (sfxpfx (x,ys))
                else (singlenest $ (x,(singlenest $ (Nothing,y)))) ++ (sfxpfx (x,ys)) 

--simplifypfxsfx -> This function will aggregate the results of sfxpfx
--into a single tuple.
simplifypfxsfx :: [([a] , [(Maybe [Char] , Maybe ([a],[a]))])] -> ([a] , [(Maybe [Char] , Maybe ([a],[a]))])
simplifypfxsfx [] = ([],[])
simplifypfxsfx (x:xs) = (fst x , ((snd x) ++ (snd (simplifypfxsfx xs))))

{------------------------------------------}

{-Functions related to Database.-}




{----------------------------------------}

{-Main Function.-}

main :: IO ()
main = do
    --Unit testing.
    print ("Starting Unit Testing:")

    let root1 = "jump"
    let rootforms1 = ["jumps","jumping","jumper","jumped"]
    let answer1 = simplifypfxsfx $ (sfxpfx $ (rootpresuf $ (presufcheck root1 rootforms1)))

    let root2 = "build"
    let rootforms2 = ["builds","building","built","buildable","unbuildable"] 
    let answer2 = simplifypfxsfx $ (sfxpfx $ (rootpresuf $ (presufcheck root2 rootforms2))) 
   
    let root4 = "fly"
    let rootforms4 = ["flies","flying","flew","flown"]
    let answer4 = simplifypfxsfx $ (sfxpfx $ (rootpresuf $ (presufcheck root4 rootforms4)))

    print answer1
    print answer2 
    print answer4    

    print "Unit Testing completed." 

{----------------}
