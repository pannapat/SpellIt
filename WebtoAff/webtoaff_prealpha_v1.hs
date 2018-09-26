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

{----------------------------}

{-Functions operating to coerce input into Custom Data Type.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: String -> [String] -> [(String , [Maybe (String,String,Int)])]
presufcheck []    []     = []
presufcheck (_:_) []     = []
presufcheck x     (y:ys) = 
    if null (x L.\\ y) 
        then (singlenest $ (x , (singlenest $ (maybetriplet $ (E.stripInfix x y , 0))))) ++ (presufcheck x ys)
        else if L.length x == L.length y 
            then (singlenest $ (x , singlenest $ (maybetriplet $ ((singleunnest $ (zipcheck zipped)) , L.length (zipcheck zipped))))) ++ (presufcheck x ys)
            else (singlenest $ (x , (map (Just) (L.zip3 (singlenest $ []) (singlenest $ (y L.\\ x)) (singlenest $ (L.length (zipcheck zipped))))))) ++ (presufcheck x ys) 
                where 
                    maybetriplet (Just (a,b),c) = Just (a,b,c)
                    zipcheck [] = []
                    zipcheck (x:xs) = 
                        if (fst x) /= (snd x)
                            then (singlenest $ (Just ([],(singlenest $ (snd x))))) ++ (zipcheck xs)
                            else zipcheck xs 
                    zipped = L.zip x y 

--simplifypresufcheck -> This function will aggregate the results of presufcheck
--into a single tuple.
simplifypresufcheck :: [(String , [Maybe (String,String,Int)])] -> (String , [Maybe (String,String,Int)])
simplifypresufcheck [] = ([],[])
simplifypresufcheck (x:xs) = (fst x , ((snd x) ++ (snd (simplifypresufcheck xs))))

--pfxsfx -> This function will run through the result
--of simplifypresufcheck, and add PFX or SFX to each tuple.
pfxsfx :: (String , [Maybe (String,String,Int)]) -> [(String , [(Maybe String , Maybe (String,String,Int))])]
pfxsfx ([],[]) = [] 
pfxsfx ((_:_), []) = []
pfxsfx (x,(y:ys)) = 
    if (null (M.fromJust (fmap (tripletsnd) y))) && (not (null (M.fromJust (fmap (tripletfst) y))))
        then (singlenest $ (x,(singlenest $ (Just "PFX",y)))) ++ (pfxsfx (x,ys))
        else if (null (M.fromJust (fmap (tripletfst) y))) && (not (null (M.fromJust (fmap (tripletsnd) y))))
            then (singlenest $ (x,(singlenest $ (Just "SFX",y)))) ++ (pfxsfx (x,ys))
            else if (not (null (M.fromJust (fmap (tripletfst) y)))) && (not (null (M.fromJust (fmap (tripletsnd) y))))
                then (singlenest $ (x,(singlenest $ (Just "PFX/SFX",y)))) ++ (pfxsfx (x,ys))
                else (singlenest $ (x,(singlenest $ (Nothing,y)))) ++ (pfxsfx (x,ys)) 

--simplifypfxsfx -> This function will aggregate the results of pfxsfx
--into a single tuple.
simplifypfxsfx :: [(String , [(Maybe String , Maybe (String,String,Int))])] -> (String , [(Maybe String , Maybe (String,String,Int))])
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
    let answer1 = simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root1 rootforms1)))

    let root2 = "build"
    let rootforms2 = ["builds","building","built","buildable","unbuildable"] 
    let answer2 = simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root2 rootforms2)))
   
    let root4 = "fly"
    let rootforms4 = ["flies","flying","flew","flown"]
    let answer4 = simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root4 rootforms4)))

    print answer1
    print answer2 
    print answer4    


    print "Unit Testing completed." 
    ---------------
{----------------}
