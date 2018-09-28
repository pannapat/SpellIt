{-=Spellit=-}
{-=webtoaff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Pre-Alpha=-}
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

--ownlength
ownLength :: [a] -> Int
ownLength = ownLength' 0
    where ownLength' a [] = a
          ownLength' a (_:xs) = ownLength' (a+1) xs

{----------------------------}

{-Functions operating to coerce input into Custom Data Type.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: String -> [String] -> [(String,[Maybe (String,String,Int)])]
presufcheck []    []     = []
presufcheck (_:_) []     = []
presufcheck x     (y:ys) = 
    if null (x L.\\ y) 
        then [(x , [(maybetriplet $ (E.stripInfix x y , 0))])] ++ (presufcheck x ys)
        else if L.length x == L.length y 
            then [(x , [(maybetriplet $ ((singleunnest $ (zipcheck zipped)) , L.length (zipcheck zipped)))])] ++ (presufcheck x ys)
            else [(x , (map (Just) (L.zip3 [[]] [(y L.\\ x)] [(L.length (zipcheck zipped))])))] ++ (presufcheck x ys) 
                where 
                    maybetriplet (Just (a,b),c) = Just (a,b,c)
                    zipcheck [] = []
                    zipcheck (x:xs) = 
                        if (fst x) /= (snd x)
                            then [(Just ([],([(snd x)])))] ++ (zipcheck xs)
                            else zipcheck xs 
                    zipped = L.zip x y 

--simplifypresufcheck -> This function will aggregate the results of presufcheck
--into a single tuple.
simplifypresufcheck :: [(String,[Maybe (String,String,Int)])] -> (String,[Maybe (String,String,Int)])
simplifypresufcheck [] = ([],[])
simplifypresufcheck (x:xs) = (fst x , ((snd x) ++ (snd (simplifypresufcheck xs))))

--pfxsfx -> This function will run through the result
--of simplifypresufcheck, and add PFX or SFX to each tuple.
pfxsfx :: (String,[Maybe (String,String,Int)]) -> [(String,[(Maybe String,Maybe (String,String,Int))])]
pfxsfx ([],[]) = [] 
pfxsfx ((_:_), []) = []
pfxsfx (x,(y:ys)) = 
    if (null (M.fromJust (fmap (tripletsnd) y))) && (not (null (M.fromJust (fmap (tripletfst) y))))
        then [(x,[(Just "PFX",y)])] ++ (pfxsfx (x,ys))
        else if (null (M.fromJust (fmap (tripletfst) y))) && (not (null (M.fromJust (fmap (tripletsnd) y))))
            then [(x,[(Just "SFX",y)])] ++ (pfxsfx (x,ys))
            else if (not (null (M.fromJust (fmap (tripletfst) y)))) && (not (null (M.fromJust (fmap (tripletsnd) y))))
                then [(x,[(Just "PFX/SFX",y)])] ++ (pfxsfx (x,ys))
                else [(x,[(Nothing,y)])] ++ (pfxsfx (x,ys)) 

--simplifypfxsfx -> This function will aggregate the results of pfxsfx
--into a single tuple.
simplifypfxsfx :: [(String,[(Maybe String,Maybe (String,String,Int))])] -> (String,[(Maybe String,Maybe (String,String,Int))])
simplifypfxsfx [] = ([],[])
simplifypfxsfx (x:xs) = (fst x , ((snd x) ++ (snd (simplifypfxsfx xs))))

{------------------------------------------}

{-Functions to pull result of simplifypfxsfx into list based on datatypes above.-}

--torule -> This function will pull data from simplifypfxsfx
--into a list based on RuleHeader and Rule datatypes defined above.
torule :: (String,[(Maybe String,Maybe (String,String,Int))]) -> [(RuleHeader,Rule)]
torule ([],[])     = []
torule ((_:_), []) = []
torule (x,(y:ys))  = 
    if fst y == Just "PFX"
        then [((PfxHead "PFX" 'A' 'Y' (L.length (y:ys))) , (Pfx "PFX" 'A' (mtripletthrd $ (snd y)) (mtripletfst $ (snd y)) '.'))] ++ (torule (x,ys))   
        else if fst y == Just "SFX"
            then [((SfxHead "SFX" 'B' 'Y' (L.length (y:ys))) , (Sfx "SFX" 'B' (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y)) '.'))] ++ (torule (x,ys))
            else if fst y == Just "PFX/SFX"
                then [((PfxHead "PFX" 'A' 'Y' (L.length (y:ys))) , (Pfx "PFX" 'A' (mtripletthrd $ (snd y)) (mtripletfst $ (snd y)) '.'))] ++ [((SfxHead "SFX" 'B' 'Y' (L.length (y:ys))) , (Sfx "SFX" 'B' (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y)) '.'))] ++ (torule (x,ys))
                else torule (x,ys)

--groupsorttorule -> This function will sort and then group
--tuples based on PfxHead or SfxHead.
groupsorttorule :: [(RuleHeader,Rule)] -> [[(RuleHeader,Rule)]]
groupsorttorule [] = []
groupsorttorule xs = L.groupBy (predicate) (sort xs)
    where predicate = (\a b -> check a == check b)
          check ((PfxHead  prefix _ _ _) , _) = prefix
          check _ = []

--simplifytorule -> This function will aggregate the results of
--groupsorttorule into a single tuple.
simplifygroupsorttorule :: [[(RuleHeader,Rule)]] -> [(RuleHeader,[Rule])]
simplifygroupsorttorule [] = []
simplifygroupsorttorule (x:xs) = [((fst (head x)) , [(snd (head x))] ++ (map (snd) (tail x)))] ++ (simplifygroupsorttorule xs)              

{------------------------------------------------------------------------------}


{-Functions related to Database.-}




{----------------------------------------}

{-Main Function.-}

main :: IO ()
main = do
    --Unit testing.
    print ("Starting Unit Testing:")


    let root1 = "jump"
    let rootforms1 = ["jumps","jumping","jumper","jumped"]
    let answer1 = simplifygroupsorttorule $ (groupsorttorule $ (torule $ (simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root1 rootforms1))))))

    let root2 = "build"
    let rootforms2 = ["builds","building","built","buildable","unbuildable"] 
    let answer2 = simplifygroupsorttorule $ (groupsorttorule $ (torule $ (simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root2 rootforms2))))))
  
    let root4 = "fly"
    let rootforms4 = ["flies","flying","flew","flown"]
    let answer4 = simplifygroupsorttorule $ (groupsorttorule $ (torule $ (simplifypfxsfx $ (pfxsfx $ (simplifypresufcheck $ (presufcheck root4 rootforms4))))))

    print answer1
    print answer2 
    print answer4    


    print "Unit Testing completed." 
    ---------------
{----------------}
