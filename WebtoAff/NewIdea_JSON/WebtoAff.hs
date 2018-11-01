{-=Spellit=-}
{-=WebtoAff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Pre-Alpha=-}
{-=Synopsis:  This Haskell Script will create a affix file=-}
{-=from the JSON input files.=-}

{-Syntax Extensions.-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE QuasiQuotes       #-}

{--------------------}

{-Imports-}

import Control.Monad (mzero)
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
import Text.PrettyPrint.Boxes as PB

{---------}

{-Custom Data Declarations (JSON).-}

--Paradigm
data Paradigm = Paradigm {
    paradigm :: String
  , slots :: [String]
  , words :: A.Value
  } deriving (Read,Show)

instance FromJSON Paradigm
    where
        parseJSON (Object v) = Paradigm <$> (v .: "paradigm_name")
                                        <*> (v .: "slots")
                                        <*> (v .: "words")

        parseJSON _          = mzero
--

--Inputs
data Inputs = Inputs {
    slotandword :: [(String,String)]
    } deriving (Read,Show)
--
--

{----------------------------------}

{-Custom Data Declarations (after JSON).-}

data AffixFileEncoding = 
    AffixFileEncoding String
    deriving (Eq, Ord, Show)

data PreRuleHeader =
    PrePfxHead String String Char
  | PreSfxHead String String Char
    deriving (Eq, Ord, Show) 

data PreRule =
    PrePfx String String Int String
  | PreSfx String String Int String
    deriving (Eq, Ord, Show)

data RuleHeader =
    PfxHead String String Char Int
  | SfxHead String String Char Int
    deriving (Eq, Ord, Show)

data Rule = 
    Pfx String String Int String String 
  | Sfx String String Int String String
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
parseinput :: A.Value -> AT.Parser [Inputs]
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
slotandwordgrab (Just [])                               = []
slotandwordgrab (Just ((Inputs {slotandword = sw}):xs)) = [sw] ++ (slotandwordgrab (Just xs))

--rootgrabber -> Used in prefinalreader.
rootgrabber :: [[(String,String)]] -> [[(String,String,String)]]
rootgrabber []     = []
rootgrabber (x:xs) = [(maptupletotriplet (L.concat (L.map (snd) (L.filter ((=="root").fst) x))) x)] ++ (rootgrabber xs)

--rootremover -> Used in truefinalreader.
rootremover :: [[(String,String,String)]] -> [[(String,String,String)]]
rootremover []     = []
rootremover (x:xs) = [L.filter (not . ((=="root").tripletsnd)) x] ++ (rootremover xs)

--readchunker -> To chunk the input appropriately.
readchunker :: [String] -> [[String]]
readchunker [] = []
readchunker xs = S.chunksOf 3 xs

--beginreader -> To transform elements of readchunker back into correct types.
beginreader :: [[String]] -> [(String,[String],Maybe [Inputs])]
beginreader []           = [] 
beginreader ([x,y,z]:xs) = [(x,(readslots y),(readwords z))] ++ (beginreader xs)

--midreader -> To transform elements of beginreader
--into intermediate format.
midreader :: [(String,[String],Maybe [Inputs])] -> [(String,[String],[[(String,String)]])]
midreader []                = []
midreader ((x,y,Just z):xs) = [(x,y,slotandwordgrab (Just z))] ++ (midreader xs) 

--midfinalreader -> To tranform elements of midreader
--into a further intermeditate format.
midfinalreader :: [(String,[String],[[(String,String)]])] -> [(String,[[(String,String)]])]
midfinalreader []           = [] 
midfinalreader ((x,y,z):xs) = [(x,z)] ++ (midfinalreader xs)

--prefinalreader ->  To transform the elements of midfinalreader
--into a further intermediate format.
prefinalreader :: [(String,[[(String,String)]])] -> [(String,[[(String,String,String)]])]
prefinalreader []         = []
prefinalreader ((x,y):xs) = [(x,rootgrabber y)] ++ (prefinalreader xs)

--truefinalreader -> To tranform the elements of prefinalreader
--into the necessary format of presufcheck.
truefinalreader :: [(String,[[(String,String,String)]])] -> [(String,[[(String,String,String)]])]
truefinalreader []         = []
truefinalreader ((x,y):xs) = [(x,rootremover y)] ++ (truefinalreader xs)

{---------------------------------------------------------------------}

{-Function operating to take each root and corresponding root paradigm-}
{-and determine differences between them.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: [(String,[[(String,String,String)]])] -> [(String,[[(String,Maybe (String,String,Int))]])]
presufcheck []          = []
presufcheck ((a,xs):ys) = [(a,nesteddiff xs)] ++ (presufcheck ys)
    where
    --Nested Function Definitions.--
    --maybetriplet
    maybetriplet :: (Maybe (String,String),Int) -> Maybe (String,String,Int)
    maybetriplet (Just (a,b),c) = Just (a,b,c) 
    --nesteddiff
    nesteddiff :: [[(String,String,String)]] -> [[(String,Maybe (String,String,Int))]]
    nesteddiff []     = []
    nesteddiff (x:xs) = [diff x] ++ (nesteddiff xs)
    --diff
    diff :: [(String,String,String)] -> [(String,Maybe (String,String,Int))]
    diff []           = [] 
    diff ((x,y,z):xs) = if L.null (x L.\\ z)
                            then [((x,maybetriplet (E.stripInfix x z , 0)))] ++ (diff xs)
                            else diff xs
    --------------------------------  

{----------------------------------------------------------}
{----------------------------------------------------------------------}

{-Functions to pull result of presufcheck into lists-}
{-based on datatypes above.-}

--simplifypresufcheck -> This function will change
--the format of the results of presufcheck.
simplifypresufcheck :: [(String,[[(String,Maybe (String,String,Int))]])] -> [(String,[(String,[Maybe (String,String,Int)])])]
simplifypresufcheck []          = [] 
simplifypresufcheck ((a,xs):ys) = [(a,nestedchanger xs)] ++ (simplifypresufcheck ys)
    where 
        --Nested Function Definitions.--
        --nestedchanger
        nestedchanger :: [[(String,Maybe (String,String,Int))]] -> [(String,[Maybe (String,String,Int)])]
        nestedchanger []     = [] 
        nestedchanger (x:xs) = [changer x] ++ (nestedchanger xs)
        --changer
        changer :: [(String,Maybe (String,String,Int))] -> (String,[Maybe (String,String,Int)])
        changer []     = ([],[])
        changer (x:xs) = (fst x,([snd x] ++ (snd (changer xs))))
        --------------------------------

--pfxsfx -> This function will run through the result
--of presufcheck and add PFX or SFX to each tuple.
pfxsfx :: [(String,[(String,[Maybe (String,String,Int)])])] -> [(String,[(String,[(Maybe String,Maybe (String,String,Int))])])] 
pfxsfx []     = []
pfxsfx ((a,xs):ys) = [(a,nestedadder xs)] ++ (pfxsfx ys)
    where
        --Nested Function Definitions.--
        --nestedadder
        nestedadder :: [(String,[Maybe (String,String,Int)])] -> [(String,[(Maybe String,Maybe (String,String,Int))])]
        nestedadder []     = []
        nestedadder (x:xs) = [adder x] ++ (nestedadder xs)
        --adder 
        adder :: (String,[Maybe (String,String,Int)]) -> (String,[(Maybe String,Maybe (String,String,Int))])
        adder ([],[])     = ([],[])
        adder ((_:_), []) = ([],[])
        adder (x,(y:ys))  = 
            if (L.null (M.fromJust (fmap (tripletsnd) y))) && (not (L.null (M.fromJust (fmap (tripletfst) y))))
                then (x,[(Just "PFX",y)] ++ (snd (adder (x,ys)))) 
                else if (L.null (M.fromJust (fmap (tripletfst) y))) && (not (L.null (M.fromJust (fmap (tripletsnd) y))))
                    then (x,[(Just "SFX",y)] ++ (snd (adder (x,ys)))) 
                    else if (not (L.null (M.fromJust (fmap (tripletfst) y)))) && (not (L.null (M.fromJust (fmap (tripletsnd) y))))
                        then (x,[(Just "PFX/SFX",y)] ++ (snd (adder (x,ys))))
                        else (x,[(Nothing,y)] ++ (snd (adder (x,ys))))
        --------------------------------

--toprerule -> This function will pull data from presufcheck
--into a list based on PreRuleHeader and PreRule.
toprerule :: [(String,[(String,[(Maybe String,Maybe (String,String,Int))])])] -> [(String,[(String,[(PreRuleHeader,PreRule)])])]
toprerule []          = []
toprerule ((a,xs):ys) = [(a,prenestedruler a xs)] ++ (toprerule ys)
    where
        --Nested Function Definitions.--
        --prenestedruler
        prenestedruler :: String -> [(String,[(Maybe String,Maybe (String,String,Int))])] -> [(String,[(PreRuleHeader,PreRule)])]
        prenestedruler [] []    = []
        prenestedruler (_:_) [] = []
        prenestedruler a (x:xs) = [preruler a x] ++ (prenestedruler a xs) 
        --preruler
        preruler :: String -> (String,[(Maybe String,Maybe (String,String,Int))]) -> (String,[(PreRuleHeader,PreRule)])
        preruler [] ([],[])     = ([],[])
        preruler [] ((_:_), []) = ([],[])
        preruler (_:_) (_, [])  = ([],[])
        preruler a (x,(y:ys))   = 
            if fst y == Just "PFX" 
                then (x,[((PrePfxHead "PFX" (a ++ "P") 'Y') , (PrePfx "PFX" (a ++ "P") (mtripletthrd $ (snd y)) (mtripletfst $ (snd y))))] ++ (snd (preruler a (x,ys))))
                else if fst y == Just "SFX"
                    then (x,[((PreSfxHead "SFX" (a ++ "S") 'Y') , (PreSfx "SFX" (a ++ "S") (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y))))] ++ (snd (preruler a (x,ys)))) 
                    else if fst y == Just "PFX/SFX"
                        then (x,[((PrePfxHead "PFX" (a ++ "P") 'Y') , (PrePfx "PFX" (a ++ "P") (mtripletthrd $ (snd y)) (mtripletfst $ (snd y))))] ++ [((PreSfxHead "SFX" (a ++ "S") 'Y') , (PreSfx "SFX" (a ++ "S") (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y))))] ++ (snd (preruler a (x,ys)))) 
                        else preruler a (x,ys)
        --------------------------------

--simplifytoprerule -> This function will simplify 
--the results of toprerule.
simplifytoprerule :: [(String,[(String,[(PreRuleHeader,PreRule)])])] -> [(String,[[(String,PreRuleHeader,PreRule)]])]
simplifytoprerule []          = []
simplifytoprerule ((a,xs):ys) = [(a,aggregaterootprerule xs)] ++ (simplifytoprerule ys)
    where
        --Nested Function Definitions.--
        --aggregaterootprerule
        aggregaterootprerule :: [(String,[(PreRuleHeader,PreRule)])] -> [[(String,PreRuleHeader,PreRule)]]
        aggregaterootprerule []     = []
        aggregaterootprerule (x:xs) = [rootprerule x] ++ (aggregaterootprerule xs)
        --rootprerule
        rootprerule :: (String,[(PreRuleHeader,PreRule)]) -> [(String,PreRuleHeader,PreRule)]
        rootprerule ([],[])        = [] 
        rootprerule ((_:_),[])     = []
        rootprerule ((x,(y,z):ys)) = [(x,y,z)] ++ (rootprerule (x,ys))  
        --------------------------------

--finalsimplifytoprerule -> This function will further
--simplify the results of simplifytoprerule.
finalsimplifytoprerule :: [(String,[[(String,PreRuleHeader,PreRule)]])] -> [(String,[(String,PreRuleHeader,PreRule)])]
finalsimplifytoprerule []     = [] 
finalsimplifytoprerule (x:xs) = [rootprerule x] ++ (finalsimplifytoprerule xs)
    where
        --Nested Function Definitions.--
        --rootprerule
        rootprerule :: (String,[[(String,PreRuleHeader,PreRule)]]) -> (String,[(String,PreRuleHeader,PreRule)])
        rootprerule ([],[])    = ([],[])    
        rootprerule ((_:_),[]) = ([],[])
        rootprerule (x,y) = (x,L.concat y) 
        --------------------------------  

--wordandprerule -> This function will take the results
--of finalsimplifytoprerule and grab just the second part
--of each list.
wordandprerule :: [(String,[(String,PreRuleHeader,PreRule)])] -> [[(String,PreRuleHeader,PreRule)]]
wordandprerule [] = []
wordandprerule (x:xs) = [listgrab x] ++ (wordandprerule xs)
    where
        --Nested Function Definitions.--
        --listgrab
        listgrab :: (String,[(String,PreRuleHeader,PreRule)]) -> [(String,PreRuleHeader,PreRule)]
        listgrab ([],[])    = []
        listgrab ((_:_),[]) = []
        listgrab (x,(y:ys)) = [y] ++ (listgrab (x,ys))
        --------------------------------

--preparetorule -> This function will take the results
--of wordandprerule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
preparetorule :: [[(String,PreRuleHeader,PreRule)]] -> [[[(String,PreRuleHeader,PreRule)]]]
preparetorule []     = []
preparetorule (x:xs) = [initialsortandgroup x] ++ (preparetorule xs) 
    where
        --Nested Function Definitions.--
        --initialsortandgroup
        initialsortandgroup :: [(String,PreRuleHeader,PreRule)] -> [[(String,PreRuleHeader,PreRule)]]
        initialsortandgroup [] = [] 
        initialsortandgroup xs = L.groupBy (\(_,b,_) (_,e,_) -> b == e) 
                                 (L.sortOn (\ys -> case ys of (_,PrePfxHead b _ _,_) -> b 
                                                              (_,PreSfxHead b _ _,_) -> b) xs)
       -------------------------------- 

--firstnexttorule -> This function will take the results
--of preparetorule and prepare it to be loaded 
--into the RuleHeader and Rule datatypes.
firstnexttorule :: [[[(String,PreRuleHeader,PreRule)]]] -> [[[(String,PreRuleHeader,PreRule)]]]
firstnexttorule []     = []
firstnexttorule (x:xs) = [largeinitialsort x] ++ (firstnexttorule xs)
    where 
        --Nested Function Definitions.--
        --largeinitialsort
        largeinitialsort :: [[(String,PreRuleHeader,PreRule)]] -> [[(String,PreRuleHeader,PreRule)]]
        largeinitialsort []     = []
        largeinitialsort (x:xs) = [initialsort x] ++ (largeinitialsort xs)
        --initialsort
        initialsort :: [(String,PreRuleHeader,PreRule)] -> [(String,PreRuleHeader,PreRule)]
        initialsort [] = []
        initialsort xs = L.sortOn (\zs -> case zs of (_,_,PrePfx _ _ c _) -> c
                                                     (_,_,PreSfx _ _ c _) -> c) (L.sortOn (\ys -> case ys of (_,_,PrePfx _ _ _ c) -> c 
                                                                                                             (_,_,PreSfx _ _ _ c) -> c) xs)
        --------------------------------  

--secondnexttorule -> This function will take the results
--of firstnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
secondnexttorule :: [[[(String,PreRuleHeader,PreRule)]]] -> [[[[(String,PreRuleHeader,PreRule)]]]]
secondnexttorule [] = []
secondnexttorule (x:xs) = [largeinitialgroup x] ++ (secondnexttorule xs)
    where
        --Nested Function Definitions.--
        --largeinitialgroup
        largeinitialgroup :: [[(String,PreRuleHeader,PreRule)]] -> [[[(String,PreRuleHeader,PreRule)]]]
        largeinitialgroup []     = []
        largeinitialgroup (x:xs) = [initialgroup x] ++ (largeinitialgroup xs) 
        --initialgroup
        initialgroup :: [(String,PreRuleHeader,PreRule)] -> [[(String,PreRuleHeader,PreRule)]]
        initialgroup [] = []
        initialgroup xs = if (L.any (isPrePfx) xs)  
                              then L.groupBy (\(_,_,PrePfx _ _ c d) (_,_,PrePfx _ _ g h) -> (c == g && d == h)) xs
                              else L.groupBy (\(_,_,PreSfx _ _ c d) (_,_,PreSfx _ _ g h) -> (c == g && d == h)) xs   
        --isPrePfx
        isPrePfx :: (String,PreRuleHeader,PreRule) -> Bool
        isPrePfx (_,_,PrePfx _ _ _ _) = True
        isPrePfx (_,_,PreSfx _ _ _ _) = False             
        --------------------------------

--thirdnexttorule -> This function will take the results
--of secondnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes
thirdnexttorule :: [[[[(String,PreRuleHeader,PreRule)]]]] -> [[[[(String,PreRuleHeader,PreRule,Rule)]]]]
thirdnexttorule []     = [] 
thirdnexttorule (x:xs) = [nestednestednestedinnerrule x] ++ (thirdnexttorule xs)
    where
        --Nested Function Definitions.--
        --nestednestednestedinnerrule
        nestednestednestedinnerrule :: [[[(String,PreRuleHeader,PreRule)]]] -> [[[(String,PreRuleHeader,PreRule,Rule)]]]
        nestednestednestedinnerrule []     = [] 
        nestednestednestedinnerrule (x:xs) = [nestednestedinnerrule x] ++ (nestednestednestedinnerrule xs)
        --nestednestedinnerrule
        nestednestedinnerrule :: [[(String,PreRuleHeader,PreRule)]] -> [[(String,PreRuleHeader,PreRule,Rule)]]
        nestednestedinnerrule []     = []
        nestednestedinnerrule (x:xs) = [innerrule x] ++ (nestednestedinnerrule xs)
        --innerrule
        innerrule :: [(String,PreRuleHeader,PreRule)] -> [(String,PreRuleHeader,PreRule,Rule)]
        innerrule [] = []
        innerrule xs = insertrule (regexmapper xs) xs 
        --insertrule
        insertrule :: String -> [(String,PreRuleHeader,PreRule)] -> [(String,PreRuleHeader,PreRule,Rule)]
        insertrule [] []                       = []
        insertrule (_:_) []                    = []
        insertrule x ((a,b,PrePfx c d e f):xs) = [(a,b,PrePfx c d e f,Pfx c d e f x)] ++ (insertrule x xs)
        insertrule x ((a,b,PreSfx c d e f):xs) = [(a,b,PreSfx c d e f,Sfx c d e f x)] ++ (insertrule x xs)
        --regexmapper
        regexmapper :: [(String,PreRuleHeader,PreRule)] -> String
        regexmapper [] = []
        regexmapper xs = if (L.any (isPrePfx) xs)
                             then "[" ++ (L.nub (L.map (L.head) (L.map (tripletfst) xs))) ++ "]"
                             else "[" ++ (L.nub (L.map (L.last) (L.map (tripletfst) xs))) ++ "]"
        --isPrePfx
        isPrePfx :: (String,PreRuleHeader,PreRule) -> Bool
        isPrePfx (_,_,PrePfx _ _ _ _) = True
        isPrePfx (_,_,PreSfx _ _ _ _) = False 
        --------------------------------  

--thirdrulelengthmapper
thirdrulelengthmapper :: [[[[(String,PreRuleHeader,PreRule,Rule)]]]] -> [Int]
thirdrulelengthmapper [] = []
thirdrulelengthmapper xs = L.concat (L.map (L.map (L.length)) xs)

--fourthnexttorule -> This function will take the results
--of thirdnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
fourthnexttorule :: [[[(String,PreRuleHeader,PreRule,Rule)]]] -> [Int] -> [[[(String,PreRuleHeader,PreRule,RuleHeader,Rule)]]]
fourthnexttorule []     []     = []
fourthnexttorule []  (_:_)     = []  
fourthnexttorule (x:xs) (y:ys) = [L.map (L.map (\zs -> case zs of (a,PrePfxHead b c d,e,f) -> (a,PrePfxHead b c d,e,PfxHead b c d y,f)
                                                                  (a,PreSfxHead b c d,e,f) -> (a,PreSfxHead b c d,e,SfxHead b c d y,f))) x] ++ (fourthnexttorule xs ys)

--torule -> To use the result of fourthnexttorule
--to grab only one RuleHeader and Rule per nested list.
torule :: [[[(String,PreRuleHeader,PreRule,RuleHeader,Rule)]]] -> [(RuleHeader,Rule)]
torule [] = []
torule xs = L.map (L.last) (L.concat (L.map (L.map (L.map (\(_,_,_,d,e) -> (d,e)))) xs)) 

--sorttorule -> To sort the results of torule
--by Rule.
sorttorule :: [(RuleHeader,Rule)] -> [(RuleHeader,Rule)]
sorttorule [] = []
sorttorule xs = L.sortOn (\ys -> case ys of (PfxHead _ a _ _,_) -> a
                                            (SfxHead _ a _ _,_) -> a) xs

--grouptorule -> To group the results of sorttorule.
grouptorule :: [(RuleHeader,Rule)] -> [[(RuleHeader,Rule)]]
grouptorule [] = []
grouptorule xs =  L.groupBy (ruleheaderruleeq) xs
    where
        --Nested Function Definitions.--
        --ruleheaderruleeq
        ruleheaderruleeq :: (RuleHeader,Rule) -> (RuleHeader,Rule) -> Bool
        ruleheaderruleeq (PfxHead _ a _ _,Pfx _ _ _ _ _) (PfxHead _ b _ _,Pfx _ _ _ _ _) = if (a == b)
                                                                                               then True
                                                                                               else False
        ruleheaderruleeq (SfxHead _ a _ _,Sfx _ _ _ _ _) (SfxHead _ b _ _,Sfx _ _ _ _ _) = if (a == b)
                                                                                               then True
                                                                                               else False
        ruleheaderruleeq (_,_) (_,_) = False
        --------------------------------

--prefinalrule -> To prepare to print 
--the affixfile from the results of
--grouptorule.
prefinalrule :: [[(RuleHeader,Rule)]] -> [([RuleHeader],[Rule])]
prefinalrule [] = []
prefinalrule xs = combineruleheaderrule (grabruleheader xs) (grabrules xs) 
    where
        --Nested Function Definitions.--
        --combineruleheaderrule
        combineruleheaderrule :: [[RuleHeader]] -> [[Rule]] -> [([RuleHeader],[Rule])]
        combineruleheaderrule [] []         = []
        combineruleheaderrule [] (_:_)      = []
        combineruleheaderrule (_:_) []      = [] 
        combineruleheaderrule (x:xs) (y:ys) = [(x,y)] ++ (combineruleheaderrule xs ys)
        --grabruleheader 
        grabruleheader :: [[(RuleHeader,Rule)]] -> [[RuleHeader]]
        grabruleheader [] = []
        grabruleheader xs = L.map (L.nub) (L.map (L.map (fst)) xs)
        --grabrules
        grabrules :: [[(RuleHeader,Rule)]] -> [[Rule]]
        grabrules [] = []
        grabrules xs = L.map (L.map (snd)) xs
        --------------------------------

--finalrule -> To prepare for the final affix file
--from the results of prefinalrule.
finalrule :: [([RuleHeader],[Rule])] -> [([String],[String])]
finalrule [] = []
finalrule ((x,y):xs) = [(nestedruleheadertostring x,nestedruletostring y)] ++ (finalrule xs)
    where
        --Nested Function Definitions.--
        --nestedruleheadertostring
        nestedruleheadertostring :: [RuleHeader] -> [String]
        nestedruleheadertostring [] = []
        nestedruleheadertostring (x:xs) = [ruleheadertostring x] ++ (nestedruleheadertostring xs)
        --nestedruletostring
        nestedruletostring :: [Rule] -> [String]
        nestedruletostring [] = []
        nestedruletostring (x:xs) = [ruletostring x] ++ (nestedruletostring xs)
        --ruleheadertostring
        ruleheadertostring :: RuleHeader -> String
        ruleheadertostring (PfxHead a b c d) = a ++ "  " ++ b ++ "  " ++ [c] ++ "  " ++ (show d)
        ruleheadertostring (SfxHead a b c d) = a ++ "  " ++ b ++ "  " ++ [c] ++ "  " ++ (show d)
        --ruletostring
        ruletostring :: Rule -> String
        ruletostring (Pfx a b c d e) = a ++ "  " ++ b ++ "  " ++ (show c) ++ "  " ++ d ++ "  " ++ e
        ruletostring (Sfx a b c d e) = a ++ "  " ++ b ++ "  " ++ (show c) ++ "  " ++ d ++ "  " ++ e
        --------------------------------

--firstfinalrule
firstfinalrule :: [([String],[String])] -> [[[String]]]
firstfinalrule [] = []
firstfinalrule (x:xs) = [finaltupletolist x] ++ (firstfinalrule xs)
    where
        --Nested Function Definitions.--
        --finaltupletolist
        finaltupletolist :: ([String],[String]) -> [[String]]
        finaltupletolist (x,y) = [x] ++ [y]
        -------------------------------- 

--secondfinalrule 
secondfinalrule :: [[[String]]] -> [[String]]
secondfinalrule [] = []
secondfinalrule xs = singlechunker (concatenate xs) 
    where
        --Nested Function Definitions.--
        --singlechunker
        singlechunker :: [[String]] -> [[String]]
        singlechunker [] = []
        singlechunker (x:xs) = if (L.length x > 1)
                                   then (S.chunksOf 1 x) ++ (singlechunker xs)
                                   else [x] ++ (singlechunker xs)
        --concatenate
        concatenate :: [[[String]]] -> [[String]]
        concatenate [] = []
        concatenate xs = L.concat xs
        --------------------------------

--thirdfinalrule
thirdfinalrule :: [[String]] -> (AffixFileEncoding,[[String]])
thirdfinalrule xs = ((AffixFileEncoding "SET UTF-8"),xs)

--fourthfinalrule
fourthfinalrule :: (AffixFileEncoding,[[String]]) -> (String,[[String]])
fourthfinalrule (x,y) = (affixfileencodingtostring x,y)
    where
        --Nested Function Definitions.--
        --affixfileencodingtostring
        affixfileencodingtostring :: AffixFileEncoding -> String
        affixfileencodingtostring (AffixFileEncoding a) = a
        --------------------------------

--finalprintlist
finalprintlist :: (String,[[String]]) -> [[String]]
finalprintlist ([],[]) = []
finalprintlist (x,y) = [x] : y

{------------------------}
{----------------------------------------------------}
{---------------------------}

{-IO Function.-}

--jsonparse -> This function will parse the JSON input files.
jsonparse :: [IO.FilePath] -> String -> (IO.FilePath,Handle) -> IO [String]
jsonparse []     []    (tempnamed,temphd) = return []
jsonparse []     (_:_) _                  = return []   
jsonparse (x:xs) ys    (tempnamed,temphd) = do 
    --Decode the current json file.
    jsondecoded <- (A.decode <$> (LBSC.readFile (ys ++ x))) :: IO (Maybe Paradigm)
    --Grab each part of decoded JSON.
    let paradigm = grabparadigm jsondecoded
    let slots    = grabslots jsondecoded
    let words    = parseMaybe parseinput =<< (grabwords jsondecoded)
    --Add these to a temp file. 
    IO.hPutStrLn temphd paradigm   
    IO.hPutStrLn temphd (show slots)
    IO.hPutStrLn temphd (show words)
    --Recurse through rest of list.
    jsonparse xs ys (tempnamed,temphd)

{--------------}

{-Main Function.-}

main :: IO ()
main = do
    --Get Command line arguments.
    cmdargs <- En.getArgs
    case cmdargs of
        [] -> error "No directory supplied."
        [allargs] ->  do --Load in all files in allargs.
                         alljson <- D.listDirectory allargs
                         
                         --Create a temporary file.
                         (tempfile,temph) <- Temp.openTempFile "." "parsed.txt" 
                       
                         --Run jsonparse on all inputs.
                         _ <- jsonparse alljson allargs (tempfile,temph)
                      
                         --Seek to the beginning of the temporary file.
                         hSeek temph AbsoluteSeek 0

                         --Read temporary file into [[String]] 
                         fulltempread <- IO.hGetContents temph
                      
                         --Prepare to print out affixfile with finalaffixlist. 
                         let finalaffixlist = finalprintlist
                                              (fourthfinalrule
                                              (thirdfinalrule
                                              (secondfinalrule 
                                              (firstfinalrule 
                                              (finalrule 
                                              (prefinalrule 
                                              (grouptorule 
                                              (sorttorule 
                                              (torule 
                                              (fourthnexttorule concatenator lengthmapper))))))))))
                                                  where
                                                      --Nested Definitions.-- 
                                                      concatenator = (L.concat
                                                                     (thirdnexttorule
                                                                     (secondnexttorule
                                                                     (firstnexttorule
                                                                     (preparetorule
                                                                     (wordandprerule
                                                                     (finalsimplifytoprerule
                                                                     (simplifytoprerule
                                                                     (toprerule
                                                                     (pfxsfx
                                                                     (simplifypresufcheck
                                                                     (presufcheck
                                                                     (truefinalreader
                                                                     (prefinalreader
                                                                     (midfinalreader
                                                                     (midreader
                                                                     (beginreader
                                                                     (readchunker
                                                                     (linefeed fulltempread)))))))))))))))))))
                                                      lengthmapper = (thirdrulelengthmapper
                                                                     (thirdnexttorule
                                                                     (secondnexttorule
                                                                     (firstnexttorule
                                                                     (preparetorule
                                                                     (wordandprerule
                                                                     (finalsimplifytoprerule
                                                                     (simplifytoprerule
                                                                     (toprerule
                                                                     (pfxsfx
                                                                     (simplifypresufcheck
                                                                     (presufcheck
                                                                     (truefinalreader
                                                                     (prefinalreader
                                                                     (midfinalreader
                                                                     (midreader
                                                                     (beginreader
                                                                     (readchunker
                                                                     (linefeed fulltempread)))))))))))))))))))
                                                      -----------------------
                                                      
                         --Print the result of secondfinalruler.
                         IO.writeFile "out.aff" $
                             (PB.render $
                             (PB.hsep 2 PB.left . L.map (PB.vcat PB.left) . L.map (L.map (PB.text)))
                             (L.transpose finalaffixlist))
                     
                         --Close temporary file.
                         hClose temph

                         --Delete temporary file.
                         removeFile tempfile      

        _ -> error "More than one directory provided."     
{----------------}
