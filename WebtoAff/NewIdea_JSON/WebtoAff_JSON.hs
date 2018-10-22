{-=Spellit=-}
{-=webtoaff: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Pre-Alpha=-}
{-=Synopsis:  This Haskell Script will contain the=-}
{-=necessary functions to parse incoming JSON files=-}
{-=from the RESTful API.=-}

{-Syntax Extensions.-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{--------------------}

{-Module-}

module WebtoAff_JSON where

{--------}

{-Imports-}

import Control.Monad (mzero)
import Data.Aeson as A
import Data.ByteString.Lazy.Char8 as LBSC
import Data.Maybe as M
import Data.Text

{---------}

{-Custom Data Declarations-}
{-and Instances.-}

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
{----------------}
{--------------------------}
