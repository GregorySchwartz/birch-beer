{- birch-beer
Gregory W. Schwartz

Blah's the blah in the blah.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Options.Generic

-- Local
import Types
import Lib

-- | Command line arguments
data Options = Options { input  :: Maybe String
                               <?> "(FILE) The input file."
                       , output :: Maybe String
                               <?> "(FILE) The output file."
                       }
               deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = firstLetter }
          
instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers 

main :: IO ()
main = do
    opts <- getRecord "birch-beer, Gregory W. Schwartz.\
                      \ Does the blah in the blah."
    return ()
