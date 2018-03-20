{-# LANGUAGE TypeApplications #-}

module EncryptPhantomExtra (
    Enc
  , Dec
  , message
  , encrypt
  , decrypt
  ) where

import Data.Char (chr, ord)

data Encrypted
data Plain

newtype Message a = Message String deriving Show

type Enc = Message Encrypted
type Dec = Message Plain

message :: String -> Dec
message = Message

encrypt :: Dec -> Enc
encrypt (Message c) = Message (encContent c)
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Enc -> Dec
decrypt (Message c) = Message (decContent c)
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList