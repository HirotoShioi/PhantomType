{-# LANGUAGE TypeApplications #-}

module EncryptPhantom (
  Message
  , Encrypted
  , Plain
  , message
  , encrypt
  , decrypt
  ) where

import Data.Char (chr, ord)

data Encrypted
data Plain

newtype Message a = Message String deriving Show

message :: String -> Message Plain
message = Message

encrypt :: Message Plain -> Message Encrypted
encrypt (Message c) = Message (encContent c)
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message Encrypted -> Message Plain
decrypt (Message c) = Message (decContent c)
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList

-- error1 = decrypt $ message "Hello word"

-- error2 = encrypt $ encrypt $ message "Hello word"