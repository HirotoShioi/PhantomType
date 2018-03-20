{-# LANGUAGE TypeApplications #-}

module EncryptPhantom (
  Message
  , Encrypted
  , Decrypted
  , message
  , encrypt
  , decrypt
  ) where

import Data.Char (chr, ord)

data Encrypted
data Decrypted

data Message a = Message {
               content :: String
             , to      :: Address
             , from    :: Address
             } deriving Show

type Address = String

message :: String -> Address -> Address -> Message Decrypted
message = Message

encrypt :: Message Decrypted -> Message Encrypted
encrypt (Message c t f) = Message (encContent c) t f
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message Encrypted -> Message Decrypted
decrypt (Message c t f) = Message (decContent c) t f
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList