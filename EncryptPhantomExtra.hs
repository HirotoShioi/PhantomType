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
data Decrypted

data Message a = Message {
               content :: String
             , to      :: Address
             , from    :: Address
             } deriving Show

type Address = String

type Enc = Message Encrypted
type Dec = Message Decrypted

message :: String -> Address -> Address -> Dec
message = Message

encrypt :: Dec -> Enc
encrypt (Message c t f) = Message (encContent c) t f
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Enc -> Dec
decrypt (Message c t f) = Message (decContent c) t f
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList