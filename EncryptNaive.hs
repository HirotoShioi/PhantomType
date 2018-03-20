{-# LANGUAGE TypeApplications #-}

module EncryptNaive (
    message
  , encrypt
  , decrypt
) where

import Data.Char (chr, ord)

type Address = String

data Message = Message {
               content :: String
             , to          :: Address
             , from        :: Address
             , isEncrypted :: Bool
             } deriving Show

message :: String -> Address -> Address -> Message
message c t f = Message c t f False 

encrypt :: Message -> Message
encrypt (Message c t f isEnc) = if isEnc
                                then error "Content is already encrypted"
                                else Message (encContent c) t f True
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message -> Message
decrypt (Message c t f isEnc) = if not isEnc
                                then error "Content is already decrypted"
                                else Message (decContent c) t f False
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList

error1 :: Message
error1 = decrypt $ message "Hello word" "Hiroto" "Denis"

error2 :: Message
error2 = encrypt $ encrypt $ message "Hello word" "Hiroto" "Denis"