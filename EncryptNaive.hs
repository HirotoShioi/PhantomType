{-# LANGUAGE TypeApplications #-}

module EncryptNaive (
    message
  , encrypt
  , decrypt
) where

import Data.Char (chr, ord)

data Message = Message String Bool deriving Show

message :: String -> Message
message c = Message c False 

encrypt :: Message -> Message
encrypt (Message c isEnc) = if isEnc
                                then error "Content is already encrypted"
                                else Message (encContent c) True
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message -> Message
decrypt (Message c isEnc) = if not isEnc
                                then error "Content is already decrypted"
                                else Message (decContent c) False
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList

error1 :: Message
error1 = decrypt $ message "Hello word"

error2 :: Message
error2 = encrypt $ encrypt $ message "Hello word"