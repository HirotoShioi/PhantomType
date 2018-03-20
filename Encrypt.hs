{-# LANGUAGE TypeApplications #-}

import Data.Char (chr, ord)

type Address = String

data Message = Message {
               content :: String
             , to      :: Address
             , from    :: Address
             } deriving Show

encrypt :: Message -> Message
encrypt (Message c t f) = Message (encContent c) t f
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message -> Message
decrypt (Message c t f) = Message (decContent c) t f
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList