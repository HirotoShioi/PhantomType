{-# LANGUAGE TypeApplications #-}

import Data.Char (chr, ord)

newtype Message = Message String deriving Show

encrypt :: Message -> Message
encrypt (Message c ) = Message (encContent c)
  where 
    encContent :: String -> String
    encContent str = let toNum = map ord str
                     in foldr (\x acc -> show x ++ " " ++ acc) [] toNum

decrypt :: Message -> Message
decrypt (Message c ) = Message (decContent c)
  where
    decContent :: String -> String
    decContent str = let numList = words str
                         decryptC = map (chr . read @ Int)
                     in  decryptC  numList