module Program where

import EncryptPhantom

test :: Message Plain
test = message "Hello world"

decryptMessage :: Message Plain
decryptMessage = decrypt $ encrypt test

encryptedMessage :: Message Encrypted
encryptedMessage = encrypt test