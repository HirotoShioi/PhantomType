module Program where

import EncryptPhantom

test :: Message Decrypted
test = message "Hello world" "Hiroto" "Denis"

decryptedMessage :: Message Decrypted
decryptedMessage = decrypt $ encrypt test

encryptedMessage :: Message Encrypted
encryptedMessage = encrypt test