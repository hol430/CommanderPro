module Comms (
    sendToDevice
) where

import qualified System.HIDAPI as Hidapi
import qualified Data.ByteString
import Protocol
import Text.Printf

-- Send a list of bytes to the device and print
-- both output and response to stdout.
sendToDevice :: Hidapi.Device -> [Byte] -> IO ()
sendToDevice handle bytes = do
    let outbuf = pad 63 bytes

    putStr "SEND "
    printBytes outbuf
    res <- Hidapi.write handle $ Data.ByteString.pack outbuf

    inbuf <- Hidapi.read handle 16
    putStr "RECV "
    printBytes $ Data.ByteString.unpack inbuf

-- Pad a byte list out to a given length with zeroes.
pad :: Int -> [Byte] -> [Byte]
pad n x = x ++ replicate (n - length x) 0x00

-- Print a byte list to stdout.
printBytes :: [Byte] -> IO ()
printBytes x = do
    putStrLn . unwords $ printf "%02x" <$> x
