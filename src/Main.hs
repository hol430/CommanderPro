module Main where
  
import Comms
import qualified System.HIDAPI as Hidapi
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Options
import Protocol
import qualified Data.ByteString

main :: IO ()
main = do
    let vendorId    = 0x1b1c
        productId   = 0x0c10
        
        red         = sRGB 1 0 0
        green       = sRGB 0 1 0
        blue        = sRGB 0 0 1
        ledConfig   = LedOptions LedEffectStatic High Forwards Alternating red green blue
        opts        = Options SetLedMode Channel1 Fan1 LedTypeHdFan Zero ledConfig

    Hidapi.init

    --details <- Hidapi.enumerate (Just vendorId) (Just productId)
    --print (show details)
    handle <- Hidapi.open vendorId productId Nothing

    apply handle $ allFans ledConfig opts

    Hidapi.close handle

apply :: Hidapi.Device -> [Options] -> IO ()
apply handle opts = do
    let bright      = raw . brightness $ opts !! 0
        init1       = [raw SetLedGroupClear]
        init2       = [raw SetLedClear]
        init3       = [raw SetLedBrightness, 0x00, bright]
        init4       = [raw SetLedModeUnknown, 0x00, 0x01]
        final       = [raw SetLedTrigger, 0xff]

    sendToDevice handle init1
    sendToDevice handle init2
    sendToDevice handle init3
    sendToDevice handle init4
    mapM_ (sendToDevice handle) (map toBytes opts)
    sendToDevice handle final
