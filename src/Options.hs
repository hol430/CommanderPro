module Options where

import Data.Colour
import Data.Colour.SRGB
import Data.Typeable
import Protocol

data Options = Options {
    command     :: Command,
    channel     :: Channel,
    fan         :: Fan,
    device      :: LedType,
    brightness  :: Brightness,
    ledOptions  :: LedOptions
}

data LedOptions = LedOptions {
    effect      :: Effect,
    speed       :: Speed,
    direction   :: Direction,
    colourMode  :: ColourMode,
    colour1     :: Colour Double,
    colour2     :: Colour Double,
    colour3     :: Colour Double
}

allFans :: LedOptions -> Options -> [Options]
allFans led opts = map (setFan led opts) fans
                   where fans = [Fan1, Fan2, Fan3, Fan4, Fan5, Fan6]

createForFans :: LedOptions -> Options -> Fan -> Fan -> [Options]
createForFans led opts from to = map (setFan led opts) fans
                                 where fans = enumFromTo from to


setFan :: LedOptions -> Options -> Fan -> Options
setFan l o f = o { fan = f, ledOptions = l}

-- TBI: Bytes 18 - 23 are 16-bit temperatures in big-endian.
-- These values are only used in temperature mode.

toBytes :: Options -> [Byte]
toBytes opts = [raw $ command opts,                 -- 0
                raw $ channel opts,                 -- 1
                raw $ fan opts,                     -- 2
                raw $ device opts,                  -- 3
                raw . effect $ ledOptions opts,     -- 4
                raw . speed $ ledOptions opts,      -- 5
                raw . direction $ ledOptions opts,  -- 6
                raw . colourMode $ ledOptions opts, -- 7
                0xff,                               -- 8
                channelRed rgb1,                    -- 9
                channelGreen rgb1,                  -- 10
                channelBlue rgb1,                   -- 11
                channelRed rgb2,                    -- 12
                channelGreen rgb2,                  -- 13
                channelBlue rgb2,                   -- 14
                channelRed rgb3,                    -- 15
                channelGreen rgb3,                  -- 16
                channelBlue rgb3]                   -- 17
                where
                    rgb1 = toSRGB24 $ colour1 (ledOptions opts)
                    rgb2 = toSRGB24 $ colour2 (ledOptions opts)
                    rgb3 = toSRGB24 $ colour3 (ledOptions opts)

colourToBytes :: Colour Double -> [Byte]
colourToBytes c = [r, g, b]
                where
                    rgb = toSRGB24 c
                    r = channelRed rgb
                    g = channelGreen rgb
                    b = channelBlue rgb

instance Show LedOptions where
    show opts = e ++ s ++ d ++ m ++ c1 ++ c2 ++ c3
        where   e  = "Effect:      " ++ (prettify $ effect opts) ++ "\n"
                s  = "Speed:       " ++ (prettify $ speed opts) ++ "\n"
                d  = "Direction:   " ++ (prettify $ direction opts) ++ "\n"
                m  = "Colour mode: " ++ (prettify $ colourMode opts) ++ "\n"
                c1 = "Colour 1:    " ++ (sRGB24show $ colour1 opts) ++ "\n"
                c2 = "Colour 2:    " ++ (sRGB24show $ colour2 opts) ++ "\n"
                c3 = "Colour 3:    " ++ (sRGB24show $ colour3 opts) ++ "\n"

instance Show Options where
    show opts = c ++ f ++ d ++ b ++ l
        where   c = "Channel:    " ++ (show $ channel opts) ++ "\n"
                f = "Fan:        " ++ (show $ fan opts) ++ "\n"
                d = "Device:     " ++ (show $ device opts) ++ "\n"
                b = "Brightness: " ++ (show $ brightness opts) ++ "\n"
                l = show $ ledOptions opts
