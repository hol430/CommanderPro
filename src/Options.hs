module Options where

import Data.Colour
import Data.Colour.SRGB
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
