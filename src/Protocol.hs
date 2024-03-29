module Protocol where

-- Credit to @pvh1987 and @PostalBlob
-- https:--github.com/audiohacked/OpenCorsairLink/issues/70#issue-288732095
-- https:--github.com/audiohacked/OpenCorsairLink/issues/70#issuecomment-380134782

import Data.Word
import Text.Regex

type Byte = Word8

class ToByte a where
    raw :: a -> Byte

class Pretty a where
    prettify :: a -> String

data Command = ReadStatus | GetFirmwareVersion | GetId | WriteDeviceId | StartFirmwareUpdate | GetBootloaderVersion | WriteTestFlag | GetThermometerConfig | GetTemperature | GetVoltage | GetFanConfig | GetFanSpeed | GetFanPower | SetFanPower | SetFanSpeed | SetFanConfigGraph | SetFanTempInfo | SetFanForce | SetFanMode | GetFanMode | GetLedStripMask | SetLedValue | SetLedColourValues | SetLedTrigger | SetLedClear | SetLedMode | SetLedTempInfo | SetLedGroupClear | SetLedModeUnknown | SetLedBrightness | GetLedCount | SetLedPortType

instance ToByte Command where

    -- --------------------------------------------------------------------
    -- Diagnostics
    -- --------------------------------------------------------------------

    -- GET device status
    raw ReadStatus = 0x01

    -- GET firmware version
    raw GetFirmwareVersion = 0x02

    -- Not sure what this does... maybe a request for an ID of some kind?
    raw GetId = 0x03

    -- SET device ID
    raw WriteDeviceId = 0x04

    -- Start firmware update
    raw StartFirmwareUpdate = 0x05

    -- GET bootloader version
    raw GetBootloaderVersion = 0x06

    -- Write test flag
    raw WriteTestFlag = 0x07

    -- --------------------------------------------------------------------
    -- Temperatures
    -- --------------------------------------------------------------------

    -- GET temperature sensor config. (Connected/Disconnected)
    raw GetThermometerConfig = 0x10

    -- GET temperature (for each of the connected sensors)
    raw GetTemperature = 0x11

    -- GET voltage (Measure 12V, 5V or 3.3V rail)
    raw GetVoltage = 0x12

    -- --------------------------------------------------------------------
    -- Fans
    -- --------------------------------------------------------------------

    -- GET fan mode configuration (Auto/Disconnected, 3-pin or 4-pin)
    raw GetFanConfig = 0x20

    -- GET fan speed in RPM
    raw GetFanSpeed = 0x21

    -- GET fan power (which units??)
    raw GetFanPower = 0x22

    -- Set fan power (in %)
    raw SetFanPower = 0x23

    -- SET fan speed (Fixed RPM)
    raw SetFanSpeed = 0x24

    -- SET fan configuration (Graph)
    raw SetFanConfigGraph = 0x25

    -- SET fan temperature info (if the group is chosen to be an external sensor)
    raw SetFanTempInfo = 0x26

    -- SET fan force (3 pin mode)
    raw SetFanForce = 0x27

    -- SetFanDetectionType
    raw SetFanMode = 0x28

    -- ReadFanDetectionType
    raw GetFanMode = 0x29

    -- --------------------------------------------------------------------
    -- LEDs
    -- --------------------------------------------------------------------

    -- ReadLedStripMask
    raw GetLedStripMask = 0x30

    -- WriteLedRgbValue
    raw SetLedValue = 0x31

    -- WriteLedColorValues
    raw SetLedColourValues = 0x32

    -- Apply LED changes.
    raw SetLedTrigger = 0x33

    -- WriteLedClear
    raw SetLedClear = 0x34

    -- WriteLedGroupSet
    raw SetLedMode = 0x35

    -- WriteLedExternalTemp
    raw SetLedTempInfo = 0x36

    -- WriteLedGroupsClear
    raw SetLedGroupClear = 0x37

    -- WriteLedMode
    raw SetLedModeUnknown = 0x38

    -- WriteLedBrightness
    raw SetLedBrightness = 0x39

    -- WriteLedCount
    raw GetLedCount = 0x3a

    -- WriteLedPortType
    raw SetLedPortType = 0x3B
    
-- --------------------------------------------------------------------
-- LED Channels
-- --------------------------------------------------------------------

data Channel = Channel1 | Channel2 deriving (Enum, Bounded)
instance ToByte Channel where

    -- LED Channel 1
    raw Channel1 = 0x00

    -- LED Channel 2
    raw Channel2 = 0x01

instance Show Channel where
    show Channel1 = "1"
    show Channel2 = "2"

-- --------------------------------------------------------------------
-- FAN IDs
-- --------------------------------------------------------------------

data Fan = Fan1 | Fan2 | Fan3 | Fan4 | Fan5 | Fan6
           deriving (Enum, Bounded, Show)

instance ToByte Fan where

    -- LED Fan 1
    raw Fan1 = 0x00

    -- LED Fan 2
    raw Fan2 = 0x0C

    -- LED Fan 3
    raw Fan3 = 0x18

    -- LED Fan 4
    raw Fan4 = 0x24

    -- LED Fan 5
    raw Fan5 = 0x30

    -- LED Fan 6
    raw Fan6 = 0x3C

instance Pretty Fan where
    prettify Fan1 = "1"
    prettify Fan2 = "2"
    prettify Fan3 = "3"
    prettify Fan4 = "4"
    prettify Fan5 = "5"
    prettify Fan6 = "6"

-- --------------------------------------------------------------------
-- LED Types
-- --------------------------------------------------------------------

data LedType =  LedTypeStrip
              | LedTypeHdFan
              | LedTypeSpFan
              | LedTypeMlFan
              deriving (Enum, Bounded)
instance ToByte LedType where

    -- RGB LED Strip
    raw LedTypeStrip = 0x0A

    -- RGB HD Fan
    raw LedTypeHdFan = 0x0C

    -- RGB SP Fan
    raw LedTypeSpFan = 0x01

    -- RGB ML Fan
    raw LedTypeMlFan = 0x04

instance Show LedType where
    show LedTypeStrip = "Strip LED"
    show LedTypeHdFan = "HD Fan"
    show LedTypeSpFan = "SP Fan"
    show LedTypeMlFan = "Ml Fan"

-- --------------------------------------------------------------------
-- LED Effects
-- --------------------------------------------------------------------

data Effect = RainbowWave
            | ColourShift
            | ColourPulse
            | ColourWave
            | Static
            | Temperature
            | Visor
            | Marquee
            | Blink
            | Sequential
            | Rainbow
            deriving (Enum, Bounded, Show)
instance ToByte Effect where

    -- Rainbow wave LED effect
    raw RainbowWave = 0x00

    -- Colour shift LED effect
    raw ColourShift = 0x01

    -- Colour pulse LED effect
    raw ColourPulse = 0x02

    -- Colour wave LED effect
    raw ColourWave = 0x03

    -- Static LED colour
    raw Static = 0x04

    -- Temperature LED effect
    raw Temperature = 0x05

    -- Visor LED effect
    raw Visor = 0x06

    -- Marquee LED effect
    raw Marquee = 0x07

    -- Blink LED effect
    raw Blink = 0x08

    -- Sequential (channel effect)
    raw Sequential = 0x09

    -- Rainbow LED effect
    raw Rainbow = 0x0A

instance Pretty Effect where
    prettify input = subRegex pattern str repl
        where pattern = mkRegex "([A-Z][a-z]*)([A-Z][a-z]*)"
              str     = show input
              repl    = "\\1 \\2"

-- --------------------------------------------------------------------
-- LED Effect Speeds
-- --------------------------------------------------------------------

data Speed = High | Medium | Slow deriving (Enum, Bounded, Show)
instance ToByte Speed where

    -- Fast LED speed
    raw High = 0x00

    -- Medium LED speed
    raw Medium = 0x01

    -- Slow LED speed
    raw Slow = 0x02

instance Pretty Speed where
    prettify = show

-- --------------------------------------------------------------------
-- LED Effect Directions
-- --------------------------------------------------------------------

data Direction = Backwards | Forwards deriving (Enum, Bounded, Show)
instance ToByte Direction where

    -- Backwards direction for LED effects.
    raw Backwards = 0x00

    -- Forwards direction for LED effects.
    raw Forwards = 0x01

instance Pretty Direction where
    prettify = show

-- --------------------------------------------------------------------
-- LED Colour Modes
-- --------------------------------------------------------------------

data ColourMode = Alternating | Random deriving (Enum, Bounded, Show)
instance ToByte ColourMode where

    -- Alternating colours
    raw Alternating = 0x00

    -- Random colours
    raw Random = 0x01

instance Pretty ColourMode where
    prettify = show
    
-- --------------------------------------------------------------------
-- LED Brightnesses
-- --------------------------------------------------------------------

data Brightness = Max | Med | Low | Zero deriving (Enum, Bounded, Show)
instance ToByte Brightness where

    -- 100% Brightness
    raw Max = 0x64

    -- 66% Brightness
    raw Med = 0x42

    -- 33% Brightness
    raw Low = 0x21

    -- 0% Brightness
    raw Zero = 0x00

instance Pretty Brightness where
    prettify Max = "100%"
    prettify Med = "66%"
    prettify Low = "33%"
    prettify Zero = "0%"
