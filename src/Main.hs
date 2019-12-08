module Main where

import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
import Comms
import qualified System.HIDAPI as Hidapi
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Word
import Options
import Protocol
import qualified Data.ByteString
import Control.Monad.Trans(liftIO)

main :: IO ()
main = do
    let effects         = map prettify [(minBound :: Effect) ..]
        speeds          = map prettify [(minBound :: Speed) ..]
        directions      = map prettify [(minBound :: Direction) ..]
        modes           = map prettify [(minBound :: ColourMode) ..]
        brightnesses    = map prettify [(minBound :: Brightness) ..]
        fans            = map prettify [(minBound :: Fan) ..]

    initGUI
    Hidapi.init

    builder <- builderNew
    builderAddFromFile builder "ui.glade"
    window <- builderGetObject builder castToWindow "window"
    on window deleteEvent $ liftIO mainQuit >> return False
    grid <- builderGetObject builder castToGrid "grid"

    createComboInGrid effects grid 0
    createComboInGrid speeds grid 1
    createComboInGrid directions grid 2
    createComboInGrid modes grid 3
    createComboInGrid brightnesses grid 4

    comboFan1 <- createCombo fans
    comboFan2 <- createCombo fans
    hboxFans <- builderGetObject builder castToBox "hboxFans"
    boxPackStart hboxFans comboFan1 PackNatural 0
    boxPackEnd hboxFans comboFan2 PackGrow 0
    boxReorderChild hboxFans comboFan1 0

    btnApply <- builderGetObject builder castToButton "btnApply"
    on btnApply buttonActivated $ do
        let vendorId    = 0x1b1c
            productId   = 0x0c10
        opts <- getOpts grid
        print("Applying changes...")

        handle <- Hidapi.open vendorId productId Nothing
        apply handle opts
        Hidapi.close handle

    widgetShowAll window
    mainGUI

createCombo :: [String] -> IO (ComboBox)
createCombo contents = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo) $ map T.pack contents
    return combo

createComboInGrid :: [String] -> Grid -> Int -> IO ()
createComboInGrid contents grid row = do
    combo <- createCombo contents
    gridAttach grid combo 1 row 1 1

-- Hardcoded config for testing purposes
doStaticConfig :: IO ()
doStaticConfig = do
    let
        vendorId    = 0x1b1c
        productId   = 0x0c10
        red         = sRGB 1 0 0
        green       = sRGB 0 1 0
        blue        = sRGB 0 0 1
        ledConfig   = LedOptions ColourPulse High Forwards Alternating red red red
        opts        = Options SetLedMode Channel1 Fan1 LedTypeHdFan Max ledConfig

    --details <- Hidapi.enumerate (Just vendorId) (Just productId)
    --print (show details)
    handle <- Hidapi.open vendorId productId Nothing

    apply handle $ allFans ledConfig opts

    Hidapi.close handle

getOpts :: Grid -> IO ([Options])
getOpts grid = do
    effect <- getSelection grid 0 :: IO (Effect)
    speed <- getSelection grid 1 :: IO (Speed)
    direction <- getSelection grid 2 :: IO (Direction)
    mode <- getSelection grid 3 :: IO (ColourMode)
    brightness <- getSelection grid 4 :: IO (Brightness)
    
    colour1 <- colourToColor <$> getColourFromGrid grid 5
    colour2 <- colourToColor <$> getColourFromGrid grid 6
    colour3 <- colourToColor <$> getColourFromGrid grid 7

    fanFrom <- getFan grid 0
    fanTo <- getFan grid 2

    let ledConfig = LedOptions effect speed direction mode colour1 colour2 colour3
        template = Options SetLedMode Channel1 Fan1 LedTypeHdFan brightness ledConfig
        opts = createForFans ledConfig template fanFrom fanTo
    
    mapM_ print opts
    return (opts)

getFan :: Grid -> Int -> IO (Fan)
getFan grid item = do
    (Just child) <- gridGetChildAt grid 1 8
    let box = castToBox child
    children <- containerGetChildren box
    let combo = castToComboBox $ children !! item
    i <- comboBoxGetActive combo

    return (toEnum i :: Fan)

--getToFan :: Grid -> IO (Fan)


getContents :: Maybe Box -> Int -> IO (String)
getContents Nothing _ = do return ("")
getContents (Just box) i = do
    widgets <- containerGetChildren box
    let widget = widgets !! i
        combo  = castToComboBox widget
    text <- getSelectedText combo
    return (text)

colourToColor :: Color -> Colour Double
colourToColor (Color r g b) = sRGB red green blue
    where red = getColourProp r
          green = getColourProp g
          blue = getColourProp b

getColourProp :: Word16 -> Double
getColourProp x = d / 65535
    where d = (fromIntegral . toInteger) x

getColourFromGrid :: Grid -> Int -> IO (Color)
getColourFromGrid grid row = do
    control <- gridGetChildAt grid 1 row
    colour <- getColour control
    return colour

getColour :: Maybe Widget -> IO (Color)
getColour Nothing = do error "Can't find colour"
getColour (Just widget) = do
    let colourButton = castToColorButton widget
    colour <- colorButtonGetColor colourButton
    return colour

getSelection :: Enum a => Grid -> Int -> IO (a)
getSelection grid row = do
    combo <- gridGetChildAt grid 1 row
    i <- getSelectedIndex combo
    return (toEnum i)

getSelectedIndex :: Maybe Widget -> IO (Int)
getSelectedIndex Nothing = do return (0)
getSelectedIndex (Just widget) = comboBoxGetActive combo
    where combo = castToComboBox widget

getSelectedText :: ComboBox -> IO (String)
getSelectedText combo = do
    text <- comboBoxGetActiveText combo
    let (Just val) = text
    if text == Nothing
        then return ("")
        else return (T.unpack val)

-- Apply specified LED config
apply :: Hidapi.Device -> [Options] -> IO ()
apply handle opts = do
    let
        bright      = raw . brightness $ opts !! 0
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
