{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Example.Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen
import Clash.Annotations.SynthesisAttributes


createDomain vSystem{vName="Input", vPeriod=20000}

createDomain vSystem{vName="Dom20MHz", vPeriod=50000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "blinker"
    , t_inputs = [ PortName "CLOCK_50"
                 , PortName "KEY0"
                 , PortName "KEY1"
                 ]
    , t_output = PortName "LED"
    }) #-}

topEntity ::
  -- | Incoming clock
  --
  -- Annotate with attributes to map the argument to the correct pin,
  -- with the correct voltage settings, on the DE0-Nano development kit.
  Clock Input
    `Annotate` 'StringAttr "chip_pin" "R8"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->
  -- | Reset signal, straight from KEY0
  Signal Input Bool
    `Annotate` 'StringAttr "chip_pin" "J15"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->
  -- | Mode choice, straight from KEY1. See 'LedMode'.
  Signal Dom20MHz Bit
    `Annotate` 'StringAttr "chip_pin" "E1"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->
  -- | Output containing 8 bits, corresponding to 8 LEDs
  --
  -- Use comma-seperated list in the "chip_pin" attribute to maps the
  -- individual bits of the result to the correct pins on the DE0-Nano
  -- development kit
  Signal Dom20MHz (BitVector 8)
    `Annotate` 'StringAttr
                "chip_pin" "L3, B1, F3, D1, A11, B13, A13, A15"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\""
topEntity clk50Mhz rstBtn modeBtn =
  -- Connect our clock, reset, and enable lines to the corresponding
  -- /implicit/ arguments of the 'mealy' and 'isRising' function.
  exposeClockResetEnable
    (mealy blinkerT initialStateBlinkerT . isRising 1)
    clk20Mhz
    rstSync
    en
    modeBtn
  where
  -- Enable line for subcomponents: we'll keep it always running
  en = enableGen
 
  -- Start with the first LED turned on, in rotate mode, with the counter
  -- on zero
  initialStateBlinkerT = (1, Rotate, 0)
 
  -- Signal coming from the reset button is low when pressed, and high when
  -- not pressed. We convert this signal to the polarity of our domain with
  -- 'unsafeFromLowPolarity'.
  rst = unsafeFromActiveLow rstBtn
 
  -- Instantiate a PLL: this stabilizes the incoming clock signal and
  -- indicates when the signal is stable. We're also using it to transform
  -- an incoming clock signal running at 50 MHz to a clock signal running at
  -- 20 MHz.
  (clk20Mhz, pllStable) =
    altpll
      @Dom20MHz
      (SSymbol @"altpll20")
      clk50Mhz
      rst
 
  -- Synchronize reset to clock signal coming from PLL. We want the reset to
  -- remain asserted while the PLL is NOT stable, hence the conversion with
  -- 'unsafeFromLowPolarity'
  rstSync =
    resetSynchronizer
      clk20Mhz
      (unsafeFromActiveLow pllStable)
data LedMode
    = Rotate
    | Complement
    deriving (Generic, NFDataX)

flipMode :: LedMode -> LedMode
flipMode Rotate     = Complement
flipMode Complement = Rotate

blinkerT ::
    (BitVector 8, LedMode, Index 6660000) ->
    Bool ->
    ((BitVector 8, LedMode, Index 6660000), BitVector 8)

blinkerT (leds, mode, cntr) key1R = ((ledsN, modeN, cntrN), leds)
    where
        -- clock frequency = 20e6 (20 MHz)
        -- led update rate = 333e-3 (every 333ms)
        cnt_max = maxBound :: Index 6660000

        cntrN | cntr == cnt_max = 0
              | otherwise = cntr + 1

        modeN | key1R   = flipMode mode
              | otherwise = mode

        ledsN | cntr == 0 =
                case mode of
                    Rotate -> rotateL leds 1
                    Complement -> complement leds
              | otherwise = leds
