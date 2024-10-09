module Example.CPU where

import Clash.Prelude
import Data.Int

data Instruction n numType
    = Multiply (Reg n) (Reg n) (Reg n)
    | Add      (Reg n) (Reg n) (Reg n)
    | Absolute (Reg n)         (Reg n)
    | Literal  numType         (Reg n)
    | Jump     Int8

type Reg n = Index n

data ALUInstruction
    = Add'
    | Multiply'
    | Absolute'

data PCInstruction
    = Jump' Int8
    | Suck


decode :: (KnownNat n1, KnownNat n2, Num d) => Vec n1 d -> Instruction n2 d -> (ALUInstruction, PCInstruction, d, d, Reg n2)
decode regs (Add x y z) = 
    (Add', Suck, regs !! x, regs !! y, z)

decode regs (Multiply x y z) = 
    (Multiply', Suck, regs !! x, regs !! y, z)


decode regs (Absolute x z) = 
    (Absolute', Suck, regs !! x, undefined , z)

decode _ (Literal x z) = 
    (Add', Suck, x, 0, z)

decode _ (Jump z) = 
    (undefined, Jump' z, undefined, undefined, 0)

alu :: Num a => ALUInstruction -> a -> a -> a
alu Add'        x y = x + y
alu Multiply'   x y = x * y
alu Absolute'   x _ = abs x

nextPC :: Int8 -> PCInstruction -> Int8
nextPC pc Suck      = pc + 1
nextPC _  (Jump' n) = n

proc :: (Num b, KnownNat n1, KnownNat n2, KnownNat n3) => Vec n3 (Instruction n2 b) -> (Vec n1 b, Int8) -> p -> ((Vec n1 b, Int8), b)
proc instrs (regs, pc) _ = ((regs'', pc'), result)
    where
        (aluInstruction, pcInstruction, x, y, wrAddr) = 
            decode regs (instrs !! pc)
        result = alu aluInstruction x y
        regs'  = replace wrAddr result regs
        regs'' = replace (0 :: Integer)      0      regs'
        pc'    = nextPC pc  pcInstruction

instrss :: Vec 4 (Instruction 3 Int16)
instrss = Literal 5 1
        :> Literal 7 2
        :> Add 1 2 1
        :> Jump 2
        :> Nil

regss :: Vec 3 Int16
regss = 0 :> 0 :> 0 :> Nil

results :: HiddenClockResetEnable dom => Signal dom Int16
results = 
    mealy (proc instrss) (regss, 0 :: Int8) (pure ())


topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Int16
topEntity = exposeClockResetEnable results
