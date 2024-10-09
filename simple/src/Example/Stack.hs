module Stack where

import Clash.Prelude

data SInstr value 
            = Push value 
            | Pop 
            | PopPush value
            deriving (Show)

stack :: (KnownNat n, Enum b1, Num b2, Num b1) => (Vec n b2, b1) -> SInstr b2 -> ((Vec n b2, b1), b2)
stack (mem, sp) instr = ((memN, spN), out)
    where
        (memN, spN) = case instr of
            Push value      -> (replace sp value mem, sp+1)
            Pop             -> (mem, sp-1)
            PopPush value   -> (replace (sp-1) value mem, sp)
        out = case instr of
            Push _      -> 0
            Pop         -> mem !! sp
            PopPush _   -> mem !! (sp - 1)
