-- Taak Lava
import Lava

halfAdder::(Bit,Bit) -> (Bit,Bit)
halfAdder (a,b) = (sum, carry)
  where sum = xor2(a,b)
        carry = and2(a,b)