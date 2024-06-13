# Implementation of a 32-bit Single Cycle MIPS Processor. <br>

**NOTE** This specific processor does not support the jump instruction

### The Instruction Memory in this specific example contains the following instructions:
```vhdl
  addi $3, $0, 1     <-> 001000 00000 00011 0000000000000001

  addi $5, $0, 3     <-> 001000 00000 00101 0000000000000011
  
  L1: add $6, $3, $0 <-> 000000 00011 00000 00110 00000 100000
  
  sw $6, 0($4)       <-> 101011 00100 00110 0000000000000000
  
  addi $3, $3, 1     <-> 001000 00011 00011 0000000000000001
  
  addi $4, $4, 1     <-> 001000 00100 00100 0000000000000001
  
  addi $5, $5, -1    <-> 001000 00101 00101 1111111111111111
  
  bne $5, $0, L1     <-> 000101 00000 00101 1111111111111011
```
