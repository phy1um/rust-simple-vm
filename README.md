# Simple VM

A simple 16-bit virtual machine, written to learn Rust basics.

## Technical Details

This machine has 8 general purpose registers named `A B C M SP BP PC Zero`.
These are all valid in any position of any instruction that takes a register.
As the names suggest, some have uses in convention:

* A, B, C and M are general purpose. M is intended to be the page register
when that is appropriate.
* SP is the stack pointer
* BP is the base pointer for function frames
* PC is the program counter
* Zero is always == 0. Writes to this register do nothing.

Registers are 16-bits. Memory is accessed with 32-bit addresses.

### Instructions 

Instructions are 16-bits and have the following bit formats:

#### Type A (loading constants):
` [ 1 R R R I I I I | I I I I I I I I ] `

`REGISTER[R] = Literal12Bit(I)`

This instruction type sets a register to an unsigned 12bit constant.
It does not support loading negative numbers, this requires extra work.

#### Type B (everything else):
` [ 0 A A A B B B M | M M M M C C C C ] `

These instructions operate on up to 3 registers in A, B and C. M is an
opcode, which is described in the table below. C is sometimes interpreted as
a 4bit nibble, or as a selector to `Stack` and `Test` instructions, like a 
secondary opcode.

Sometimes fields A, B and C are combined into 4, 7 and 10 bit literals.
In this case, they are always combined with C in the least significant 
position, then B, then A.

* Nibble: CCCC
* Lit7Bit: BBB CCCC
* Lit10Bit: AA ABBB CCCC


| Operation     | Effect                       | Notes |
|---------------|---------------|--------------|-----------------------|
| Add           | REG[C] = REG[A] + REG[B]     | |
| Sub           | REG[C] = REG[A] - REG[B]     | |
| AddImm        | REG[A] += Lit7Bit(B,C)       | |
| AddImmSigned  | REG[A] += Lit7Bit(B,C)       | Interpret Lit7Bit(B,C) as a signed 7bit 2s compliment number, then sign extend this to 16bits before performing the addition |
| ShiftLeft     | REG[B] = REG[A] << Nibble(C) | |
| ShiftRightLogical     | REG[B] = REG[A] >> Nibble(C) | |
| ShiftRightArithmetic     | REG[B] = REG[A] >> Nibble(C) | | 
| Load          | REG[A] = MEM[REG[B] + REG[C]<<16] | |
| Store         | MEM[REG[B] + REG(C)<<16] = REG[A] | |
| JumpOffset    | REG[PC] += Lit10Bit(A,B,C) | |
| SetAndSave    | tmp = REG[B]; REG[C] = REG[A]; REG[A] = tmp | |
| AddAndSave    | tmp = REG[B]; REG[C] += REG[A]; REG[A] = tmp | |
| Test          | if TestOp(REG[A], REG[B]) then set FLAGS[Compare] | |
| AddIf         | if FLAGS[Compare] then { REG[A] = REG[B]+2\*Nibble(C); FLAGS[Compare] = false } | |
| Stack         | StackOp(REG[A], REG[B]) | |
| LoadStackOffset | REG[A] = MEM[REG[B] - Nibble(C)\*2] | |
| System        | See section below | |
 
