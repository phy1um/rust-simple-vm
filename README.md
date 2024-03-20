# Simple VM

A simple 16-bit virtual machine, written to learn Rust basics.

## Technical Details

This machine has 8 general purpose registers named `A B C M SP BP PC Zero`.
These are all valid in any position of any instruction that takes a register.
As the names suggest, some have intended uses:

* A, B, C and M are general purpose. M is intended to be the page register
when that is needed.
* SP is the stack pointer
* BP is the base pointer for function frames
* PC is the program counter
* Zero is always == 0. Writes to this register do nothing.

Registers are 16-bits. Memory is accessed with 32-bit addresses, created by combining 2 registers.

### Instructions 

Instructions are 16-bits and have the following bit formats:

#### Type A (loading constants):
` [ 1 R R R I I I I | I I I I I I I I ] `

`REG[R] = Literal12Bit(I)`

This instruction type sets a register to an unsigned 12bit constant.
It does not support negative numbers, you will end up with a positive 
12bit number in the 16bit register.

#### Type B (everything else):
` [ 0 X X X Y Y Y P | P P P P Z Z Z Z ] `

These instructions operate on up to 3 registers in X, Y and Z. P is an
opcode, which is described in the table below. C is sometimes interpreted as
a 4bit nibble, or as a selector to `Stack` and `Test` instructions - like a 
secondary opcode.

Sometimes fields X, Y and Z are combined into 4, 7 and 10 bit literals.
In this case, they are always combined with Z in the least significant 
position, then Y, then X.

* Nibble (4bit): ZZZZ
* Lit7Bit: YYY ZZZZ
* Lit10Bit: XX XYYY ZZZZ

##### Opcodes
* Add (0x1):
  - `REG[C] = REG[X] + REG[Y]`
* Sub (0x2):
  - `REG[C] = REG[X] - REG[Y]`
* AddImm (0x3):
  - `REG[X] += Lit7Bit(Y,Z)`
* AddImmSigned (0x4): 
  - `REG[X] += Lit7Bit(Y,Z)`
  - Interpret Lit7Bit(B,C) as a signed 7bit 2s compliment number, then sign extend this to 16bits before performing addition
* ShiftLeft (0x5):
  - `REG[Y] = REG[X] << Nibble(Z)`
* ShiftRightLogical (0x6):
  - `REG[Y] = REG[X] >> Nibble(Z)`
* ShiftRightArithmetic (0x7): 
  - `REG[Y] = REG[X] >> Nibble(Z)`
* Load (0x8): 
  - `REG[X] = MEM[REG[Y] + REG[Z]<<16]`
* Store (0x9): 
  - `MEM[REG[Y] + REG(Z)<<16] = REG[X]`
* JumpOffset (0xa): 
  - `REG[PC] += Lit10Bit(X,Y,Z)`
* SetAndSave (0x10):
  - `tmp = REG[Y]; REG[Z] = REG[X]; REG[X] = tmp`
* AddAndSave (0x11):
  - `tmp = REG[Y]; REG[Z] += REG[X]; REG[X] = tmp`|
* Test (0xb):
  - `if TestOp(REG[X], REG[Y]) then set FLAGS[Compare]`
  - See section below for a list of TestOps
* AddIf (0xc): 
  - `if FLAGS[Compare] then { REG[X] = REG[Y]+2\*Nibble(Z); FLAGS[Compare] = false }`
* Stack (0xd): 
  - `StackOp(REG[X], REG[Y])`
  - See section below for a list of StackOps
* LoadStackOffset (0xe): 
  - `REG[X] = MEM[REG[Y] - Nibble(Z)\*2]`
* System (0xf):
  - See section below
 
#### Test Ops

A test instruction compares the values in 2 registers X and Y, then 
conditionally sets the `Compare` flag. 
The list of possible test operations are:

|Index  |Name       | Operation                 |
|-------|-----------|---------------------------|
|      0| Eq | X == Y |
|      1| Neq | X != Y | 
|      2| Lt | X < Y |
|      3| Lte | X <= Y |
|      4| Gt | X > Y |
|      5| Gte | X >= Y |
|      6| BothZero | X == 0 && Y == 0 |
|      7| EitherNonZero | X != 0 || Y != 0|
|      8| BothNonZero | X != 0 && Y != 0 |

#### Stack Ops

A stack instruction operates on a stack in memory. This instruction takes 2 registers.
The first register is an argument the the stack operation. _The second register is the 
address of the stack in memory_, which is usually the register SP but could be anything.
This means that SP does not have to be the stack register, and that you can do other clever things
with this stack instruction.
For 2 registers X and Y, the list of possible stack operations are:

|Index  |Name       | Operation                 |
|-------|-----------|---------------------------|
|      0| Pop | REG[X] = MEM[REG[Y]-2]; REG[Y] += 2 |
|      1| Push | MEM[REG[Y]] = REG[X]; REG[Y]+=2 | 
|      2| Peek | REG[X] = MEM[REG[Y]-2] |
|      3| Swap | x=POP; y=POP; PUSH x; PUSH y |
|      4| Dup | x=PEEK; PUSH x |
|      5| Rotate | x=POP; y=POP; z=POP; PUSH x; PUSH z; PUSH y; |
|      6| Add | x=POP; y=POP; PUSH (x+y) |
|      7| Sub | x=POP; y=POP; PUSH (x-y) |

#### System

The `System` operation performs an action with the host system. This usually involves IO,
or some actions to control the running of the VM. The work of the system operation is 
deferred to a _System Handler_, and each handler attached to the VM is uniquely identified by
a 1-bye _System Handler Index_. There are 2 modes of determining this index:

If X is the `Zero` register, then the System Handler Index is taken from Nibble(Z). As
this can only be 16 possible indices, we should try to put important system operations
at index 0-15. REG[X] is taken as an argument to the handler.

If X is any other register, then the handler index is REG[X], and Nibble(Z) is taken
as an argument to the handler.

A handler may read and modify any machine state, it is not restricted to just
examining the registers passed in. Some calls may depend on specific values in
registers. This is all dependent on the host, refer to host details for the 
operations performed by each system handler, and what the handler indices are.

