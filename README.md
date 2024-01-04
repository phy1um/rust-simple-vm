# Simple VM

A simple 16-bit virtual machine, written to learn Rust basics.

## Technical Details

This machine has 8 registers:

* A, B, C and M are general purpose
* SP is the stack pointer
* BP is the base pointer for function frames
* PC is the program counter
* FLAGS contains 16 status bits, these are currently undefined and always 0

Registers are 16-bits. Memory is accessed with 16-bit addresses, meaning there is
are 65536 bytes of addressable memory.

### Instructions 

Instructions are 16-bits and have the following bit format:

` [ O O O O O O O O | A A A A B B B B ] `

Here, O is the operation (eg PUSH or ADD). A and B are arguments to that operation,
and in some cases are combined into a single 8-bit value.

| Operation     | A             | B            | Description           |
|---------------|---------------|--------------|-----------------------|
| Nop           | -             | -            | Do nothing            |
| Push          | 8bit literal  | Join with A  | Push literal to stack |
| PopRegister   | Register      | -            | Pop value to register |
| AddStack      | -             | -            | Pop 2 values, push their sum |
| AddRegister   | Register      | Register     | A = A + B  |


