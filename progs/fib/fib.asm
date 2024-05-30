.defvar SIG_EXIT $f0
.defvar n 5

.defmacro Call Stack SP SP Push / Stack BP SP Push / Add SP Zero BP / Imm C !0 / SetAndSave C C PC
.defmacro Return LoadStackOffset M BP 2 / LoadStackOffset BP BP 1 / Add M Zero SP / Add !0 Zero PC 

; setup machine
Imm SP $100
ShiftLeft SP SP 4
Imm BP $800 

Imm A !n
.Call !fib
Add A Zero M
Imm A !SIG_EXIT
System A Zero 0

; fib[n] where n is in register A, 
:fib
Stack C SP Push
Imm C !base
Test A Zero Lte
AddIf PC C 0
Stack A SP Push
Imm B 1
Sub A B A
.Call !fib
Add A Zero C
Imm B 2
Stack A SP Pop
Sub A B A
Stack C SP Push
.Call !fib
Stack C SP Pop
Add A C A
Stack C SP Pop
.Return C

:base
Imm A 1
.Return C

; f(n) = f(n-1) + f(n-2)
