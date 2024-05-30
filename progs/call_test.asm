.offsetPC $10
.defmacro Call Imm C !0 / Stack BP SP Push / Stack SP SP Push / Add SP Zero BP / SetAndSave C PC PC
.defmacro Return AddImmSigned BP 2 / LoadStackOffset C BP 1 / LoadStackOffset SP BP 2 / AddImmSigned SP -2 / LoadStackOffset BP BP 3 / AddImm C 2 / Add C Zero PC
.defmacro SysReg Imm !1 !0 / System C Zero 0

Imm A 12
Imm C !func
.Call !func
Stack A SP Push
Imm A 17
.Call !func
Stack B SP Pop
Add A B A
.SysReg $f0 C

:func
Stack C SP Push
Add A A A
.Return

