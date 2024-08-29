.heap $400 $1000
.section data 0 RO
Imm M $400
ShiftLeft M M 4
StoreWord Zero M Zero
Imm A $f0
System A Zero 0
