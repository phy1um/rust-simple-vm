Imm A 400
Imm B $123
ShiftLeft B B 4
Add B B BP
AddImm B 4
Imm M $400
ShiftLeft M M 4
:loop
Imm C !end
Test A Zero Lte
AddIf PC C 0
Imm C 1
Sub A C A
Store M Zero B
AddImm M 2
Imm C 16
ShiftLeft C C 4
Add B BP B
Imm PC !loop
:end
Imm A 240
System A A 0
