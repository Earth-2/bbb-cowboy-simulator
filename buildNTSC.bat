@ECHO OFF

SET BBB = "c:\Users\keega\Dropbox\Coding\assembly\6502\atarigames\bbb\"

ECHO %BBB%bbb.bin
dasm %BBB%bbb.asm -f3 -v5 -o"%BBB%bbb.bin" -lbbb.lst -sbbb.sym

PAUSE