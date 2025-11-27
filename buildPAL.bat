@ECHO OFF

SET BBB = "c:\Users\keega\Dropbox\Coding\assembly\6502\atarigames\bbb\"

ECHO %BBB%bbb_pal.bin
dasm %BBB%bbb_pal.asm -f3 -v5 -o"%BBB%bbb_pal.bin" -lbbb_pal.lst -sbbb_pal.sym

PAUSE