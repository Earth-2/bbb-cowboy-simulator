
;   All sprites on the Atari 2600 are 8 pixels wide.  Each of these tables
;   represents one frame of sprite animation.  Each byte represents one row of
;   sprite pixels. Note that these are all "upside down"--because the counter in 
;   the main screenloop is decremented, the highest-address (last) rows of each
;   table are drawn first

;-------------------------------------------------------------------------------------
;SPRITE GRAPHIC TABLES
;-------------------------------------------------------------------------------------

    ;===DIGITS===
        
   ; .ALIGN 256

Digits                          ;Tables for the digits 0 to 9. The digits are doubled
Zero        .BYTE %00100010     ;on each frame because when the bits are actually
            .BYTE %01010101     ;loaded into the sprite registers the correct 10s and
            .BYTE %01010101     ;1s digits are filtered out through some clever bit
            .BYTE %01010101     ;arithmetic that I claim no credit for
            .BYTE %00100010
   
One         .BYTE %00010001
            .BYTE %00110011
            .BYTE %00010001
            .BYTE %00010001
            .BYTE %00010001
            
Two         .BYTE %00100010
            .BYTE %01010101
            .BYTE %00010001
            .BYTE %00100010
            .BYTE %01110111
            
Three       .BYTE %01110111
            .BYTE %00010001
            .BYTE %00100010
            .BYTE %00010001
            .BYTE %01100110
            
Four        .BYTE %00010001
            .BYTE %00110011
            .BYTE %01010101
            .BYTE %01110111
            .BYTE %00010001
            
Five        .BYTE %01110111
            .BYTE %01000100
            .BYTE %01100110
            .BYTE %00010001
            .BYTE %01100110
            
Six         .BYTE %00110011
            .BYTE %01000100
            .BYTE %01100110
            .BYTE %01010101
            .BYTE %00100010
            
Seven       .BYTE %01110111
            .BYTE %00010001
            .BYTE %00100010
            .BYTE %01000100
            .BYTE %01000100
            
Eight       .BYTE %00100010
            .BYTE %01010101
            .BYTE %00100010
            .BYTE %01010101
            .BYTE %00100010
            
Nine        .BYTE %00100010
            .BYTE %01010101
            .BYTE %00110011
            .BYTE %00010001
            .BYTE %01100110

;LETTER B==================

LetterB     .BYTE %11111111
            .BYTE %01100011
            .BYTE %01100011
            .BYTE %01101100
            .BYTE %01100011
            .BYTE %00000011
            .BYTE %11111100

B_HEIGHT    = * - LetterB

;PLAYER====================

PlrFrm1     .BYTE 0
            .BYTE %01000000    
            .BYTE %00100000    
            .BYTE %00010000    
            .BYTE %00100000    
            .BYTE %00010000    
            .BYTE %00111100    
            .BYTE %01110010    
            .BYTE %11001101    
            .BYTE %10110010    
            .BYTE %10011111    
            .BYTE %11101011
            .BYTE %01111110    
            .BYTE %00000000    
            .BYTE %01111110    
            .BYTE %01011010
            
PLR_HEIGHT  = * - PlrFrm1   ;calculate player height
            
PlrFrmJmp   .BYTE 0
            .BYTE %00000000    
            .BYTE %00000000    
            .BYTE %00000000    
            .BYTE %01100000    
            .BYTE %10110000    
            .BYTE %00111100    
            .BYTE %01110010    
            .BYTE %11001101    
            .BYTE %10110010    
            .BYTE %10011111    
            .BYTE %11101011
            .BYTE %01111110    
            .BYTE %00000000    
            .BYTE %01111110    
            .BYTE %01011010 
            
PlrFrmDead  .BYTE 0
            .BYTE %00000010    
            .BYTE %00001010    
            .BYTE %00010100    
            .BYTE %00100000    
            .BYTE %00010000    
            .BYTE %00111100    
            .BYTE %01010000    
            .BYTE %11101101    
            .BYTE %11110010    
            .BYTE %11011101    
            .BYTE %11101011
            .BYTE %01011100    
            .BYTE %00000000    
            .BYTE %01111110    
            .BYTE %01011010

;Player color table
;(Color tables are likewise upside down)

PlrColor    .BYTE $9f   ;bottom of spring
            .BYTE $9c
            .BYTE $9a
            .BYTE $98
            .BYTE $94   ;top of spring

            .BYTE $18   ;bottom of head
            .BYTE $1A
            .BYTE $1C
            .BYTE $1E
            .BYTE $1E
            .BYTE $1E
            .BYTE $1C   ;top of head
        
            .BYTE $1A 

            .BYTE $10   ;bottom of hat
            .BYTE $10
            .BYTE $10   ;top of hat

;MUNCHER-------------------

MnchrFrm1   ;.BYTE %00111000
            ;.BYTE %00011111
            ;.BYTE %01111111
            ;.BYTE %10101011
            ;.BYTE %11111111
            ;.BYTE %11011111
            ;.BYTE %10100111
            ;.BYTE %00111110
            ;.BYTE %00111000
            ;.BYTE %01000000
            ;.BYTE 0
            .BYTE %00001100
            .BYTE %00000100
            .BYTE %00110011
            .BYTE %01001110
            .BYTE %11111111
            .BYTE %01010111
            .BYTE %00000011
            .BYTE %00101111
            .BYTE %11110011
            .BYTE %01111011
            .BYTE %00011110
            .BYTE 0
        
MnchrFrm2   ;.BYTE %00000111
            ;.BYTE %00111011
            ;.BYTE %01111111
            ;.BYTE %10101011
            ;.BYTE %11111111
            ;.BYTE %11011111
            ;.BYTE %10100111
            ;.BYTE %00111110
            ;.BYTE %00111000
            ;.BYTE %01000000
            ;.BYTE 0
            .BYTE %00000110
            .BYTE %01110011
            .BYTE %00111011
            .BYTE %00111110
            .BYTE %11111111
            .BYTE %01010011
            .BYTE %00101111
            .BYTE %11111001
            .BYTE %01111101
            .BYTE %00011111
            .BYTE %00001110
            .BYTE 0
        
MnchrFrm3   ;.BYTE %00111000
            ;.BYTE %00011111
            ;.BYTE %01111111
            ;.BYTE %00000111
            ;.BYTE %10101111
            ;.BYTE %11111111
            ;.BYTE %11100111
            ;.BYTE %10110111
            ;.BYTE %00011110
            ;.BYTE %00011000
            ;.BYTE %00010000
            .BYTE %00111000
            .BYTE %00011111
            .BYTE %00001011
            .BYTE %00111111
            .BYTE %11111111
            .BYTE %01010011
            .BYTE %11111111
            .BYTE %01110011
            .BYTE %00011011
            .BYTE %00001110
            .BYTE %00001100
            .BYTE 0
        
MnchrFrm4   ;.BYTE %00000111
            ;.BYTE %00111011
            ;.BYTE %01111111
            ;.BYTE %00000111
            ;.BYTE %10101111
            ;.BYTE %11111111
            ;.BYTE %11100111
            ;.BYTE %10110111
            ;.BYTE %00011110
            ;.BYTE %00011000
            ;.BYTE %00010000
            .BYTE %00011100
            .BYTE %01111110
            .BYTE %00001110
            .BYTE %11111111
            .BYTE %01010011
            .BYTE %00101011
            .BYTE %11111101
            .BYTE %01100111
            .BYTE %00010111
            .BYTE %00011110
            .BYTE %00011100
            .BYTE 0

MUNCHER_HEIGHT  = * - MnchrFrm4

;CRAZY WORM----------------

WormFrm1    .BYTE %00001111
            .BYTE %01111001
            .BYTE %00100001
            .BYTE %11010101
            .BYTE %11100011
            .BYTE %01111111
            .BYTE %01110110
            .BYTE %00011101
            .BYTE %01110011
            .BYTE %01010000
            .BYTE %01100000
            .BYTE 0
        
WormFrm2    .BYTE %00001111
            .BYTE %00111111
            .BYTE %01000001
            .BYTE %00011101
            .BYTE %00001001
            .BYTE %10100011
            .BYTE %11111110
            .BYTE %00101111
            .BYTE %01011001
            .BYTE %00101001
            .BYTE %00000110
            .BYTE 0
            
WORM_HEIGHT     = * - WormFrm2

;BIRDOID-------------------
        
BoidFrm1    .BYTE %00011000
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %01100110
            .BYTE %11000011
            .BYTE 0
            
BoidFrm2    .BYTE 0
            .BYTE %00011000
            .BYTE %00111100
            .BYTE %11111111
            .BYTE %01100110
            .BYTE 0
            
BoidFrm3    .BYTE 0
            .BYTE 0
            .BYTE %10111101
            .BYTE %01111110
            .BYTE %00111100
            .BYTE 0

BOID_HEIGHT     = * - BoidFrm3
 
;EYE-----------------------
        
EyeFrm1     .BYTE %01000100
            .BYTE %10101010
            .BYTE %10101010
            .BYTE %11000110
            .BYTE %01111100
            .BYTE %11000110
            .BYTE %10111010
            .BYTE %10101010
            .BYTE %10111010
            .BYTE %01000100
            .BYTE %01111100
            .BYTE %01111100
            .BYTE %10010010
            .BYTE %00010000
            .BYTE %00010000
            .BYTE 0
            
EyeFrm2     .BYTE %00101000
            .BYTE %01111100
            .BYTE %01111100
            .BYTE %01101100
            .BYTE %01111100
            .BYTE %11000110
            .BYTE %10111010
            .BYTE %10101010
            .BYTE %10111010
            .BYTE %01000100
            .BYTE %01111100
            .BYTE %01111100
            .BYTE %01010100
            .BYTE %00010000
            .BYTE %00010000
            .BYTE 0
            
EYE_HEIGHT      = * - EyeFrm2

;BEHEMOTH------------------
    
BehFrm1     .BYTE %10000001
            .BYTE %00000000
            .BYTE %10000001
            .BYTE %10000001
            .BYTE %10000001
            .BYTE %01000001
            .BYTE %01100110
            .BYTE %00001100
            .BYTE %01111000
            .BYTE %11111110
            .BYTE %11111111
            .BYTE %00000011
            .BYTE %01010101
            .BYTE %01010101
            .BYTE %00000001
            .BYTE %11111001
            .BYTE %01101100
            .BYTE %01000110
            .BYTE %11011011
            .BYTE %11011111
            .BYTE %00001111
            .BYTE %01001111
            .BYTE %01100110
            .BYTE %00110100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00011100
            .BYTE %00000100
            .BYTE 0
            .BYTE 0

BEH_HEIGHT      = * - BehFrm1

BehFrm2     .BYTE %00100000
            .BYTE %00000000
            .BYTE %01000000
            .BYTE %00110000
            .BYTE %00001000
            .BYTE %01000100
            .BYTE %01000100
            .BYTE %00001100
            .BYTE %00111000
            .BYTE %11111100
            .BYTE %11111110
            .BYTE %00000111
            .BYTE %00000011
            .BYTE %00000001
            .BYTE %01010101
            .BYTE %01010101
            .BYTE %00000001
            .BYTE %11111100
            .BYTE %01001110
            .BYTE %01000111
            .BYTE %11011011
            .BYTE %00001111
            .BYTE %01001111
            .BYTE %01100110
            .BYTE %00110100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00011100
            .BYTE %00000100
            .BYTE 0
            
BehFrm3     .BYTE %01000010
            .BYTE %00000000
            .BYTE %01000010
            .BYTE %01000110
            .BYTE %00100100
            .BYTE %00010000
            .BYTE %00001000
            .BYTE %00111000
            .BYTE %01111100
            .BYTE %00000110
            .BYTE %00000011
            .BYTE %00000001
            .BYTE %00000001
            .BYTE %00000001
            .BYTE %01010101
            .BYTE %01010101
            .BYTE %00000000
            .BYTE %11111110
            .BYTE %01000111
            .BYTE %00011011
            .BYTE %10001111
            .BYTE %01000111
            .BYTE %01100110
            .BYTE %00110100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00011100
            .BYTE %00000100
            .BYTE 0
            .BYTE 0
            
BehFrm4     .BYTE %00010000
            .BYTE %00000000
            .BYTE %01010000
            .BYTE %00010000
            .BYTE %01010000
            .BYTE %01010000
            .BYTE %01001000
            .BYTE %00001000
            .BYTE %00111000
            .BYTE %11111100
            .BYTE %11111110
            .BYTE %00000111
            .BYTE %00000011
            .BYTE %00000001
            .BYTE %01010101
            .BYTE %01010101
            .BYTE %00000001
            .BYTE %11111100
            .BYTE %01001110
            .BYTE %01000111
            .BYTE %11011011
            .BYTE %00001111
            .BYTE %01001111
            .BYTE %01100110
            .BYTE %00110100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00111100
            .BYTE %00011100
            .BYTE %00000100
            .BYTE 0
         
;RADIATION DUDE------------
            
RadDudeFrm1 .BYTE %11111111
            .BYTE %11000011
            .BYTE %11111111
            .BYTE %11011011
            .BYTE %01111111
            .BYTE %01111111
            .BYTE %01111111
            .BYTE %01111110
            .BYTE %00111110
            .BYTE %00110110
            .BYTE %00110110
            .BYTE %00110110
            .BYTE %00100100
            .BYTE %00100100
            .BYTE %00100000
            .BYTE %00100000
            .BYTE %00100000
            .BYTE %00100000
            .BYTE 0

RADDUDE_HEIGHT = * - RadDudeFrm1

;BLUE BOI------------------

BBFrm1      .BYTE %00010000
            .BYTE %00010001
            .BYTE %00100010
            .BYTE %00100100
            .BYTE %00100100
            .BYTE %00100100
            .BYTE %00101000
            .BYTE %01111000
            .BYTE %10011100
            .BYTE %11101100
            .BYTE %11111100
            .BYTE %01011000
            .BYTE %01111000
            .BYTE %00111000
            .BYTE %00001000
            .BYTE %00000100
            .BYTE 0
            
BB_HEIGHT   = * - BBFrm1

BBFrm2      .BYTE %11000011
            .BYTE %00100100
            .BYTE %01000010
            .BYTE %10000001
            .BYTE %10000001
            .BYTE %01100110
            .BYTE %00111100
            .BYTE %01001110
            .BYTE %01110110
            .BYTE %01111110
            .BYTE %00101100
            .BYTE %00111100
            .BYTE %00011000
            .BYTE %00001000
            .BYTE %00010000
            .BYTE 0
            .BYTE 0
            
BBFrm3      .BYTE %11100111
            .BYTE %01100110
            .BYTE %10000001
            .BYTE %10000001
            .BYTE %01100110
            .BYTE %00111100
            .BYTE %01001110
            .BYTE %01110110
            .BYTE %01111110
            .BYTE %00101100
            .BYTE %00111100
            .BYTE %00011000
            .BYTE %00110000
            .BYTE 0
            .BYTE 0
            .BYTE 0
            .BYTE 0
            
BBFrm4      .BYTE %11100111
            .BYTE %01100110
            .BYTE %10000001
            .BYTE %01100110
            .BYTE %00111100
            .BYTE %01001110
            .BYTE %01110110
            .BYTE %01111110
            .BYTE %00101100
            .BYTE %00111100
            .BYTE %00011000
            .BYTE %00010000
            .BYTE %00100000
            .BYTE 0
            .BYTE 0
            .BYTE 0
            .BYTE 0
            
BBFrm5      .BYTE %11100111
            .BYTE %01100110
            .BYTE %11011011
            .BYTE %00111100
            .BYTE %01001110
            .BYTE %01110110
            .BYTE %01111110
            .BYTE %00101100
            .BYTE %00111100
            .BYTE %00011000
            .BYTE %00010000
            .BYTE %00010000
            .BYTE 0
            .BYTE 0
            .BYTE 0
            .BYTE 0
            .BYTE 0
            
BBFrm6      .BYTE %11100111
            .BYTE %01100100
            .BYTE %10000010
            .BYTE %01001100
            .BYTE %01111000
            .BYTE %10011100
            .BYTE %11101100
            .BYTE %11111100
            .BYTE %01011000
            .BYTE %01111000
            .BYTE %00110000
            .BYTE %00100000
            .BYTE %00010000
            .BYTE 0
            .BYTE 0
            .BYTE 0
            .BYTE 0
            
BBFrm7      .BYTE %11000011
            .BYTE %00100100
            .BYTE %01000010
            .BYTE %01000010
            .BYTE %00101100
            .BYTE %01111000
            .BYTE %10011100
            .BYTE %11101100
            .BYTE %11111100
            .BYTE %01011000
            .BYTE %01111000
            .BYTE %00110000
            .BYTE %00100000
            .BYTE %00010000
            .BYTE 0
            .BYTE 0
            .BYTE 0
            
BBFrm8      .BYTE %10000001
            .BYTE %01000010
            .BYTE %01100110
            .BYTE %01000100
            .BYTE %01000100
            .BYTE %00101000
            .BYTE %01111000
            .BYTE %10011100
            .BYTE %11101100
            .BYTE %11111100
            .BYTE %01011000
            .BYTE %01111000
            .BYTE %00110000
            .BYTE %00010000
            .BYTE %00001000
            .BYTE 0
            .BYTE 0
 
;-------------------------------------------------------------------------------------
;MONSTER FRAME TABLES
; Stores the addresses of the frames in an 8-word table, to be used to quickly get
; the animation frames for each monster
;-------------------------------------------------------------------------------------

MnchrFrames .WORD MnchrFrm1 + MUNCHER_HEIGHT
            .WORD MnchrFrm1 + MUNCHER_HEIGHT
            .WORD MnchrFrm2 + MUNCHER_HEIGHT
            .WORD MnchrFrm2 + MUNCHER_HEIGHT
            .WORD MnchrFrm3 + MUNCHER_HEIGHT
            .WORD MnchrFrm3 + MUNCHER_HEIGHT
            .WORD MnchrFrm4 + MUNCHER_HEIGHT
            .WORD MnchrFrm4 + MUNCHER_HEIGHT

WormFrames  .WORD WormFrm1 + WORM_HEIGHT
            .WORD WormFrm1 + WORM_HEIGHT
            .WORD WormFrm1 + WORM_HEIGHT
            .WORD WormFrm1 + WORM_HEIGHT
            .WORD WormFrm2 + WORM_HEIGHT
            .WORD WormFrm2 + WORM_HEIGHT
            .WORD WormFrm2 + WORM_HEIGHT
            .WORD WormFrm2 + WORM_HEIGHT

BoidFrames  .WORD BoidFrm1 + BOID_HEIGHT
            .WORD BoidFrm1 + BOID_HEIGHT
            .WORD BoidFrm2 + BOID_HEIGHT
            .WORD BoidFrm2 + BOID_HEIGHT
            .WORD BoidFrm3 + BOID_HEIGHT
            .WORD BoidFrm3 + BOID_HEIGHT
            .WORD BoidFrm2 + BOID_HEIGHT
            .WORD BoidFrm2 + BOID_HEIGHT

EyeFrames   .WORD EyeFrm1 + EYE_HEIGHT
            .WORD EyeFrm1 + EYE_HEIGHT
            .WORD EyeFrm2 + EYE_HEIGHT
            .WORD EyeFrm2 + EYE_HEIGHT
            .WORD EyeFrm1 + EYE_HEIGHT
            .WORD EyeFrm1 + EYE_HEIGHT
            .WORD EyeFrm2 + EYE_HEIGHT
            .WORD EyeFrm2 + EYE_HEIGHT

BehFrames   .WORD BehFrm1 + BEH_HEIGHT
            .WORD BehFrm1 + BEH_HEIGHT
            .WORD BehFrm2 + BEH_HEIGHT
            .WORD BehFrm2 + BEH_HEIGHT
            .WORD BehFrm3 + BEH_HEIGHT
            .WORD BehFrm3 + BEH_HEIGHT
            .WORD BehFrm4 + BEH_HEIGHT
            .WORD BehFrm4 + BEH_HEIGHT

BBFrames    .WORD BBFrm1 + BB_HEIGHT
            .WORD BBFrm2 + BB_HEIGHT
            .WORD BBFrm3 + BB_HEIGHT
            .WORD BBFrm4 + BB_HEIGHT
            .WORD BBFrm5 + BB_HEIGHT
            .WORD BBFrm6 + BB_HEIGHT
            .WORD BBFrm7 + BB_HEIGHT
            .WORD BBFrm8 + BB_HEIGHT

;-------------------------------------------------------------------------------------
;PLAYFIELD GRAPHIC TABLES
;-------------------------------------------------------------------------------------

            ;===CLOUD===
            
PF1Cloud    .BYTE %01101100    ;flofey clouds
            .BYTE %01101110
            .BYTE %11111111
            .BYTE %01111100
            .BYTE %00110000
            
            ;===MOUNTAINS===

PF0Mtn      .BYTE %11110000    ;bottom slopes of mountain
            .BYTE %11110000
            .BYTE %11100000
            .BYTE %11100000
            .BYTE %11000000
            .BYTE %11000000
            .BYTE %10000000
            .BYTE %10000000

PF2Mtn      .BYTE %11111111    ;inner crater of mountain
            .BYTE %01111111
            .BYTE %00111111
            .BYTE %00001111
            .BYTE %00000011
            .BYTE %00000011
            .BYTE %00000001
            .BYTE %00000001

