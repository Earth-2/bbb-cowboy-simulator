
;   Each sound effect contains 3 pieces of information: channel, volume, and
;   frequency. Sound effects can be played across one of two identical audio
;   circuits.

;CHANNELS
;------------------------------------------------
; 0
; 1 = Buzzy tones
; 2 = Carries distortion 1 downward to low rumble
; 3 = Flangy wavering tones
; 4 = Pure tone
; 5 = Pure tone (same as 4)
; 6 = Between pure tone and buzzy tone
; 7 = Reedy, brighter tones
; 8 = White noise
; 9 = Reedy, brighter tones (same as 7)
; A = Between pure tone and buzzy tone (same as 6)
; B
; C = Very low pure tone
; D = Very low pure tone (same as C)
; E = Low-to-rumble electronic tones (square-wavey)
; F = Low-to-rumble electronic tones (noisy)
;
; Frequencies can range from 0 to 31.

;-------------------------------------------------------------------------------------
;SOUND EFFECTS TABLES
;-------------------------------------------------------------------------------------
;Frequency tables for sound effects. These (and the channel/volume tables) are loaded
; in backwards, like the graphics tables.  Note that the frequency tables don't need 
; to start with a 0, but it's good to do so for consistency's sake (they will all
; start with a 0 in their corresponding C/V tables)

SFXF:
        ;hopping
        ;.byte 0,2,3,4,5,6
        ;.byte 0,6,7,8,9,10
        .BYTE 0,2,7,4,9,6
        
        ;extra life
        .BYTE 0,3,8,4,12,5,16,7,22,8,28
        .BYTE 7,22,6,16,5,12,4,8,3
        ;.BYTE 0,4,9,16,4
        
        ;dead
        .BYTE 0,14,14,14,14,14,13,12,11, 0,0    ;oh!
        .BYTE 5,6,7,8,9,10,11,12,  0,0,0,0      ;uh-
        .BYTE  31,5,31,10,31,20                 ;thud
        
        ;jump
        .BYTE 0,1,1,1,3,3,3,5,5,5,1
        
        ;birdoid appears
        .BYTE 0,11,11,11,11,14,14,12,12
        
        ;muncher appears
        .BYTE 0,31,31,31,31,31,31,31,27,23,19,15,11,8,6,8,13,15,16,18,19
        
        ;blue boi hops
        .BYTE 0,14,15,16,17,18,22,22,22,22
        
        ;worm rises
        .BYTE 0,7,6,8,7,9,8,10,9,11,10,12,11,13,12,14,13,15,14,16,15,17,16,18,17,19,18
        
        ;behemoth stomps
        .BYTE 0,31,31,31,31,31,31,31,27,23,19,15
        
        ;behemoth rises out of ground
        .BYTE 0,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,22

;Make sure frequency table isn't too big
SFXF_COUNT = * - SFXF
    .IF SFXF_COUNT > 256
        .ECHO "Warning: table SFXF is too large"
    .ENDIF

;-------------------------------------------------------------------------------------
;Channel/volume tables for sound effects. The zero indicates to the SFXupdate routine
; that the sound effect is over and no further update is necessary (plus zero is a 
; silent channel, so no sound will be played.)

SFXCV:
        .BYTE 0,$62,$41,$62,$41,$62 ;hopping
SFX_HOP = * - SFXCV-1

        .BYTE 0,$CA,$CA,$CA,$CA,$CA,$CA,$CA,$CA,$CA,$CA
SFX_LIFE = * - SFXCV-1        
        .BYTE $CA,$CA,$CA,$CA,$CA,$CA,$CA,$CA,$CA
        ;.BYTE 0,$CA,$CA,$CA,$CA
SFX_GOTEM = * - SFXCV-1

        .BYTE 0,$18,$18,$18,$18,$18,$18,$18,$18, $B0,$B0        ;oh!
        .BYTE $18,$18,$18,$18,$18,$18,$18,$18, $B0,$B0,$B0,$B0  ;uh-
        .BYTE $8C,$8C,$8C,$8C,$8C,$8C                           ;thud
SFX_OUCH = * - SFXCV-1

        .BYTE 0,$63,$63,$63,$63,$65,$65,$67,$67,$67,$67
SFX_JUMP = * - SFXCV-1

        .BYTE 0,$41,$41,$41,$41,$41,$41,$41,$42
SFX_BOID = * - SFXCV-1

        .BYTE 0,$C5,$C0,$C7,$C0,$C9,$C0,$C9,$C9,$C7,$C7,$C9,$C9,$C7,$C7,$C5,$C5,$C3,$C3,$C3,$C3
SFX_MCHR = * - SFXCV-1

        .BYTE 0,$46,$46,$46,$46,$46,$46,$46,$46,$46
SFX_BB   = * - SFXCV-1

        .BYTE 0,$c2,$c2,$c3,$c3,$c4,$c4,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5,$c5
SFX_WORM = * - SFXCV-1

        .BYTE 0,$75,$70,$77,$70,$79,$70,$79,$79,$7A,$7F,$7F
SFX_BEHSTMP = * - SFXCV-1

        .BYTE 0,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A,$7A
SFX_BEHRIS = * - SFXCV-1

;Run a couple more size checks, to ensure channel/volume table isn't too big and
;C/V table is same size as frequency table
SFXCV_COUNT = * - SFXCV
    .IF SFXCV_COUNT > 256
        .ECHO "Warning: table SFXCV is too large"
    .ENDIF
    .IF SFXF_COUNT != SFXCV_COUNT
        .ECHO "Warning: SFX frequency and channel/volume tables are different sizes"
    .ENDIF

;-------------------------------------------------------------------------------------
;SOUND EFFECTS SUBROUTINES
;-------------------------------------------------------------------------------------
;SFXoff: silence all sound registers by setting to 0

SFXoff:     .SUBROUTINE

            LDX #0
            STX SFXleft     ;set audio indices to 0
            STX SFXright
            STX AUDV0       ;set volumes to 0
            STX AUDV1
            STX AUDC0       ;set channels to 0 (silent channel)
            STX AUDC1
            
            RTS
            
;-------------------------------------------------------------------------------------
;SFXtrigger: start playing sound effect, after checking that one of the audio circuits
; is free first

SFXtrigger: .SUBROUTINE

            LDX SFXleft     ;index of left channel sound
            LDA SFXCV,x     ;load current sound to A
            BNE .LNotFree   ;if not 0, then left is busy
            STY SFXleft     ;otherwise store Y (previously defined as
            RTS             ;our sound to play) as the channel index
            
.LNotFree:  LDX SFXright    ;same shit but for the right channel
            LDA SFXCV,x
            BNE .RNotFree
            STY SFXright
            RTS
            
        ;if both are busy, prioritize the lower index...
            
.RNotFree:  CPY SFXleft     
            BCC .LNotLower
            STY SFXleft
            RTS
            
.LNotLower: CPY SFXright
            BCC .RNotLower
            STY SFXright
            
.RNotLower: RTS

;-------------------------------------------------------------------------------------
;SFXUpdate: if sound effect is currently playing, updates to next "note" of sound
; effects tables

SFXUpdate:  .SUBROUTINE

            LDX SFXleft
            LDA SFXF,x
            STA AUDF0       ;frequency
            LDA SFXCV,x
            STA AUDV0       ;volume in lower nybble
            LSR             ;channel in upper nybble, so necessary to shift right
            LSR             ;four
            LSR             ;times
            LSR             ;:)
            STA AUDC0       ;channel
            BEQ .skipLDec   ;if sound is finished, no need to keep decrementing
            DEC SFXleft     ;otherwise, decrement to next part of audio
            
.skipLDec:  LDX SFXright
            LDA SFXF,x
            STA AUDF1
            LDA SFXCV,x
            STA AUDV1
            LSR
            LSR
            LSR
            LSR
            STA AUDC1
            BEQ .skipRDec
            DEC SFXright

.skipRDec:  RTS

