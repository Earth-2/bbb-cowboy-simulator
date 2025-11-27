
;   PICTURE.ASM holds two subroutines, for drawing the main gameplay frame and the 
;   title screen.

MAIN_HEIGHT     = 90    ;192 - 3 = 189 - 5 = 184 - 8 = 176 - 8 = 168
                        ;    - 30 = 138 / 2 = 69 [beavis and butthead]
                        ;    - 1 = 68, there's another WSYNC I forgot about
                                
;-------------------------------------------------------------------------------------
;Picture: The kernel, which draws the 192 lines visible on the screen during gameplay.
;-------------------------------------------------------------------------------------

Picture:    .SUBROUTINE

    ;Start with sky area, which is mostly decorative although I will try to put the
    ; lives counter up here
            LDA #0
            STA CTRLPF

            STA WSYNC           ;1 line of clear sky

            LDY #4              ;playfield AND scanline index

            LDA cloudCol
            CLC
            ADC flash

            STA COLUPF          ;load cloud color
            LDA skyCol+1
            STA COLUBK          ;change sky color
                
.Sky:       LDA PF1Cloud,y      ;draw cloud in sky
            STA PF1
            STA WSYNC
            DEY
            BNE .Sky

            LDA skyCol+2
            STA COLUBK          ;change sky color
            LDA cloudCol+1
            STA COLUPF          ;bottom line of cloud a different color
            LDA PF1Cloud,y
            STA PF1

            STA WSYNC
            
            LDA skyCol+3
            STA COLUBK          ;change sky color

            STY PF1             ;PF0 is blank in top 8 rows  
            LDA mtnCol
            CLC
            ADC flash
            STA COLUPF

            LDA #%00000001
            STA temp
            ORA cactusWidth
            STA CTRLPF          ;make playfield reflect, set cactus size

            
            
            LDX #8              ;8 rows of top mountain
            STA WSYNC           ;line of clear sky
            
            LDA skyCol+4
            STA COLUBK          ;change sky color...

.MtnTop:   
            
            LDA PF2Mtn,x        ;increment through mountain tables
            STA PF2
            LDA temp
            STA PF1 
            ASL
            ORA #%00000001
            STA temp
            STA WSYNC              
            DEX
            BNE .MtnTop
            
            ;New registers for bottom of mountain
            LDA #%11111111  ;PF1 AND PF2 registers are filled in
            STA PF1
            STA PF2

            LDA skyCol+5
            STA COLUBK          ;keep changin' that sky color
            LDX #7              ;8 rows of mountain bottom
            LDA livesGfx
            STA GRP0            
            LDA livesGfx+1
            STA GRP1            



.MtnBottom: LDA PF0Mtn,x        ;increment through "mountain" table
            STA PF0
            STA WSYNC
            DEX
            BNE .MtnBottom
            
            STX GRP0            ;X is 0 here
            STX GRP1
            STX WSYNC
            
            LDA bakCol
            CLC
            ADC flash
            STA COLUBK          ;Mars background color
            STX PF0             ;X is still 0
            STX PF1
            STX PF2             ;disable all playfield graphics

            ;LDA cactusCol
            ;STA COLUPF          ;cactus color (ball color always = playfield color)
            
    ;Putting object movements here allows us to also put sprites in the sky

            LDA mnstrRef
            STA REFP1
           
            
            LDA cactusXpos
            LDX #4
            JSR XPosObject
            
            LDA mnstrXpos
            LDX #1              ;1 = PL1 object (monster)
            JSR XPosObject      ;position monster horizontally
            
            LDA #PLRXPOS
            DEX                 ;X is now 0 (player)
            JSR XPosObject      ;Position player on X axis

            LDA mnstrSize
            STA NUSIZ1          ;set height of monsters
            LDA mnstrCol
            STA COLUP1
            
            STA WSYNC
            STA HMOVE

            LDY #MAIN_HEIGHT    ;y indexes scanline #, AND also which
                                ;line of sprite(s) to show

;ALL SPRITE DRAWING SHOULD OCCUR WITHIN THIS LOOP
;-------------------------------------------------------------------------------------
.Main:      LDA (plrClrPtr),y   ;load current line of player color table
            STA COLUP0          ;store to player color register
            LDA cactusCol
            STA COLUPF
            LDA plrHeight
            SEC
            SBC #1
            DEC plrDraw         ;count down until at top of player
            CMP plrDraw         ;performs plrHeight-1 - plrDraw
            BCS .DrawPlr        ;is plrHeight-1 >= plrDraw?
            LDA #0              ;GRP0 will be set to 0
            .BYTE $2C           ;fancy way to skip only next instruction
                
.DrawPlr:   LDA (plrPtr),y      ;load in player graphic table

;LINE 1 of 2 LINE KERNEL--------

.Line1:     STA WSYNC
            STA GRP0            ;load in new line of player graphics  
            
            CPY cactusHeight    ;is y = cactusHeight?
            BNE .NoCactus       ;if so, enable cactus
            LDA #2
            STA ENABL           
.NoCactus:  
            LDA cactusCol+1
            STA COLUPF
            LDA #MONSTER
            BIT objFlags 
            BEQ .NoMnstr        ;if there's no monsters then don't bother

            ;DEC mnstrDraw
            LDA mnstrDraw
            DEC mnstrDraw
            SEC
            SBC mnstrHeight
            BCC .DrawMnstr
            BMI .Below
.NoMnstr:   LDA #0
            .BYTE $2C           ;skip ahead to WSYNC
.Below:     LDA mnstrUnder
            .BYTE $2C           ;skip ahead to WSYNC           
.DrawMnstr: LDA (mnstrPtr),y

;LINE 2 OF 2 LINE KERNEL--------

.Line2:     STA WSYNC
            STA GRP1            ;load in new line of monster graphics

            DEY                 ;decrement dual-scanline counter
            BPL .Main           ;if y>0, do next scanline pair
;-------------------------------------------------------------------------------------

    ; THE UNDERGROUND

            LDA #$44
            STA COLUBK          ;ground (background) color
            STX ENABL           ;disable ball (cactus). X is still 0 here
            STX RESP0       ;place Doom Meter at this X-position
            STX GRP0        ;must be 0'd before the vertical delay! Note that 
                            ;the vertical delay register actually contains two bytes,
                            ;which it seems represent two separate sprites depending
                            ;on whether VDELPX is enabled or not
            STX GRP1

            STX NUSIZ1
            STX REFP1           ;and do not reflect
   
            STX VDELP0          ;disable for Doom Meter
            
            LDA #DOOM_COL
            STA COLUP0
            LDA doomMeter
            STA GRP0            ;the doom meter!
            
            LDA #$64
            STA groundCol

            LDA #7
            STA NUSIZ0      ;size of Doom Meter
            
            LDY #3

.Ground:    STA WSYNC
            DEC groundCol
            LDA groundCol

            STA COLUBK
            DEY
            BNE .Ground
            
            STY NUSIZ0
            STY GRP0        ;disable Doom Meter graphics
            
            STA WSYNC
            
            LDA #CTR_COL    ;Display color
            STA COLUP0      ;seconds
            STA COLUP1      ;minutes

            LDX #11         ;2
.ctrdelay:  DEX             ;2
            BNE .ctrdelay   ;3/2 for a delay of 56 cycles
            
            STA RESP1       ;set position of minute counter
            STA RESP0       ;seconds come right after
            
            LDX #5          ;set counter for draw loop
            STX VDELP0      ;re-enable vertical delay
            STA WSYNC       ;WSYNC right before .Draw to keep everything aligned

.DrawCtr: 
            JSR DrawDigs
            DEX   
            BNE .DrawCtr    ;keep looping until x is 0

            STA WSYNC
            
            STX GRP0        ;set seconds digit graphics to 0
            STX GRP1        ;set minutes digit graphics to 0
            
            LDY #16
.UGround:   STA WSYNC       ;last visible rows of screen
            DEY
            BNE .UGround

            RTS
            
;-------------------------------------------------------------------------------------
;Title: Basically just a separate kernel for displaying the title screen, using 
; playfield registers 0 and 2 to draw our cowboy and player 0 to draw title
; initials     
;-------------------------------------------------------------------------------------

TTLMNSIZE   = PLR_HEIGHT*8          ;height of area of screen where title is drawn
TTLBRDSIZE  = ((PIC_HEIGHT-TTLMNSIZE)/2)   ;height of top and bottom border areas

Title:      .SUBROUTINE

            ;LDA #$26
            STX COLUBK              ;black background (blackground?)
            INC titleBaseCol        ;need to start with a different color each frame
            LDA titleBaseCol
            STA titleCol            ;...and then titleCol will cycle it each scanline

            LDX #TTLBRDSIZE
.TopBord:   STA WSYNC
            DEX
            BNE .TopBord
            
            LDX #PLR_HEIGHT
.Main:      
            LDY #8
.Line:      STA WSYNC
            INC titleCol
            LDA titleCol
            STA COLUP0
            LSR
            STA COLUPF
            LDA PlrFrm1,x
            ASL
            ASL
            ASL
            ASL
            STA PF2
            LDA PlrFrm1,x
            STA PF0
            SLEEP 13
            
            LDA #0
            STA PF0
            STA PF2
            STA VDELP0
            DEY
            BNE .Line
            DEX
            BNE .Main
            
            STA WSYNC
            LDA #%00000110  ;3 Bs
            STA NUSIZ0
            LDY #5
            NOP
.delay:     DEY
            BNE .delay
            STA RESP0

            LDY #10
.BtmBord1:  STA WSYNC
            DEY
            BNE .BtmBord1
            
            LDY #B_HEIGHT
.Letters:   
            LDA LetterB,y
            STA WSYNC
            STA GRP0

            STX WSYNC
            STX GRP0
            
            DEY
            BPL .Letters
            
            LDY #36
.BtmBord2:  STA WSYNC
            DEY
            BNE .BtmBord2
                        
            RTS                       

;-------------------------------------------------------------------------------------
;Endscreen: Another separate kernel for displaying the end screen, which will just
; display the final score.  Could I find some way to incorporate this into the above
; title screen kernel? Probably, but EHHHHH
;-------------------------------------------------------------------------------------

DIGHEIGHT   = 5*8
ENDBRDSIZE  = ((PIC_HEIGHT-DIGHEIGHT)/2)-1

Endscreen:   .SUBROUTINE

            LDA #$86            ;set the background color 
            STA COLUBK

            LDX #ENDBRDSIZE+7
.TopBord:   STA WSYNC
            DEX
            BNE .TopBord
            
            LDA #CTR_COL    ;Display color
            STA COLUP0      ;seconds
            STA COLUP1      ;minutes
            
            LDA #%00100111  ;make the counter and colon graphics extra wide
            STA NUSIZ0
            STA NUSIZ1

            LDX #2          ;2
.delay1:    DEX             ;2
            BNE .delay1     ;3/2 for a delay of 11 cycles
            STA RESP1       ;set position of minute counter
            NOP
            NOP
            NOP
            NOP
            NOP
            STA RESM1
            STA RESP0       ;seconds come right after
            
            LDX #5          ;set counter for draw loop
            STA WSYNC       ;WSYNC right before .Draw to keep everything aligned
            LDA #0

.Draw:      
            STA ENAM1
            
            JSR DrawDigs

            LDY #(DIGHEIGHT/5-1)
            TXA
            AND #%00000001
            ASL
            
.Wsyncs     STA WSYNC
            DEY
            BNE .Wsyncs
            
            DEX   
            BNE .Draw       ;keep looping until x is 0
            
            STX GRP0        ;set seconds digit graphics to 0
            STX GRP1        ;set minutes digit graphics to 0           
            
            LDX #ENDBRDSIZE+7
.BtmBord:   STA WSYNC
            DEX
            BNE .BtmBord
            
            LDA delay       ;wait a tic before checking button
            BNE .Return
            
            LDA INPT4       ;button resume
            BMI .Return     
            
            PLA
            PLA             ;pop return address off stack, to avoid overflow
            JMP NewGame     ;just jump the fuck out of this subroutine
            
.Return:    RTS                       

;-------------------------------------------------------------------------------------
;DrawDigs: Draws a single scanline of counter graphics
;-------------------------------------------------------------------------------------
     
DrawDigs:   .SUBROUTINE

            LDY tensDigit   ;get seconds' 10's digit offset
            LDA Digits,y    ;look up offset in table
            AND #$F0        ;isolate 10's digit
            STA secondsGfx  ;store a line of graphics table
            LDY onesDigit   ;now do this for the 1's place...
            LDA Digits,y
            AND #$0F
            ORA secondsGfx  ;adds 1's digit in without disrupting 10's digit
            STA secondsGfx
            
            INC onesDigit   ;for seconds...
            INC tensDigit   ;(do tens place too)
            STA GRP0
            
            LDY tensDigit+1 ;get minutes' 10's digit offset
            LDA Digits,y    ;look up offset in table
            AND #$F0        ;isolate 10's digit
            STA minutesGfx  ;store a line of graphics table
            LDY onesDigit+1 ;now do this for the 1's place...
            LDA Digits,y
            AND #$0F
            ORA minutesGfx  ;adds 1's digit in without disrupting 10's digit
            STA minutesGfx

            INC onesDigit+1 ;and for minutes
            INC tensDigit+1
      
            STA GRP1
            
            RTS
            
;-------------------------------------------------------------------------------------
;CalcTimer
;-------------------------------------------------------------------------------------

CalcTimer:  .SUBROUTINE 
                     
            LDA gameState
            ROR
            BCC .Stopped    ;stop if inactive
            INC frames      ;otherwise, here is where we increment the frame counter
.Stopped:   LDA frames      ;check if we're up to one second of frames
            CMP #FPS
            BCC NoTimeCalc  ;if not, skip to drawing timer
            LDA #0
            STA frames      ;otherwise, set to 0        
            
            SED             ;switch to decimal mode :O 

            LDA seconds
            CLC
            ADC #1          ;add one second to timer
            STA seconds
            
            JSR LevelUp     ;check every second if we're ready to level up yet
.NoLevel:            
            LDA seconds
            CMP #$60        ;are we up to 60 seconds?
            BCC NoTimeCalc  ;if not, skip to drawing timer
            
            LDA #0
            STA seconds     ;reset seconds to 0
            LDA minutes
            CLC
            ADC #1          ;add 1 minute
            STA minutes

    ;FETCH OFFSETS FOR TIMER GRAPHICS

NoTimeCalc: CLD             ;make sure we're no longer in decimal mode            
            LDX #1          ;decrement through mins, secs respectively
                            ;   seconds+0 = seconds (obvi)
                            ;   seconds+1 = minutes
TimerGrafs: LDA seconds,x
            AND #$0F        ;isolate 1's place
            STA temp
            ASL             ; X*2
            ASL             ;  *2
            ADC temp        ;  +X = X*5
            STA onesDigit,x ;where to find 1's digit
            LDA seconds,x
            AND #$F0        ;isolate 10's place
            LSR             ; X/2
            LSR             ;  /2
            STA temp
            LSR             ;  /2
            LSR             ;  /2
            ADC temp        ;  +(X/4) = X/16 + X/4 = X/16 + 4X/16 = 5*(X/16)
            STA tensDigit,x ;where to find 10's digit
            DEX
            BPL TimerGrafs

            RTS
