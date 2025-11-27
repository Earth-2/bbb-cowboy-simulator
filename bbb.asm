
                    ;===    BRAQING BADD BALLOON     ===;
                    ;      BUSTED.BRANDED.BEASTED       ;
                    ;       BAKED BEAN BOUNCYBOI        ;
                    ;     TRIPLE-B COWBOY SIMULATOR     ;
                    ;=== Keegan Covey, (C) 2017-2020 ===;
                        
    ; Endless-running (jumping?) game where you run a gauntlet of enemies and
    ; obstacles through the Martian desert.

    ;TV FORMAT : NTSC

    ;====OBJECTS===============================CONTROLS==========================;
    ; Player, seconds:           P0       ; Button:     Jump
    ; Monsters, minutes:         P1       ; Right:      Speed up
    ; Cactus/wall:               BL       ; Left:       Slow down
    ;                                     ; Up:         Higher jump
    ; Colon at endscreen:        M1       ; Down:       Lower jump
    ;============================================================================;

    .PROCESSOR  6502        ;well, technically 6507 ;3
    .INCLUDE    "vcs.h"     ;for TIA and RIOT registers' locations in memory
    .INCLUDE    "macro.h"   ;for some Atari-related macros
    
NO_ILLEGAL_OPCODES  = 1     ;Not 100% necessary but will keep DASM happy
    
    ;===CONSTANTS===
    ;These aren't actually stored in ROM or RAM, but
    ;are named for programmer's convenience/easy
    ;editing and debugging
                
FPS             = 60            ;frames per second (used in calculating game
                                ; timer). On an NTSC television, this will ALWAYS
                                ; be equal to 60                                
PIC_HEIGHT      = 192

LEVEL_TIME      = $15           ;time (seconds) between levelings up           
                                ; Must be in hex, since it's calculated as a BCD

STREAK          = 10            ;How many monsters must be dodged for an extra life?

;Player flags--------------

PLRXPOS         = 33            ;player's X position onscreen

JUMP_ARC        = 16            ;total distance of standard jump arc
JUMP_DELAY      = 2             ;factor by which jumpCount is delayed
HOP_HEIGHT      = 3
DEFHOPDELAY     = 2             ;default hopping delay (at slow/normal speed)

SPEED           = 4             ;the smaller this is, the more wack speed mode will be

;Game state flags----------
;stored in gameState

ACTIVE          = %00000001     ;there is an active game on
DEAD            = %00000010     ;the player is dead
TITLE           = %00000100     ;set to title screen
SPEED_M         = %01000000     ;Speed Mode active (this must be bit 6, as we will
                                ;be testing it using an extremely arcane and weird
                                ;property of BIT and the V flag)
;Object flags--------------
;stored in objFlags
            
CACTUS          = %00000001     ;cactus flag
MONSTER         = %00000010     ;monster flag (note all ground-level monsters share
                                ; the same flag since we can only show one at a time)
DISABLE_MNSTR   = %11111101

;Monster flags-------------
;stored in mnstrFlags
                      
MUNCHER         = %10000000
WORM            = %01000000
BIRDOID         = %00100000
EYE             = %00010000
BEHEMOTH        = %00001000
RADDUDE         = %00000100
BLUEBOI         = %00000010

ODDS            = 9

BEH_START       = 130           ;behemoth's starting X position
BBHOP_HEIGHT    = 9

;Colors of things----------

CACTUS_BASECOL  = $C0
ROCK_BASECOL    = $09
MNCHR_COL       = $6A
WORM_COL        = $58
BOID_COL        = $0E
EYE_COL         = $AE
BEH_COL         = $BE
RADDUDE_COL     = $FE
BLUEBOI_COL     = $8E

CTR_COL         = $0D           ;color of counter object
LIVES_COL       = $1C           ;lives counter
DOOM_COL        = $18           ;Doom Meter

;=====================================================================================
;VARIABLES - Allocate some symbolic locations in RAM
;=====================================================================================

     SEG.U vars     ;Begin variables segment: nothing is actually written to RAM at
    .ORG $80        ; this point, only that space is named and allocated
                    ;NOTE ALSO that RAM is zero-page, making accessing it faster :3
                    ;The space from $00-7F is reserved for the TIA registers, and
                    ; declared in vcs.h
                    
gameState       .DS 1
    
frames          .DS 1   ;standard binary, counts frames up to 1 second

seconds         .DS 1   ;BCD for seconds played
minutes         .DS 1   ;BCD for minutes played

onesDigit       .DS 2   ;Where to find selected digit at
tensDigit       .DS 2   ;+0 = seconds
                        ;+1 = minutes
                        ;+2 = hours, not currently implemented

level           .DS 1   ;current level (updates by time interval)
lives           .DS 1
streak          .DS 1   ;Counts number of monsters safely dodged

secondsGfx      .DS 1
minutesGfx      .DS 1
livesGfx        .DS 2   ;For the lives counters (for up to 8 lives)

skyCol          .DS 6   ;holds sky color table per level
cloudCol        .DS 2   ;holds cloud color table per level
mtnCol          .DS 1   ;color of mountains per level
bakCol          .DS 1   ;color of background per level
groundCol       .DS 1   ;color of ground area per level

plrYpos         .DS 1   ;player's Y (from bottom of screen to top of player)
plrHeight       .DS 1
plrDraw         .DS 1   ;measures from top against player height to determine when to
                        ; start drawing player
plrPtr          .DS 2   ;pointer to player graphic table
plrClrPtr       .DS 2   ;pointer to player color table

mnstrXpos       .DS 1   ;monster's X (from left)
mnstrYpos       .DS 1   ;monster's Y (from bottom of screen to top of monster)
mnstrHeight     .DS 1   ;height of monster sprite
mnstrDraw       .DS 1   ;uses same basic mechanism as plrDraw
mnstrPtr        .DS 2   ;pointer to monster graphic table
mnstrUnder      .DS 1   ;used for "worm neck" effect
mnstrFrame      .DS 1   ;counter for monster animation
mnstrSpeed      .DS 1   ;how much faster monster goes than cacti
mnstrSize       .DS 1   ;used with NUSIZ1 to maintain monster size
mnstrCol        .DS 1   ;color of monster
mnstrRef        .DS 1   ;does monster reflect?
mnstrAnimHi     .DS 8   ;8 monster animation frames (higher bytes)
mnstrAnimLo     .DS 8   ;8 monster animation frames (lower bytes)

mnstrOdds       .DS 8   ;+0 = odds of ANY monster appearing
                        ;+1 = odds of rad dude appearing
                        ;+2 = odds of birdoid appearing
                        ;+3 = odds of muncher appearing
                        ;+4 = odds of crazy worm appearing
                        ;+5 = odds of eye appearing
                        ;+6 = odds of blue boi appearing
                        ;+7 = odds of behemoth appearing
cactusOdds      .DS 1   ;odds of a cactus appearing (MUST FOLLOW mnstrOdds
                        ; in RAM!!!)

animDelay       .DS 1   ;higher number means slower animation

;These values will never occur simultaneously: as such they share the same
;address

multMnchr               ;multiplication factor for munchers                        
bbHop                   ;Blue boi's hop factor
titleCol                ;color (per scanline) of title graphic
wormTmr                 ;calculates when worm will start rising
boidTmr                 ;delay in birdoid motion
                .DS 1 

;Same with these, although they occur simultaneous to some of the ones above

wormDir
bbHopHeight             ;so hop factor can be randomized with each jump      
boidInitYpos            ;Initial Y position of birdoid
titleBaseCol            ;color of top scanline of title (for the "rolling" effect)
                .DS 1

jump            .DS 1   ;used for calculating jumping
jumpHeight      .DS 1   ;height that player actually jumps
jumpCount       .DS 1   ;counter for slowing jump cycle

hop             .DS 1   ;used for calculating hopping
hopCount        .DS 1   ;counter for slowing hop cycle
hopDelay        .DS 1   ;factor by which hop is slowed

objSpeed        .DS 1   ;speed of oncoming objects

objFlags        .DS 1   ;Each bit will note if object is onscreen or not--note
                        ; constants above for which bit flags which object
                        
mnstrFlags      .DS 1   ;like objFlags, but specifically for which type of monster is
                        ;onscreen

cactusXpos      .DS 1   ;cactus' X position (from left)
cactusHeight    .DS 1   ;height of cactus (also line where cactus/ball object will be
                        ; activated)
cactusWidth     .DS 1
cactusCol       .DS 2   ;stores randomly generated cactus color

rand8           .DS 1   ;16 bits of random number generation
rand16          .DS 1   ;upper bits of random # generation...not necessary, but 
                        ; greatly improves randomization effect

delay           .DS 1   ;delay counter for certain actions

doomCtr         .DS 1   ;countdown to death in speed mode
doomMeter       .DS 1   ;countdown to death as displayed onscreen.  It will also
                        ;be used as part of counter mechanism: each time doomCtr
                        ; <= 0, a pixel will be knocked off

flash           .DS 1   ;timer used in screen flash effect

titleBakCol
temp            .DS 1   ;temporary variable for when one is needed
tempPtr         .DS 2   ;temporary pointer for when one is needed

SFXleft         .DS 1   ;left audio channel index
SFXright        .DS 1   ;right audio channel index

    .ECHO "----",[$100 - *]d , "/ 128 bytes of RAM remaining"   ;Assembler will help
                                                                ; me keep track of my
                                                                ; precious RAM

;=====================================================================================
;ROM SEGMENT - contains game code and various tables for graphics and stuff
;=====================================================================================
;INITIALIZE MEMORY - Clear RAM, including stack, and all registers (c. Andrew Davie)
;-------------------------------------------------------------------------------------

    .SEG code           ;begin code segment (what will be written to ROM)
    .ORG $F000          ;8K ROM starts at this address

RESET:      CLD         ;Making sure decimal mode flag is cleared

            LDX #0      ;X = 0
            TXA         ;A = 0
            TAY         ;Y = 0

Clear:      DEX         ;decrement X around until it's 0 again
            TXS         ;set stack pointer to X
            PHA         ;push 0 to stack
            BNE Clear   ;keep decrementing stack through entirety of memory until
                        ; everything including X is 0, and pointer is back at $FF

;-------------------------------------------------------------------------------------
;Once-only initialization
;-------------------------------------------------------------------------------------
        
    ;The game resets from this point if lives = 0. Note that gameState is not redefined
    ; here; this means we can tell the game whether to show the tile screen (only at 
    ; power on) or the endscreen (at every game over)

GameOver:   STA CXCLR       ;clear all collision registers
                          
            LDA #TITLE
            ORA gameState
            STA gameState       ;by doing this, can have both TITLE and DEAD flags
                                ;set, signifying time to show Game Over screen

            LDA #HOP_HEIGHT
            STA hop             ;initialize hop distance to default

            LDA INTIM           ;seed random number generator. Must be done outside
            STA rand8           ; of gameplay loop, or else this will be the same
            EOR #$FF            ; series of numbers every frame
            STA rand16
            
            JSR FetchOdds       ;set level for first 30 seconds of game
            JSR GetLevCols      ;set colors for first minute of game
            JSR CalcTimer

;=====================================================================================
;START OF FRAME - total frame must be 262 scanlines total
;=====================================================================================
;Vertical sync - 3 scanlines to signal TV to start a new frame
;-------------------------------------------------------------------------------------

FRAME:      LDA #2
            STA WSYNC   ;WSYNC before enabling VSYNC
            STA VSYNC   ;start vertical sync            
            STA WSYNC   ; 1
            STA WSYNC   ; 2
            LSR         ;set bit 1 to 0, for ending VSYNC
            STA WSYNC   ; 3 (right before VSYNC shutoff)
            STA VSYNC   ;end vertical sync
            
;-------------------------------------------------------------------------------------
;Vertical Blank - 37 scanlines to wait while TV sets laser to top of screen
;-------------------------------------------------------------------------------------
            
            LDA #44
            STA TIM64T  ;Timer for vertical blank
                        ;EQUATION: scanlines = (timer * 64) / 76 
                        
            LDA #2
            STA objSpeed    ;default speed of oncoming objects (this must be 
                            ; reset every frame or the speed control won't work)
                            ;Speed increases to 4, or decreases to 1, if sticc
                            ; is held left or right
            LDA delay
            BEQ CheckSw
            JMP NotReset

CheckSw:    LDA SWCHB       ;load console switches
            ROR
            BCC NewGame     ;Branch if reset is thrown
            JMP NotReset
         
    ;Only when reset switch is thrown
            
NewGame:    LDA #ACTIVE
            STA gameState   ;ACTIVE is only game state flag at start, by default
            
            LDA SWCHB
            ROL
            BCC NoSpeedM    ;check P1 difficulty switch; branch if 0 (easy)
            LDA #SPEED_M
            ORA gameState
            STA gameState   ;turn on speed mode

NoSpeedM:   LDA #3          ;reset to 3 lives at game start       
            STA lives

            ;LDA #STREAK    ;By commenting this out, I save 4 bytes and also get a
            ;STA streak     ;free "extra life" sound at power on which is kinda neat
            
            LDA #30         ;frame delay before pressing reset switch again
            STA delay       ;no enemies can appear during delay period, and
                            ;player can't control either
                            
            LDA #0          ;reset level, frame/timer counts on new game
            STA level
            STA frames
            STA seconds
            STA minutes
            STA doomMeter   ;Doom Meter is 0 if SPEED_M is 0
            STA doomCtr
            DEC doomCtr     ;doomCtr inits to $FF
            
            BIT gameState
            BVC FirstOdds
            DEC doomMeter   ;Doom Meter is also $FF if SPEED_M is 1   

FirstOdds:  LDX #ODDS-1     ;zero odds at new game
ResetOdds:  STA mnstrOdds,X
            DEX
            BPL ResetOdds

            JSR GetLevCols

        ;A less flexible but more ROM-efficient way to set level 0 odds
        
            LDA #250
            STA mnstrOdds
            LDA #128
            STA mnstrOdds+1
            LDA #15
            STA cactusOdds
            
            LDY #SFX_HOP
            JSR SFXtrigger

    ;Execute from here each time player dies
            
Continue:   LDA #0
            STA objFlags
            STA mnstrFlags
            STA mnstrXpos
            STA mnstrYpos
            STA mnstrHeight
            STA cactusXpos
            STA jump
            STA mnstrSize
            STA mnstrUnder
            STA mnstrSpeed
            STA flash
            STA mnstrRef
            
            STA CXCLR       ;clear all collision registers

            LDA gameState
            ORA #ACTIVE         ;flag ACTIVE
            AND #%11111101      ;unflag DEAD
            STA gameState
            
            LDA #DEFHOPDELAY
            STA hopDelay        ;initialize hop delay factor to default
            
            LDA #STREAK     ;reset streak ;-;
            STA streak

            LDA #PLR_HEIGHT
            STA plrHeight
            SEC
            SBC #1
            STA plrYpos
            
            LDA #HOP_HEIGHT
            STA hop

            LDA #$FF
            STA cactusHeight
            
            JSR LifeGrafs   ;this routine is time-inefficient and will overload the
                            ; vertical blank time at, like, 80 lives or something,
                            ; which even if that ever somehow happens will only cause
                            ; a frame or two of jitter on resuming the game   
                            
NotReset:   JSR DoPlayer    ;has to be a separate subroutine to keep branches in page
            JSR DoMnstr
            
            LDA delay
            BEQ ResDelay0   ;don't decrement delay if it's already 0
            DEC delay
ResDelay0:  LDA gameState
            ROR             ;is game state active? (Shifts ACTIVE bit into carry slot)
            BCC Inactive    ;if not, skip to "inactive" section
            
            JSR DoCactus    ;Make/move cactus object
            
    ; See if Doom Counter is at 0 yet
    ; - When Doom Counter hits 0, roll Doom Meter back 1
    ; - If Doom Counter goes over $FF, add one to Doom Meter
    ; - If Doom Meter hits 0, end the game!
    
            BIT gameState
            BVC DMNotZero
            
            LDA delay
            BNE DMNotZero
    
            LDA objSpeed    ;2 normally, 1 if slow, 4 if fast 
            SEC
            SBC #3          ; - 3 (change to 2 for a more merciful speed mode) 
            BMI Negative

            CLC
            ADC doomCtr     ;add positive number
            ADC #SPEED*2
            STA doomCtr
            BVC CheckDM

            SEC
            ROR doomMeter

            JMP CheckDM
            
Negative:   EOR #$FF
            CLC
            ADC #1+#SPEED   ;1 for twos compliment; SPEED for speed quotient
            STA temp
            LDA objSpeed
            LSR
            BNE NotSlow
            ASL temp
            
NotSlow:    LDA doomCtr
            SEC
            SBC temp
            STA doomCtr
            
            BCS CheckDM     ;did not cross back around past 0
            ASL doomMeter
 
CheckDM:    LDA doomMeter   ;is Doom Meter at 0?
            BNE DMNotZero
            
    ;doom meter has struck 0
    
            LDA #DEAD
            STA gameState
            LDA #$FF
            STA lives           ;-1 lives (game over)
            STA plrYpos
            STA mnstrYpos
            STA cactusHeight    ;everything disappears!
            LDA #$0F
            STA flash
            STA delay
            JSR SFXoff
            LDY #SFX_BEHSTMP
            JSR SFXtrigger

DMNotZero:  JMP Active          ;skip inactive section
            
    ;Inactive: only executes if game state is inactive

Inactive:   LDA #DEAD       
            BIT gameState   ;is player dead?
            BEQ DoTitle     ;if not, set then game is inactive but player isn't dead;
                            ; skip ahead and set title on 
            ASL             ;is TITLE ALSO set???
            BIT gameState
            BNE DoTitle
                        
    ;Dead player
            
            STA HMCLR       ;player is dead; clear horizontal move registers
            
            LDA delay
            BEQ Delay0      ;if delay=0, skip decrement and delay check
            JMP Active      ;...and skip input check
            
Delay0:     LDA INPT4       ;check if button is pressed
            BMI Active      ;if not, keep game paused
            
            LDA #20
            STA delay       ;resume game delay
            
            LDA lives       ;how many lives?
            BMI EndGame
            JMP Continue    ;if >=0, branch up to continue game

EndGame:    LDA #20
            STA delay
            JMP GameOver    ;if lives <0, reset to title
            
Active:     LDX #1
            LDA #134
            JSR XPosObject
            DEX
            LDA #142
            JSR XPosObject  ;positioning lives counter objects
          
            STA WSYNC
            STA HMOVE
            LDA #LIVES_COL
            STA COLUP0
            STA COLUP1      ;color of lives counter
            LDA skyCol
            STA COLUBK      ;color of sky
            BPL VertBlank   ;Skip turning on title
            
DoTitle:    LDA #TITLE
            ORA gameState
            STA gameState
            
    ;*** Any code in vertical blank goes above here ***

VertBlank:  LDA INTIM
            BNE VertBlank   ;little empty loop to check if we're at the end of the
                            ;timer yet
            LDX #1
            STA WSYNC       ;last WSYNC to start screen blanking in right spot
            STX VBLANK      ;End screen blanking, start drawing!
                
;-------------------------------------------------------------------------------------
;PICTURE - 192 scanlines, what will be visible onscreen
;-------------------------------------------------------------------------------------

            LDA #TITLE
            BIT gameState       ;is TITLE activated?
            BEQ DoPicture       ;if not, skip ahead to picture
            LSR
            BIT gameState       ;if so...is player also dead?
            BEQ DrawTitle
            JSR Endscreen       ;if so, do the endscreen instead of title
            JMP OverBlank
            
DrawTitle:  JSR Title           ;otherwise, go to title-drawing routine...
            JMP OverBlank       ;and then skip to Overblank
            
DoPicture:  JSR Picture         ;jump to draw primary gameplay frame

OverBlank:  LDA #2      ;Begin overscan blanking (another stupid bit 1 instruction)
            STA WSYNC   ;start new scanline first
            STA VBLANK  ;vertical blank: cease drawing anything to screen

            LDA #36
            STA TIM64T  ;set overscan timer (see vertical blank timer for equation)
                
;-------------------------------------------------------------------------------------
;Overscan - 30 lines of blank to fit visible display to all screens
;-------------------------------------------------------------------------------------

            JSR SFXUpdate   ;advance sound effects, once per frame

CheckFlash: LDA flash       ;is the flash on?
            BEQ TestLives   ;if not, skip ahead to lives check
            
    ;Flash effect
            
            DEC flash       ;subtract one from flash
            AND #%00001111  ;and every 16 frames...
            CMP #%00001000
            BNE TestLives
            LDA #DEAD
            BIT gameState
            BNE TestLives
            JSR SFXoff      ;play extra life sound
            LDY #SFX_LIFE
            JSR SFXtrigger
            
TestLives:  LDA streak      ;has streak counted down to 0?
            BNE TestColls
            
    ;Extra life!!!
    
            JSR SFXoff
            LDY #SFX_LIFE
            JSR SFXtrigger
            INC lives
            JSR LifeGrafs
            LDA #STREAK
            STA streak
            LDA #64
            STA flash
            
    ;Collision detections

TestColls:  LDA #DEAD
            BIT gameState   ;is player dead?
            BNE NotHurt     ;only check collisions if not already dead
            
            LDA #%01000000
            BIT CXP0FB      ;BIT 6: player collides with ball (cactus)
            BNE Dead
            LDA #%10000000
            BIT CXPPMM      ;BIT 7: player collides with monster
            BEQ NotHurt

Dead:       LDA gameState
            ORA #DEAD
            AND #%11111110
            STA gameState
            DEC lives       ;subtract one life
            LDA #60
            STA delay       ;set input delay timer
            
            JSR SFXoff      ;disable any sound effects currently playing
            LDY #SFX_OUCH
            JSR SFXtrigger
NotHurt:    JSR CalcTimer 
                     
    ;*** Code in overscan period goes above here ***

Overscan:   LDA INTIM       ;check if overscan timer has ended yet
            BNE Overscan 

            JMP FRAME       ;start new TV frame

;=====================================================================================
;END OF FRAME            
;=====================================================================================
;SUBROUTINES
;=====================================================================================
;Subroutines for drawing visible gameplay frame, title screen, and endscrees are
; in a separate file for editing convenience
;-------------------------------------------------------------------------------------

    .INCLUDE "picture.asm"
    
;-------------------------------------------------------------------------------------
;All subroutines related to monsters are in here, for the sake of easy editing.
;-------------------------------------------------------------------------------------

    .INCLUDE "monsters.asm"

;-------------------------------------------------------------------------------------
    ;===FINE ADJUST TABLE===
    ;Works with XPosObject to position objects horizontally with
    ;pixel precision, which Atari normally can't do by itself due
    ;to ratio of TIA and processor cycle times
;-------------------------------------------------------------------------------------

FADJLEFT = [#FINEADJ - *]d
    .ECHO "----", #FADJLEFT, "bytes before FineAdjust"
    .ALIGN 256  ;table only works if it's at top of page

FINEADJ = *

FineAdjust  DC.B %01110000      ;left 7
            DC.B %01100000      ;left 6
            DC.B %01010000      ;left 5
            DC.B %01000000      ;left 4
            DC.B %00110000      ;left 3
            DC.B %00100000      ;left 2
            DC.B %00010000      ;left 1
            DC.B %00000000      ;no adjust
            DC.B %11110000      ;right 1
            DC.B %11100000      ;right 2
            DC.B %11010000      ;right 3
            DC.B %11000000      ;right 4
            DC.B %10110000      ;right 5
            DC.B %10100000      ;right 6
            DC.B %10010000      ;right 7
        
fineAdjustTable .EQU FineAdjust - %11110001 ;11110001 is -15
          
;-------------------------------------------------------------------------------------
;GetFrames: Sets frames for monster animation
;-------------------------------------------------------------------------------------
  
GetFrames:  .SUBROUTINE

            LDY #15
.AnimLoop:  TYA
            LSR
            TAX
            LDA (tempPtr),Y
            STA mnstrAnimHi,X
            DEY
            LDA (tempPtr),Y
            STA mnstrAnimLo,X
            DEY
            BPL .AnimLoop
            
            RTS 
            
;-------------------------------------------------------------------------------------
;LivesGrafs
;-------------------------------------------------------------------------------------

LifeGrafs:  .SUBROUTINE

            LDY lives       ;set y to lives
            LDX #0          ;index for lives graphic
            TXA             ;set A 0 also
            STA livesGfx
            STA livesGfx+1  ;also initialize both graphic bytes to 0

.AddLife:   CLC             ;gotta clear this each time
            ROL
            ROL             ;move existing life over to next slot...
            ORA #%00000001  ;and add new one
            DEY             ;decrement lives counter
            BMI .Return     ;finish if no more lives
            STA livesGfx,X  ;otherwise store to current lives register
            BCC .AddLife    ;branch back if current lives register isn't full

            LDX #1          ;otherwise set index to 1 (second set of lives)
            LDA livesGfx+1  ;load in second register
            ORA #1          ;add a life...
            STA livesGfx+1  ;and update
            BCS .AddLife    ;and just keep going!

.Return:    RTS

;-------------------------------------------------------------------------------------
;Routine for initializing radiation dude
; Basically just an obstacle, sits and does nothing
;-------------------------------------------------------------------------------------

RadDude:    .SUBROUTINE

            LDA #5
            STA mnstrSize
            
            LDA #RADDUDE
            BIT mnstrFlags
            BNE .Spec

.Init:      LDA mnstrFlags
            ORA #RADDUDE
            STA mnstrFlags
            LDA #RADDUDE_HEIGHT-1
            STA mnstrHeight
            STA mnstrYpos
            INC mnstrYpos
            INC mnstrYpos
            LDA #150
            STA mnstrXpos   ;start monster at right of screen
            LDA #RADDUDE_COL
            STA mnstrCol

    ;Load animation frames into place
            
            LDX #7
.AnimLoop:  LDA #<(RadDudeFrm1 + RADDUDE_HEIGHT)
            STA mnstrAnimLo,X
            LDA #>(RadDudeFrm1 + RADDUDE_HEIGHT)
            STA mnstrAnimHi,X
            DEX
            BPL .AnimLoop

            RTS 
            
.Spec:      LDA mnstrCol
            EOR #%00001111
            STA mnstrCol
            STA COLUP1          ;flickering effect

.Return:    RTS 

;-------------------------------------------------------------------------------------
;DoPlayer: Fetches player animation frames, handles hopping, jumping, and death
; animation, and also deals with player input.  A very busy routine.
;-------------------------------------------------------------------------------------

DoPlayer:   .SUBROUTINE

    ;START OF DOPLAYER

            LDA gameState
            ROR             ;is game active?
            BCS .InitPlr    ;if so, branch ahead; otherwise check if player is dead
            
            ROR             ;is player dead?
            BCS .Dead
            RTS
            
.Dead:      LDA #<(PlrFrmDead + PLR_HEIGHT-1)
            STA plrPtr
            LDA #>(PlrFrmDead + PLR_HEIGHT-1)
            STA plrPtr+1
            
            LDA #<(PlrColor + PLR_HEIGHT-1)
            STA plrClrPtr
            LDA #>(PlrColor + PLR_HEIGHT-1)
            STA plrClrPtr+1
            
            JMP .SetPlayer

.InitPlr:   LDA #<(PlrColor + PLR_HEIGHT-1)
            STA plrClrPtr
            LDA #>(PlrColor + PLR_HEIGHT-1)
            STA plrClrPtr+1
            
            CLC
            LDA #PLR_HEIGHT-1
            CMP plrYpos
            BCC .notUnder
            STA plrYpos
            LDA #0
            STA jump
            
.notUnder:  LDA jump
            BNE .Input   ;if already jumping, then skip to forward/backward tests
            
;"HOPPING" CYCLE----------------
    
            LDA #<(PlrFrm1 + PLR_HEIGHT-1)
            STA plrPtr
            LDA #>(PlrFrm1 + PLR_HEIGHT-1)
            STA plrPtr+1
            
            INC hopCount        ;increment hop delay counter
            LDA hopDelay    
            CMP hopCount        ;has counter reached delay factor?
            BCS .Input          ;if not, skip to input testing

            LDA #0
            STA hopCount        ;otherwise, reset hop delay counter to 0
            
            LDA plrYpos
            CLC
            ADC hop             ;add "hop" to Y position
            STA plrYpos
            
            DEC hop             ;decrement hop distance (through negative)
            LDA hop
            CMP #-HOP_HEIGHT
            BNE .Input          ;if not, skip to input testing

            LDA #PLR_HEIGHT-1   ;otherwise, reset Y position...
            STA plrYpos
            LDA #HOP_HEIGHT     ;and hop
            STA hop
            
            LDY #SFX_HOP
            JSR SFXtrigger      ;play hopping sound
            
;GET INPUT-----------------------
    
.Input:     LDA delay
            BNE .Return
            LDA jump
            BNE .TestRight   ;if already jumping, then skip to forward/backward tests

.TestBtn:   LDA INPT4           ;JOYSTICK BUTTON
            BMI .TestRight
            LDA #HOP_HEIGHT
            STA hop             ;reset hop
            LDA #PLR_HEIGHT-1
            STA plrYpos         ;reset player Y position
            LDA #(JUMP_ARC+1)   ;jump arc value must be odd number
            STA jump            ;store jump arc into jump
            
            LDY #SFX_JUMP
            JSR SFXtrigger

.TestUp:    LDA #%00010000      ;Is UP also pressed?
            BIT SWCHA
            BNE .CalcJump       ;if not, check if down is pressed
            LDA jump
            CLC
            ADC #4              ;add to jump height if up is pressed
            STA jump

.CalcJump:  LDA jump
            LSR                 ;divide jump arc distance by 2  
            STA jumpHeight      ;store into jumpHeight

.TestRight: LDA #DEFHOPDELAY
            STA hopDelay        ;default hopping speed

            LDA #%10000000      ;RIGHT
            BIT SWCHA
            BNE .TestLeft
            INC objSpeed        ;increase speed of oncoming objects
            INC objSpeed        ;(by two)
            LDA #DEFHOPDELAY-1
            STA hopDelay        ;hop faster
                
.TestLeft:  LDA #%01000000      ;LEFT
            BIT SWCHA
            BNE .Return
            DEC objSpeed        ;decrease speed of oncoming objects by one
            LDA #DEFHOPDELAY+1
            STA hopDelay        ;hop slower
.Return:

;JUMP DYNAMICS-------------------

            LDA jump
            BEQ .SetPlayer       ;if jump is 0, skip jump processing        
            
.DoJump:    LDA #<(PlrFrmJmp + PLR_HEIGHT-1)    ;load jump animation frame
            STA plrPtr
            LDA #>(PlrFrmJmp + PLR_HEIGHT-1)    ;higher byte of same
            STA plrPtr+1 
            
            INC jumpCount       ;Increment jump delay counter
            LDA #JUMP_DELAY     ;have we reached the delay factor?
            BIT jumpCount       ;test if we have
            BEQ .SetPlayer       ;if not, don't do any jump logic
            LDA #0              
            STA jumpCount       ;otherwise, reset jump delay counter

            DEC jump            ;Reduce jump quotient by 1

            LDA plrYpos         ;start with current Y position
            CLC                 ;MUST CLEAR CARRY FLAG HERE!!!
            ADC jump            ;add jump quotient
            SEC
            SBC jumpHeight      ;then subtract height of player jump
            
            STA plrYpos         ;...and finally, update player's Y position 
            
            LDA #%00100000
            BIT SWCHA
            BNE .NoDown
            LDA plrYpos
            SEC
            SBC #7
            STA plrYpos
.NoDown:  

            LDA jump
            BNE .SetPlayer      ;has jump reached 0 now?
            
            LDY #SFX_HOP
            JSR SFXtrigger      ;play hopping sound on landing
            
;PLAYER ANIMATION---------------

.SetPlayer: LDA #MAIN_HEIGHT    ;start with height of playable area
            CLC
            ADC plrHeight       ;plus height of player...
            SEC
            SBC plrYpos         ;minus player position from bottom...
            STA plrDraw         ;gives us line to start drawing player
            
            LDA plrPtr          ;get loaded player animation frame
            SEC
            SBC plrYpos         ;subtract player's Y position
            STA plrPtr          ;update plrPtr
            BCS .SetPlrCol      ;branches based on 
            DEC plrPtr+1
            
.SetPlrCol: LDA plrClrPtr
            SEC
            SBC plrYpos
            STA plrClrPtr
            BCS .PCNoCarry
            DEC plrClrPtr+1
.PCNoCarry:
            
    ; END OF DOPLAYER
            
            RTS
         
;-------------------------------------------------------------------------------------
;XPosObject
;Handles fine X positioning of objects. A and X should be loaded BEFORE this sub-
;routine is called.
;-------------------------------------------------------------------------------------
                
XPosObject: .SUBROUTINE              ;A = X coordinate

            STA WSYNC               ;X = object to be moved
            SEC
.DivBy15:   SBC #15
            BCS .DivBy15

            TAY
            LDA fineAdjustTable,Y
            STA HMP0,X              ;the x should be from 0 to 5, and will correspond
                                    ;to HMP0, HMP1, HMM0, HMM1, HMBL
            STA RESP0,X             ;Commit to draw sprite at this position
            
            STA WSYNC

            RTS   
               
;=====================================================================================
;TABLES and some RELATED SUBROUTINES
;=====================================================================================
;GRAPHIC TABLES
;-------------------------------------------------------------------------------------

    .INCLUDE "graphics.asm"
    
;-------------------------------------------------------------------------------------
;SOUND EFFECTS ROUTINES AND TABLES
;-------------------------------------------------------------------------------------

    .INCLUDE "sfx.asm"                  

;-------------------------------------------------------------------------------------
;LEVEL (COLOR) TABLES
;-------------------------------------------------------------------------------------
;Every couple of levels the background colors change.  The colors per level are
;stored here. Unlike other graphics/color tables, these are stored top to bottom. This
;method seems pretty storage-intensive, and is, but I wanted to have a progression of
;particular color schemes and this seemed to me the best way to do it

CTABLEFT = [#COLORTABS - *]d
    .ECHO "----", #CTABLEFT, "bytes before color tables"
    .ALIGN 256
COLORTABS = *

;00:00-01:30
;LEVEL 0-1-----------   Desert, day
Level1
SkyCol1     .BYTE $44
            .BYTE $46
            .BYTE $48
            .BYTE $4A
            .BYTE $4C
            .BYTE $4E
            
CloudCol1   .BYTE $0D       ;FLASHES
            .BYTE $0F
            
MtnCol1     .BYTE $35       ;FLASHES

BakCol1     .BYTE $43       ;FLASHES

COLORS      = * - Level1    ;Get size for all color tables

;01:30-02:30
;LEVEL 2-------------   Desert, dusk
Level2
SkyCol2     .BYTE $00
            .BYTE $02
            .BYTE $50
            .BYTE $02
            .BYTE $50
            .BYTE $52
            
CloudCol2   .BYTE $31
            .BYTE $1E
            
MtnCol2     .BYTE $61

BakCol2     .BYTE $51

;02:30-03:30
;LEVEL 3-------------   Desert, night
Level3
SkyCol3     .BYTE $00
            .BYTE $00
            .BYTE $80
            .BYTE $00
            .BYTE $80
            .BYTE $60
            
CloudCol3   .BYTE $03
            .BYTE $08
            
MtnCol3     .BYTE $01

BakCol3     .BYTE $01

;03:30-04:30
;LEVEL 4-------------   Deep night
Level4
SkyCol4     .BYTE $00
            .BYTE $00
            .BYTE $00
            .BYTE $00
            .BYTE $02
            .BYTE $04
            
CloudCol4   .BYTE $43
            .BYTE $1C
            
MtnCol4     .BYTE $01

BakCol4     .BYTE $01

;04:30-05:30
;LEVEL 5-------------   Foggy
Level5
SkyCol5     .BYTE $00
            .BYTE $02
            .BYTE $04
            .BYTE $06
            .BYTE $08
            .BYTE $0A
            
CloudCol5   .BYTE $03
            .BYTE $04
            
MtnCol5     .BYTE $51

BakCol5     .BYTE $A1

;05:30-06:30
;LEVEL 6-------------   Snowstorm
Level6
SkyCol6     .BYTE $04
            .BYTE $02
            .BYTE $06
            .BYTE $04
            .BYTE $02
            .BYTE $00
            
CloudCol6   .BYTE $03
            .BYTE $06
            
MtnCol6     .BYTE $0E

BakCol6     .BYTE $03

;06:30-07:30
;LEVEL 7-------------   Snowy, clear
Level7
SkyCol7     .BYTE $9E
            .BYTE $9E
            .BYTE $9E
            .BYTE $9C
            .BYTE $9A
            .BYTE $8A
            
CloudCol7   .BYTE $0E
            .BYTE $0C
            
MtnCol7     .BYTE $0E

BakCol7     .BYTE $a5

;07:30-08:30
;LEVEL 8-------------   Green
Level8
SkyCol8     .BYTE $9E
            .BYTE $9E
            .BYTE $9E
            .BYTE $9C
            .BYTE $9A
            .BYTE $8A
            
CloudCol8   .BYTE $0E
            .BYTE $0C
            
MtnCol8     .BYTE $F7

BakCol8     .BYTE $F3

;-------------------------------------------------------------------------------------
;LEVELING ROUTINES
;-------------------------------------------------------------------------------------

    .INCLUDE "leveling.asm"
    
;-------------------------------------------------------------------------------------
;Random
;8-16 BIT "random" number generation algorithm...basically just a bunch of BIT-
; shifting and bitwise operations to create simulated randomness
;Notice the RTI? The BRK IRQ is hard-coded to jump to Random, so we can save a few
; precious bytes for every call for a random number.
;-------------------------------------------------------------------------------------

Random:     .SUBROUTINE ;.SUBROUTINE means we can use temporary labels in this
                        ;section (e.g. .NoEor)
            LDA rand8
            LSR         ;shift lower 8 bits right
            ROL rand16
            
            BCC .NoEor
            EOR #$B4    ;if carry flag is set, flip some bits
.NoEor:     STA rand8   ;...and store into lower 8 bits
            EOR rand16

            RTI

;=====================================================================================
;ROM check and interrupt vectors
;=====================================================================================

    ;Assembler will tell us how much ROM is left
    .IF (* & $FF)
INTLEFT = [($FFFA - *)]d
        .ECHO "----", #INTLEFT, "bytes before interrupt vectors"
    .ENDIF 

    ;Interrupt vectors: at program start and certain other events, these are the bytes
    ;the Atari is hardcoded to look at to decide where to begin execution
    
    .ORG $FFFA  ;at the very end of ROM
    
        .WORD RESET     ;NMI
        .WORD RESET     ;RESET
        .WORD Random    ;IRQ (BRK will jump to this location in memory)
