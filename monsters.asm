
;   These are the monster-specific subroutines for BreaQing Badd Balloon. The
;   cactus routine is included in this document despite the fact that the cactus
;   is calculated AND handled separately.

;-------------------------------------------------------------------------------------
;DoMnstr
;Chooses whether to send a monster, which monster to send, and its movement speed,
;graphics, object color, and animation
;-------------------------------------------------------------------------------------
    
DoMnstr:    .SUBROUTINE
            
            LDA #MONSTER
            BIT objFlags
            BNE .Exists     ;if there's already a monster/title, branch ahead
            
            LDA delay
            BEQ .Select
            RTS
            
    ;See if the game is active
          
.Select:    LDA gameState
            ROR
            BCS .Randomize  ;if so, randomize monster generation           
            RTS             ;otherwise, no need to draw anything; abort routine now
            
    ;If there's no monster already, check if we should create one and, if so, what
    ; type of monster it should be

.Randomize: BRK             ;generate random #: will there be a monster?
            .BYTE $0E
            CLC
            CMP mnstrOdds   ;odds of a monster appearing (varies by level)
            BCS .Create
            RTS 
           
.Create:    LDA objFlags
            ORA #MONSTER
            STA objFlags    ;set monster flag to on
            
            BRK             ;if there is a monster, what kind should it be?
            .BYTE $0E
.CheckRD:   CMP mnstrOdds+1
            BCC .CheckBoid
            JSR RadDude
            JMP .Standard
.CheckBoid: CMP mnstrOdds+2 ;Birdoid's probability range
            BCC .CheckMnchr  
            JSR Birdoid
            JMP .Standard
.CheckMnchr:CMP mnstrOdds+3 ;Muncher's probability range
            BCC .CheckWorm  
            JSR Muncher
            JMP .Standard
.CheckWorm: CMP mnstrOdds+4 ;Worm's probability range
            BCC .CheckEye  
            JSR Worm
            JMP .Standard
.CheckEye:  CMP mnstrOdds+5 ;Eye's probability range
            BCC .CheckBB
            JSR Eye
            JMP .Standard
.CheckBB:   CMP mnstrOdds+6 ;Blue Boi's probability range
            BCC .CheckBeh
            JSR BlueBoi
            JMP .Standard
.CheckBeh:  CMP mnstrOdds+7 ;Behemoth's probability range
            BCC .NoMnstr
            JSR Behemoth
            JMP .Standard

.NoMnstr:   LDA objFlags
            AND #DISABLE_MNSTR
            STA objFlags
            RTS 

    ;If monsters already exist, do monster-specific behavior processing
            
.Exists:    
.SpecWorm:  LDA #WORM       ;Worm-specific routines
            BIT mnstrFlags
            BEQ .SpecMnchr            
            JSR Worm 

.SpecMnchr: LDA #MUNCHER    ;Muncher-specific routines
            BIT mnstrFlags
            BEQ .SpecBoid            
            JSR Muncher
            
.SpecBoid:  LDA #BIRDOID    ;birdoid-specific routines
            BIT mnstrFlags
            BEQ .SpecEye            
            JSR Birdoid
            
.SpecEye:   LDA #EYE        ;eye-specific routines
            BIT mnstrFlags
            BEQ .SpecBeh            
            JSR Eye
            
.SpecBeh:   LDA #BEHEMOTH   ;behemoth-specific routines
            BIT mnstrFlags
            BEQ .SpecRad
            JSR Behemoth
            
.SpecRad:   LDA #RADDUDE    ;radation dude-specific routines
            BIT mnstrFlags
            BEQ .SpecBB
            JSR RadDude
            
.SpecBB:    LDA #BLUEBOI
            BIT mnstrFlags
            BEQ .Standard
            JSR BlueBoi
            
    ;Loads created/current monster/title's graphics into their right place, and them
    ; as necessary
           
.Standard:  LDA #ACTIVE
            BIT gameState
            BEQ .NotAtEdge      ;don't move horizontally if game is inactive

            LDA mnstrXpos
            TAX                 ;save x position into x register
            SEC
            SBC objSpeed
            SBC mnstrSpeed      ;some monsters move faster than oncoming cacti
            STA mnstrXpos  
            CPX mnstrXpos       ;compare new xpos with old xpos       
            BNE .CheckEdge
            DEC mnstrXpos       ;if they're the same, reduce xpos by 1 (makes sure
                                ;monsters are always moving)
.CheckEdge: CMP #8              ;Has the monster left the screen?
            BCS .NotAtEdge
            
    ;Monster has left the frame
    
            LDA objFlags
            AND #DISABLE_MNSTR
            STA objFlags        ;switch MONSTER flag off
            LDA #0
            STA mnstrFlags
            STA mnstrUnder
            STA mnstrSize
            STA mnstrSpeed
            STA mnstrRef
            ;STA bbHop
            ;STA bbHopHeight
            DEC streak          ;add to dodged monsters streak
            INC flash           ;flash for one frame
            LDY #SFX_GOTEM      ;play "dodged monster" sound effect
            JSR SFXtrigger
            RTS                 ;no more monster; quit subroutine
            
    ;Monster hasn't yet left the frame
            
.NotAtEdge: LDA #MAIN_HEIGHT    ;start with screen height
            CLC
            ADC mnstrHeight     ;add height of monster
            ADC #1              ;add one more
            SEC
            SBC mnstrYpos       ;minus Y position of top of monster, from bottom
            STA mnstrDraw       ;...gives us counter of when to draw the monster

            LDA animDelay
            EOR #%00000001      ;flip BIT 0 of animDelay
            STA animDelay
            BEQ .SkipAnim
            
            LDA #DEAD
            BIT gameState
            BNE .SkipAnim

            INC mnstrFrame      ;increment to next frame
.SkipAnim:              

            LDA mnstrFrame
            AND #7              ;limit monster to 8 frames of animation
            STA mnstrFrame
            TAX                 ;transfer frame # to X register
            LDA mnstrAnimLo,X
            SEC
            SBC mnstrYpos
            STA mnstrPtr
            LDA mnstrAnimHi,X
            SBC #0
            STA mnstrPtr+1        ;mnstrPtr contains address of sprite table

.Return:    RTS 

;-------------------------------------------------------------------------------------
;Routine for initializing muncher
; Little dude who just runs straight across screen. After a few levels, they start
; occasionally appearing in pairs
;-------------------------------------------------------------------------------------

Muncher:    .SUBROUTINE

            LDA #MNCHR_COL
            STA mnstrCol
            
            LDA #MUNCHER
            BIT mnstrFlags
            BNE .Spec
            
.Init:      LDA mnstrFlags
            ORA #MUNCHER
            STA mnstrFlags
            LDA #MUNCHER_HEIGHT-1
            STA mnstrYpos
            STA mnstrHeight
            INC mnstrYpos
            INC mnstrYpos
            LDA #160
            STA mnstrXpos   ;start monster at right of screen
            BRK
            .BYTE $0E
            AND #1
            CLC
            ADC #1
            STA mnstrSpeed  ;randomize speed to either 1 or 2
            LDA #0
            STA multMnchr   ;defaults to 0
            
            LDA level
            CMP #5          ;level at which multiple munchers can appear (5)
            BCC .1mnchr
            
            BRK
            .BYTE $0E
            AND #3          ;determine whether there will be 2 munchers
            BNE .1mnchr  
            
            LDA #%00000100
            STA multMnchr
            
.1mnchr:    LDA #<MnchrFrames
            STA tempPtr
            LDA #>MnchrFrames
            STA tempPtr+1
            JSR GetFrames
            
            LDY #SFX_MCHR
            JSR SFXtrigger

            RTS 

.Spec:      LDX multMnchr
            BEQ .Return
            LDA mnstrXpos
            CMP #(160-64)       ;time for second muncher to appear? 64
            BCS .Return         ;if not then skip ahead
            STX mnstrSize
            
            LDA mnstrXpos
            CMP #16
            BCS .Return         ;x position >= 12 (first muncher is gone)
            CPX #0
            BEQ .Return         ;if so, skip ahead
            
            CLC
            ADC #64
            STA mnstrXpos
            LDA #0
            STA multMnchr
            STA mnstrSize
            DEC streak          ;dodged first muncher
            LDY #SFX_GOTEM
            JSR SFXtrigger
            INC flash

.Return     RTS 

;-------------------------------------------------------------------------------------
;Routine for initializing worm
; Purple worm who starts rising out of the ground, blocking player's path if player's 
; too slow. Has no forward motion
;-------------------------------------------------------------------------------------

Worm:       .SUBROUTINE

            LDA #WORM_COL
            STA mnstrCol
            
            LDA #WORM
            BIT mnstrFlags
            BNE .Spec
      
.Init:      LDA mnstrFlags
            ORA #WORM
            STA mnstrFlags           
            LDA #WORM_HEIGHT-1
            STA mnstrHeight
            BRK
            .BYTE $0E
            AND #30                 ;worm max height of n+WORM_HEIGHT-1
            CLC
            ADC #6                  ;min height
            STA mnstrYpos
            LDA #160
            STA mnstrXpos
            LDA #FPS/3
            STA wormTmr
            LDA #1
            STA wormDir             ;worm starts moving up (add 1 each frame)
           
            LDA WormFrm1
            STA mnstrUnder
            
            LDA #<WormFrames
            STA tempPtr
            LDA #>WormFrames
            STA tempPtr+1
            JSR GetFrames

            RTS 
            
.Spec:      LDA gameState
            ;ASL
            ROR
            BCC .Return         ;if inactive, skip Ypos processing
            
            DEC wormTmr         ;increment worm timer 1 frame
            BNE .NoSound        ;if EXACTLY 1/3 SEC, play sound effect
            LDY #SFX_WORM
            JSR SFXtrigger
.NoSound:   BPL .Return         ;if less than 1/3 SEC, skip it goood
            
            LDA mnstrYpos
            CMP #MAIN_HEIGHT    ;worm won't raise head outside of gameplay frame
            BNE .NotAtTop       ;is the worm at the top yet?
            LDA #%11111111      ;if so, switch direction to -1
            STA wormDir
            LDA mnstrYpos
.NotAtTop:  
            CLC
            ADC wormDir         ;add/subtract 1 to/from worm Y position
            STA mnstrYpos  
            
            ;JMP .Return
            ;LDA mnstrXpos
            ;CMP #PLRXPOS        ;has player passed worm?
            ;BCS .Return
            ;STA REFP1           ;if so, worm looks around
           
.Return     RTS 
           
;-------------------------------------------------------------------------------------
;Routine for initializing birdoid
; Just a little bird. Has slight up/down motion, very slow forward motion
;-------------------------------------------------------------------------------------
            
Birdoid:    .SUBROUTINE

            LDA #BOID_COL
            STA mnstrCol
            
            LDA #BIRDOID
            BIT mnstrFlags
            BNE .Spec
    
.Init:      LDA mnstrFlags
            ORA #BIRDOID
            STA mnstrFlags           
            LDA #BOID_HEIGHT-1
            STA mnstrHeight
            BRK
            .BYTE $0E
            AND #63                  ;birdoid max Y
            ADC #BOID_HEIGHT      ;min height is height of head graphic
            STA mnstrYpos
            STA boidInitYpos
            LDA #160
            STA mnstrXpos
            LDA #$FF                ;birdoid hovers slowly along
            STA mnstrSpeed
            LDA #%00000111
            STA boidTmr             ;initialize bird movement timer
            
            LDA #<BoidFrames
            STA tempPtr
            LDA #>BoidFrames
            STA tempPtr+1
            JSR GetFrames
            
            LDY #SFX_BOID
            JSR SFXtrigger
            
            RTS 

.Spec:      LDA gameState
            ;ASL
            ROR
            BCC .Return        ;if inactive, skip Ypos processing
    
            DEC boidTmr     ;decrement birdoid timer
            LDA boidTmr
            AND #%00000011
            STA boidTmr
            BNE .Return     ;if != 0, end birdoid routine  
               
            LDA boidInitYpos
            CMP #(MAIN_HEIGHT/2)
            BCS .HighBird
            INC mnstrYpos
            .BYTE $2C
.HighBird   DEC mnstrYpos
            
.Return:    RTS 
            
;-------------------------------------------------------------------------------------
;Routine for initializing eye
; Nasty flying critter who tries to follow player.  Moves faster up than down
;-------------------------------------------------------------------------------------

Eye:        .SUBROUTINE

            LDA #EYE_COL
            STA mnstrCol
            LDA #5
            STA mnstrSize
            
            LDA #EYE
            BIT mnstrFlags
            BNE .Spec

.Init:      LDA mnstrFlags
            ORA #EYE
            STA mnstrFlags
            BRK
            .BYTE $0E
            AND #63                 ;max Eye height
            ADC #(EYE_HEIGHT/2)
            STA mnstrYpos
            LDA #EYE_HEIGHT-1
            STA mnstrHeight
            LDA #150
            STA mnstrXpos           ;start monster at right of screen
            LDA #$FF
            STA mnstrSpeed
            
            LDA #<EyeFrames
            STA tempPtr
            LDA #>EyeFrames
            STA tempPtr+1
            JSR GetFrames

            RTS 

.Spec:      LDA gameState
            ROR
            BCC .Return        ;if inactive, skip Ypos processing
            
            BRK
            .BYTE $0E           ;garbled Eye noise
            AND #%11110111      ;keeps volume from going too high
            STA AUDV1
            LSR
            LSR
            LSR
            LSR
            STA AUDC1
            BRK                 ;randomize frequency separately
            .BYTE $0E
            STA AUDF1

            LDX mnstrXpos
            CPX #140
            BCS .Return
            LDA plrYpos
            CMP mnstrYpos       ;compare eye's Y to player's Y
            BEQ .Return         ;if equal, skip to main monster processing
            BCC .HighEye        ;if higher than player, skip ahead
            INC mnstrYpos       ;otherwise, move eye up by 2
            INC mnstrYpos
            .BYTE $2C           ;skip next line
.HighEye:   DEC mnstrYpos
.Cont:      CPX #90
            BCS .Return
            LDA #1              ;if this is >1...
            STA mnstrSpeed      ;...increase eye speed once it's past X=60
            
.Return:    RTS 

;-------------------------------------------------------------------------------------
;Routine for initializing behemoth
; Large enemy who rises out of the ground, charges player, and then slips into reverse
; near edge of screen, making him more persistent than other foes
;-------------------------------------------------------------------------------------

Behemoth:   .SUBROUTINE

            LDA #BEH_COL
            STA mnstrCol
            LDA #7
            STA mnstrSize

            LDA #BEHEMOTH
            BIT mnstrFlags
            BNE .Spec

.Init:      LDA mnstrFlags
            ORA #BEHEMOTH
            STA mnstrFlags
            LDA #BEH_HEIGHT-1
            STA mnstrHeight
            LDA #0
            STA mnstrYpos
            LDA #BEH_START
            STA mnstrXpos   ;start monster at right of screen
            LDA #3
            STA mnstrSpeed
            
            LDA #<BehFrames
            STA tempPtr
            LDA #>BehFrames
            STA tempPtr+1
            JSR GetFrames
            
            LDY #SFX_BEHRIS
            JSR SFXtrigger

            RTS 
            
.Spec:      LDA #DEAD           ;is player dead?
            BIT gameState
            BNE .Return         ;if so, end behemoth subroutine
            
            LDA mnstrHeight
            CLC
            ADC #1
            CMP mnstrYpos       ;is monster risen yet?
            BCC .Attack         ;if so, do normal movement behavior

            LDA #BEH_START      ;keep X position constant until it's up to ground
            STA mnstrXpos       ; level
            INC mnstrYpos       ;rise up a bit
            LDA #0
            STA mnstrFrame      ;stay at first frame of animation
            RTS 
            
.Attack:    LDA mnstrSpeed
            BPL .Forward
            STA mnstrRef
.Forward:   LDA mnstrFrame
            AND #2
            BNE .NoSound
            
            LDY #SFX_BEHSTMP    ;only make stomp noise on second frame
            JSR SFXtrigger
.NoSound:   LDA mnstrXpos
            CMP #30             ;is behemoth at X=37?
            BCS .Charge         ;if more, go to .Charge
            LDA #%11111100      ;otherwise, make behemoth move backwards
            STA mnstrSpeed
            RTS 
            
.Charge:    CMP #100
            BCC .Return
            LDA #3
            STA mnstrSpeed
            STA mnstrRef
            
.Return:    RTS 
            
;-------------------------------------------------------------------------------------
;Radiation Dude has been moved elsewhere for space purposes!!!
;-------------------------------------------------------------------------------------
;Routine for initializing blue boi
; Annoying hopping boi with predictable but hard-to-dodge movement pattern
;-------------------------------------------------------------------------------------

BlueBoi:    .SUBROUTINE
            
            LDA #BLUEBOI_COL
            STA mnstrCol
            
            LDA #BLUEBOI
            BIT mnstrFlags
            BNE .Spec

.Init:      LDA mnstrFlags
            ORA #BLUEBOI
            STA mnstrFlags
            LDA #BB_HEIGHT-1
            STA mnstrHeight
            LDA #BB_HEIGHT+7
            STA mnstrYpos
            INC mnstrYpos
            LDA #160
            STA mnstrXpos       ;start monster at right of screen
            LDA #1
            STA mnstrSpeed
            
            BRK
            .BYTE $0E           ;randomize initial hop
    
            AND #%00000110
            CLC
            ADC #3
            STA bbHopHeight
            STA bbHop
            
            LDA #7
            STA mnstrFrame
            
            LDA #0
            STA mnstrUnder
            
            LDA #<BBFrames
            STA tempPtr
            LDA #>BBFrames
            STA tempPtr+1
            JSR GetFrames

            RTS 
            
.Spec:      LDA #DEAD
            BIT gameState
            BNE .Return         ;skip jumping if player is dead
            
            LDA #%00000001
            BIT animDelay
            BNE .Return         ;skip jumping every other frame of animation, to avoid
                                ;weird animation clipping
            LDA mnstrFrame                                
            CMP #6
            BNE .NoSound
            LDY #SFX_BB
            JSR SFXtrigger 
                       
.NoSound:   LDA mnstrFrame
            BNE .Return         ;skip jumping if blue boi is still winding up    
            
        ;is jumping        

            LDA #7              ;this is actually the frame BEFORE the jumping
                                ; animation, since it always increments in the Monster
                                ; routine
            STA mnstrFrame      ;keep at "jumping" frame while in the air
            
            LDA mnstrSpeed
            EOR #%00000001
            STA mnstrSpeed      ;one way of doing 1/2 monster speed

            LDA mnstrYpos
            CLC
            ADC bbHop           ;add "hop" to Y position
            STA mnstrYpos
            
            DEC bbHop           ;decrement hop distance (through negative)
            LDA bbHop
            CLC
            ADC bbHopHeight
            BNE .Return         ;if not, skip to input testing
            
        ;has landed

            LDA #BB_HEIGHT+1    ;otherwise, reset Y position...
            STA mnstrYpos
            BRK                 ;randomize next hop
            .BYTE $0E
            AND #%00000110
            CLC
            ADC #3
            STA bbHopHeight     ;set new hop height with each jump
            STA bbHop    
            INC mnstrFrame      ;start advancing animation once on ground
            LDA #0
            STA mnstrSpeed      ;monster doesn't move forward while winding up to hop

.Return:    RTS 

;-------------------------------------------------------------------------------------
;DoCactus: Handles creation and movement of cactus objects
;-------------------------------------------------------------------------------------

DoCactus:   .SUBROUTINE

            LDA #CACTUS
            BIT objFlags
            BNE .Exists     ;there's already a cactus--skip ahead
            
            LDA delay
            BNE .Return

            BRK
            .BYTE $0E
            AND cactusOdds
            BNE .Return     ;unless the number is 0, don't create a cactus

            LDA objFlags
            ORA #CACTUS          
            STA objFlags        ;set cactus flag to on
            
            BRK
            .BYTE $0E
            AND #3              ;random # from 0 to 3
            ASL                 ;shift left to correct ball object bits...
            ASL
            ASL
            ASL
            STA cactusWidth     ;generate cactus width (1, 2, 4, or 8 pixels)
            
            LDA level
            CMP #8              ;no rocks before this level
            BCS .RockLev

            LDA cactusWidth
            CMP #%00110000
            BNE .CactusCol
            LDA #%00100000
            STA cactusWidth

.RockLev:   LDA cactusWidth        
            CMP #%00110000
            BNE .CactusCol
            LDA #ROCK_BASECOL
            .BYTE $2C
.CactusCol: LDA #CACTUS_BASECOL
            STA cactusCol

            BRK                 ;randomize cactus height
            .BYTE $0E
            AND #31
            CLC
            ADC #8              ;minimum height of 8 (total 8-39)
            STA cactusHeight    ;set cactus height
            
            LDA #%00110000
            CMP cactusWidth
            BNE .NotRock

            LDA cactusHeight
            ADC #10
            STA cactusHeight
            LDA #ROCK_BASECOL
            STA cactusCol+1
            JMP .SetX

.NotRock:   BRK
            .BYTE $0E           ;randomize cactus color
            AND #%00000111
            ORA #%00001100
            CLC
            ADC cactusCol
            STA cactusCol
            EOR #%00000111      ;flip a few bits for a...
            STA cactusCol+1     ;second cactus color

.SetX:            
            LDA #160            ;coordinate for right side of screen
            STA cactusXpos      ;set cactus starting X
   
.Exists:    LDA cactusXpos          ;calculate horizontal cactus movement
            SEC
            SBC objSpeed
            STA cactusXpos
            CMP #6                  ;cactus flickers if I just compare with 0
            BCS .NotAtEdge

            LDA objFlags            ;Eliminate cactus once it reaches edge of screen
            AND #(%11111111-CACTUS)
            STA objFlags            ;turn cactus flag off
            LDA #$FF
            STA cactusHeight        ;set height to -1 (it will not show this way)

.NotAtEdge: 

.Return:    RTS 
   
