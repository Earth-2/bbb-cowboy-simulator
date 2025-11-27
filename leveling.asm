
;   LEVELING.ASM contains routines and tables connected with levelling up,
;   including level color tables and routines for loading them, and routine for
;   getting odds of monsters/cacti appearing in a level

;-------------------------------------------------------------------------------------
;LevelUp
;Calculates player's current level every LEVEL_TIME seconds by subtracting LEVEL_TIME
;from the current time in seconds repeatedly until the result is 0, suggesting that
;the current time is divisible by LEVEL_TIME AND that it's time to level up.
;This routine assumes that it begins in decimal mode, and remains in decimal mode on
;return.
;-------------------------------------------------------------------------------------

LevelUp:    .SUBROUTINE

            SEC
.GetMod:    SBC #LEVEL_TIME ;how fast player levels up (in BCD!)
            BEQ .LevUp
            BMI .SkipLevUp
            BPL .GetMod

.LevUp:     INC level       ;level up!
            CLD             ;exit decimal mode
            JSR FetchOdds   ;Per level, get odds of things appearing
            LDA #%00000001  ;ensure we only call GetLevCols on even-numbered levels
            BIT level       
            BEQ .SkipLevUp
            JSR GetLevCols
            
.SkipLevUp: SED             ;aaaand back to decimal mode

            RTS

;-------------------------------------------------------------------------------------
;FetchOdds: Per difficulty level, generate odds of any monster/cactus appearing at a
; given moment.
;-------------------------------------------------------------------------------------

FetchOdds:  .SUBROUTINE
     
            LDA level
            BEQ .Return         ;if level=0, odds table already loaded
            CMP #20             ;leveling stops here
            BCS .Return         ;also true if level >= 30
            
            AND #%00000011
            BNE .EachMnstr
            LDA mnstrOdds
            SEC
            SBC #8              ;subtract previous odds by this number
            ;LSR                 ;divide previous mnstrOdds by 2
            CMP #3              ;odds never greater than 1/4
            BCC .EachMnstr
            STA mnstrOdds
            
            LDA level
            CMP #14
            BCS .Return
            
.EachMnstr: LDX #1              ;start doing odds for each monster        
            ;LDX #ODDS-1
.OddsLoop:  LDA mnstrOdds,X
            BEQ .QuitOdds       ;if mnstrOdds = 0, leave loop for further instructions
                                ;(means monster is not on level)
            LDA #255            ;255
            SEC
            SBC mnstrOdds,X     ; - current odds
            LSR                 ; / 2
            LSR                 ; / 2
            LSR                 ; / 2
            LSR
            CLC
            ADC mnstrOdds,X     ; + current odds
            STA mnstrOdds,X     ; = the new odds
            
.Next:      INX                 ;next mnstrOdds table index
            CPX #ODDS-1           ;are we at last element of table?
            BNE .OddsLoop       ;if not, keep going

.QuitOdds:  LDA level
            CMP #15
            BCS .Return         ;no new monsters from level 13 onwards
            
            CMP #9
            BEQ .Return         ;skip a couple levels before introducing behemoth
            CMP #11
            BEQ .Return
            
            AND #%00000001
            CMP #%00000001      ;new monster on level 1, 5, 9, etc.
            BNE .Return
            
            DEX                
            LDA mnstrOdds,X     ;grab odds of previous monster
            LSR                 ; / 2
            LSR                 ; / 2
            LSR                 ; / 2
            LSR                 ; / 2!
            INX
            STA mnstrOdds,X     ;set initial odds of new monster
            
.Return:    RTS

;-------------------------------------------------------------------------------------
;GetLevCols
;-------------------------------------------------------------------------------------

GetLevCols: .SUBROUTINE
            
            LDA #<Level1    ;Load lower byte of Level1 table's address
            STA tempPtr
            LDA #>Level1    ;Load upper byte of Level1 table's address
            STA tempPtr+1
            LDA level
            LSR             ;divide level # by 2, to increment through table
            AND #%00000111  ;limit to 0-7
            TAX             ;transfer to index register x
            BEQ .FirstLev   ;For levels 0 and 1 (00:00-01:30)

.NextLevel: LDA tempPtr
            CLC
            ADC #COLORS     ;move forward on color table
            STA tempPtr     ;update tempPtr
            DEX
            BNE .NextLevel  ;do again until on current level (x=0)
            
.FirstLev:  LDY #COLORS
.SetCols:   LDA (tempPtr),Y ;indirect addressing; dereference address stored at location
            STA skyCol,Y    ;direct addressing; store dereferenced value into array
            DEY
            BNE .SetCols    
            
.Return:    RTS
  