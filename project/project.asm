	processor 6502

	include "vcs.h"
	include "macro.h"


	seg.u Variables
	org $80

PlaXPos		byte
PlaYPos 	byte
EnnemyXPos	byte
EnnemyYPos	byte
MissileXPos	byte
MissileYPos	byte
Score		byte
Timer		byte
AnimOffset      byte
Random          byte
ScoreSprite     byte
TimerSprite     byte
PfColor    	byte
BgColor      	byte
Temp		byte

OnesDigitOffset word
TensDigitOffset word
PlaSpritePtr	word
PlaColorPtr	word
EnnemySpritePtr	word
EnnemyColorPtr	word		; defining pointers lookup table

PLA_HEIGHT = 9
ENNEMY_HEIGHT = 9
DIGITS_HEIGHT = 5


	seg Code
	org $F000


Reset:
	CLEAN_START


	lda #10
	sta PlaYPos
	lda #60
	sta PlaXPos
	
	lda #83
	sta EnnemyYPos
	lda #57
	sta EnnemyXPos

	lda #%11010100
	sta Random		; #$D4

	lda #0
	sta Score
	sta Timer

	MAC DRAW_MISSILE	; Macro to know if we draw missile
		lda #%00000000
		cpx MissileYPos
		bne SkipDraw
		lda #%00000010
		inc MissileYPos
SkipDraw
		sta ENAM0
	ENDM

	lda #<PlaSprite
	sta PlaSpritePtr	; low-byte pointer
	lda #>PlaSprite
	sta PlaSpritePtr+1	; high-byte pointer

	lda #<PlaColor
        sta PlaColorPtr 
        lda #>PlaColor
        sta PlaColorPtr+1

	lda #<EnnemySprite
        sta EnnemySpritePtr 
        lda #>EnnemySprite
        sta EnnemySpritePtr+1
	
	lda #<EnnemyColor
        sta EnnemyColorPtr
        lda #>EnnemyColor
        sta EnnemyColorPtr+1


StartFrame:
	lda #2
	sta VBLANK
	sta VSYNC		; turn on VBLANK and VSYNC
	REPEAT 3
		sta WSYNC	; 3 recommended lines of VSYNC
	REPEND
	lda #0
	sta VSYNC
	REPEAT 33
		sta WSYNC
	REPEND


        lda PlaXPos
        ldy #0
        jsr SetObjectXPos

        lda EnnemyXPos
        ldy #1
        jsr SetObjectXPos

	lda MissileXPos
	ldy #2
	jsr SetObjectXPos

        jsr CalcDigitOffset     ; fetch lookup table

        sta WSYNC
        sta HMOVE

	lda #0
	sta VBLANK		; turn off VBLANK


	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1
	sta CTRLPF		; enable reflection
	sta COLUBK

	lda #$1E
	sta COLUPF


	ldx #DIGITS_HEIGHT	; x counter with 5
ScoreDigitLoop:
	ldy TensDigitOffset
	lda Digits,Y
	and #$F0		; remove graphics for ones
	sta ScoreSprite

	ldy OnesDigitOffset
	lda Digits,Y
	and #$0F		; remove graphics for tens
	ora ScoreSprite		; merge with tens sprite
	sta ScoreSprite
	sta WSYNC
	sta PF1

	ldy TensDigitOffset+1	; get left offset of timer
	lda Digits,Y
	and #$F0
	sta TimerSprite

	ldy OnesDigitOffset+1	; get ones offset of timer
	lda Digits,Y
	and #$0F
	ora TimerSprite
	sta TimerSprite

	jsr Sleep12Cycles

	sta PF1			; Update timer display

	ldy ScoreSprite
	sta WSYNC

	sty PF1
	inc TensDigitOffset
	inc TensDigitOffset+1
	inc OnesDigitOffset
	inc OnesDigitOffset+1	; inc digits for next line of data

	jsr Sleep12Cycles

	dex
	sta PF1
	bne ScoreDigitLoop

	sta WSYNC

	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	sta WSYNC
	sta WSYNC

GameVisibleLine:
	lda BgColor
	sta COLUBK
	lda PfColor
	sta COLUPF

	lda #%00000001
	sta CTRLPF		; reflect playfield

	lda #$F0
	sta PF0
	lda #$FC
	sta PF1
	lda #0
	sta PF2

	ldx #85			; X counts remaining scanlines
GameLineLoop:
	DRAW_MISSILE

AreWeInsidePlaSprite:
	txa
	sec
	sbc PlaYPos
	cmp #PLA_HEIGHT
	bcc DrawSpriteP0	; result < height -> call routine
	lda #0
DrawSpriteP0:
	clc
	adc AnimOffset
	tay			; only Y support indirect addressing
	lda (PlaSpritePtr),Y
	sta WSYNC
	sta GRP0
	lda (PlaColorPtr),Y
	sta COLUP0

AreWeInsideEnnemySprite:
        txa
        sec
        sbc EnnemyYPos
        cmp #ENNEMY_HEIGHT
        bcc DrawSpriteP1
        lda #0
DrawSpriteP1:
        tay

	lda #%00000101
	sta NUSIZ1		; stretch sprite

        lda (EnnemySpritePtr),Y
        sta WSYNC
        sta GRP1
        lda (EnnemyColorPtr),Y
        sta COLUP1

	dex
	bne GameLineLoop

	lda #0
	sta AnimOffset
	
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC	; 30 recommended lines of VBLANK
	REPEND
	lda #0
	sta VBLANK


CheckP0Up:
	lda #%00010000
	bit SWCHA
	bne CheckP0Down
	lda PlaYPos
	cmp #70			; if (playerY > 70) {
	bpl CheckP0Down		;   continue;
	inc PlaYPos		; } else { inc }
	lda #0
	sta AnimOffset

CheckP0Down:
	lda #%00100000
	bit SWCHA
	bne CheckP0Left
	lda PlaYPos
	cmp #5
	bmi CheckP0Left
	dec PlaYPos
	lda #0
	sta AnimOffset

CheckP0Left:
	lda #%01000000
	bit SWCHA
	bne CheckP0Right
	lda PlaXPos
	cmp #35
	bmi CheckP0Right
	dec PlaXPos
	lda #PLA_HEIGHT
	sta AnimOffset

CheckP0Right
	lda #%10000000
	bit SWCHA
	bne CheckButtonPressed
	lda PlaXPos
	cmp #100
	bpl CheckButtonPressed
	inc PlaXPos
	lda #PLA_HEIGHT
	sta AnimOffset

CheckButtonPressed:
	lda #%10000000
	bit INPT4
	bne NoInput
	lda PlaXPos
	clc
	adc #4
	sta MissileXPos
	lda PlaYPos
	clc
	adc #8
	sta MissileYPos

NoInput:


UpdateEnnemyPosition:
	lda EnnemyYPos
	clc
	cmp #0
	bmi ResetEnnemyPosition
	dec EnnemyYPos
	;dec EnnemyYPos
	jmp EndPositionUpdate

ResetEnnemyPosition:
	jsr GetRandomPosition

SetScoreValue:
	sed			; set decimal mode
	lda Timer
	clc
	adc #1
	sta Timer
	cld

	;inc Score
	;inc Timer

EndPositionUpdate:


CheckCollisionP0P1:
	lda #%10000000		; CXPPMM bit 7 detects P0 and P1 collision
	bit CXPPMM
	bne P0P1Collided
	jsr SetBgPfColor
	jmp CheckCollisionM0P1
P0P1Collided:
	jsr GameOver

;CheckCollisionP0PF:
;	lda #%10000000		; CXP0FB
;	bit CXP0FB
;	bne CollisionP0PF
;	jmp EndCollisionCheck
;CollisionP0PF:
;	jsr GameOver

CheckCollisionM0P1:
	lda #%10000000
	bit CXM0P
	bne M0P1Collided
	jmp EndCollisionCheck
M0P1Collided:
	sed
	lda Score
	clc
	adc #1
	sta Score
	cld
	lda #0
	sta MissileYPos

EndCollisionCheck:
	sta CXCLR


	jmp StartFrame




SetBgPfColor subroutine
	lda #$C2
	sta PfColor		; set green color
	lda #$00
	sta BgColor		; set black color
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A -> target x position
;; Y -> object type (0=player0, 1=player1, 2=missile0, 3=missile1, 4=ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
	sta WSYNC
	sec
Div15Loop:
	sbc #15
	bcs Div15Loop
	eor #7
	asl
	asl
	asl
	asl
	sta HMP0,Y
	sta RESP0,Y
	rts

GameOver subroutine
	lda #$30
	sta PfColor
	sta BgColor

	lda #0
	sta Score
	rts

GetRandomPosition subroutine
	lda Random
	asl
	eor Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random

	lsr
	lsr			; divide by 4 with 2 right shifts
	sta EnnemyXPos
	lda #30
	adc EnnemyXPos
	sta EnnemyXPos

	lda #96
	sta EnnemyYPos
	rts

CalcDigitOffset subroutine
	ldx #1			; loop counter
PrepareScoreLoop	
	lda Score,X		; load a with Timer (X = 1) or Score (X = 0)
	and #$0F		; hide 4 bits to skip tens
	sta Temp
	asl
	asl
	adc Temp
	sta OnesDigitOffset,X

	lda Score,X
	and #$F0		; hide 4 bits to remove ones
	lsr
	lsr
	sta Temp
	lsr
	lsr
	adc Temp
	sta TensDigitOffset,X

	dex
	bpl PrepareScoreLoop	; X >= 0

	rts

Sleep12Cycles subroutine
	rts			; jsr and rts takes 6 cycles, so 12 in total



Digits:
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00110011          ;  ##  ##
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###

    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###

    	.byte %00100010          ;  #   #
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #

    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01100110          ; ##  ##
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01000100          ; #   #
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###

    	.byte %01100110          ; ##  ##
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01100110          ; ##  ##

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01100110          ; ##  ##
    	.byte %01000100          ; #   #
    	.byte %01000100          ; #   #	

PlaSprite:
	.byte #%00000000
        .byte #%00000000	;$0E
	.byte #%01111100	;$0E
        .byte #%11111110	;$0E
        .byte #%11111110	;$0E
        .byte #%01111100	;$0E
        .byte #%00111000	;$0E
        .byte #%00010000	;$0E
        .byte #%00010000	;$0E

PlaSpriteTurn:
	.byte #%00000000
        .byte #%00000000	;$0E
	.byte #%00111000	;$0E
        .byte #%01111100	;$0E
        .byte #%01111100	;$0E
        .byte #%01111100	;$0E
        .byte #%00111000	;$0E
        .byte #%00010000	;$0E
        .byte #%00010000	;$0E

PlaColor:
	.byte #$0E
        .byte #$0E
	.byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E

PlaColorTurn:
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E
        .byte #$0E

EnnemySprite:
	.byte #%00000000
        .byte #%10101010	;$40
        .byte #%10101010	;$40
        .byte #%11111110	;$40
        .byte #%01111100	;$40
        .byte #%01010100	;$40
        .byte #%11111110	;$40
        .byte #%11111110	;$40
        .byte #%10000010	;$40

EnnemyColor:
	.byte #$00
        .byte #$40
        .byte #$40
        .byte #$40
        .byte #$40
        .byte #$40
        .byte #$40
        .byte #$40
        .byte #$40



	org $FFFC
	.word Reset		; 2 bytes with program reset address
	.word Reset		; 2 bytes with interruption vector

