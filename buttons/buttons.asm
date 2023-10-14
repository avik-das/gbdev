; buttons.asm - Avik Das
;
; A simple test program that shows which button (or multiple buttons)
; is currently being pressed. This functionality allows testing
; controllers on emulators.

  ; = DATA/VARIABLES ==================================================

  ; Here we set up some locations in memory to store data that will be
  ; used in the program. Typically, we will store data in the internal
  ; RAM, which gives us 8KB to work with.

SECTION "RAM",WRAM0[$c000]

  ; The buttons that are joypad pressed on the joypad. The fields are
  ; as follows:
  ;
  ;   bit 7: down
  ;   bit 6: up
  ;   bit 5: left
  ;   bit 4: right
  ;   bit 3: start
  ;   bit 2: select
  ;   bit 1: B
  ;   bit 0: A
  ;
  ; When a bit is set, the corresponding button is pressed. This
  ; structure is updated by read_joypad.
PAD   : DB

VBFLAG: DB ; whether or not we are in V-Blank

  ; Instead of directly manipulating values in the OAM during V-Blank,
  ; we store a copy of the OAM. Then, we can alter this copy at any
  ; time, not just during V-Blank, and when the OAM is indeed
  ; available, we initiate a DMA transfer from the copy to the OAM.
OAMBUF EQU $df00  ; allocate the last page in RAM for the copy
ARROWS EQU OAMBUF ; the arrows start at the first sprite

  ; = INTERRUPT HANDLERS ==============================================

  ; These are simple interrupt handlers that simply call the actual
  ; procedures responsible for taking any action. The procedures will
  ; call "reti".

SECTION "vblank",ROM0[$40]
  nop
  jp    vblank

SECTION "start",ROM0[$100]
  nop
  jp    start
  
  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
  
  DB "BUTTONS",0,0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
  DB 0   ; not a GBC catridge
  DB 0,0 ; new licensee code, high and low nibbles. not important
  DB 0   ; not SGB
  DB 0   ; catridge type, ROM only
  DB 0   ; ROM size, 256Kbit = 32KByte = 2 banks
  DB 0   ; RAM size, no RAM
  DB 1   ; destination code, non-Japanese
  DB $33 ; old licensee code, $33 => check new licensee code
  DB 0   ; mask ROM version number, usually 0
  DB 0   ; complement check. computed by rgbfix. important.
  DW 0   ; checksum. computed by rgbfix. not important.

  ; = INITIALIZATION ==================================================

start:
  nop
  di           ; disable interrupts
  ld sp, $ffff ; put the stack at the top of the RAM

init:
  call lcd_off

  call init_ram
  call copy_dma_routine

  call load_bg
  call load_obj

  call lcd_on

  ld a,%00000001 ; enable V-Blank interrupt
  ld [$ffff],a
  ei

  ; = MAIN LOOP =======================================================

loop:
  halt
  nop

  ld   a,[VBFLAG]
  or   0
  jr   z,loop
  ld   a,0
  ld   [VBFLAG],a

  call read_joypad
  call react_to_input
  jr   loop

  ; = INITIALIZATION FUNCTIONS ========================================

init_ram:
  ; initialize the RAM variables
  ld a,0
  ld [   PAD],a
  ld [VBFLAG],a

  ret

copy_dma_routine:
  ; copy the sprite DMA procedure into HRAM
  ld hl,sprite_dma
  ld de,hram_sprite_dma
  ld bc,sprite_dma_end-sprite_dma
  call memcpy

  ret

load_bg:
  ; reset the screen position
  ld a,0
  ld [$ff42],a ; scrolly will always be 0
  ld [$ff43],a ; scrollx will always be 0

  ; load the background palette
  ld  a,[bgpal]
  ld  [$ff47],a

  ; load the background tiles into the Tile Data Table
  ld hl,bgtiles ; source address
  ld de,$8800   ; destination address
  ld bc,352     ; number of bytes to copy
  call memcpy

  ; load background into Background Tile Map
  ld hl,bg
  ld de,$9800
  ld bc,1024
  call memcpy

  ret

MACRO SetUpArrow
    ; parameters:
    ;   \1 = sprite number
    ;   \2 = x-coordinate
    ;   \3 = y-coordinate
    ;   \4 = tile number
    ;   \5 = OAM attributes
SPRITE_OAM_LOCATION\@ EQU ARROWS+\1*4

  ld a,\3 ; y-coordinate
  ld [SPRITE_OAM_LOCATION\@],a
  ld a,\2 ; x-coordinate
  ld [SPRITE_OAM_LOCATION\@+1],a
  ld a,\4 ; pattern number
  ld [SPRITE_OAM_LOCATION\@+2],a
  ld a,\5
  ld [SPRITE_OAM_LOCATION\@+3],a

ENDM

load_obj:
  ; load the object palette 0
  ld a,[sppal] ; load the object palette 0 data
  ld [$ff48],a ; and store it into the object palette 0 register

  ; load the object palette 1
  ld a,[sppal+1] ; load the object palette 1 data
  ld [$ff49],a   ; and store it into the object palette 1 register

  ; zero out the OAM buffer
  ld de,OAMBUF
  ld bc,256
  call zeromem

  ; load the sprite tiles into the Sprite Pattern Table
  ld hl,arrows ; source address
  ld de,$8000  ; destination address
  ld bc,32     ; number of bytes to copy
  call memcpy

  ; Display the sprites on the screen by populating the Object
  ; Attribute Memory (OAM). Note that the actual Y-coordinate on the
  ; screen is the stored coordinate minus 16, and the actual X-
  ; coordinate is the stored coordinate minus 8.

  ; Initialize all the arrows
  SetUpArrow 0,  40,  56, 0, %01000000 ; Up D-Pad Button
  SetUpArrow 1,  24,  72, 1, %00000000 ; Left D-Pad Button
  SetUpArrow 2,  56,  72, 1, %00100000 ; Right D-Pad Button
  SetUpArrow 3,  40,  88, 0, %00000000 ; Down D-Pad Button
  SetUpArrow 4, 108,  88, 0, %00000000 ; B Button
  SetUpArrow 5, 132,  80, 0, %00000000 ; A Button
  SetUpArrow 6,  68, 126, 0, %00000000 ; Select Button
  SetUpArrow 7,  84, 126, 0, %00000000 ; Select Button

  ret

  ; = INTERRUPT HANDLERS ==============================================

vblank:
  push af
  push bc
  push de
  push hl

  ; Note that the DMA procedure must be initiated from High RAM. The
  ; mechanism for that is detailed alongside the definition of this
  ; initiation procedure.
  call hram_sprite_dma

  ld   a,1
  ld   [VBFLAG],a

  pop  hl
  pop  de
  pop  bc
  pop  af
  reti

  ; = MAIN LOOP FUNCTIONS =============================================

read_joypad:
  ; First, we will read the direction pad by sending a value to the
  ; joypad register (P1) that will enable the d-pad (by clearing bit 4)
  ; and disable the buttons (by setting bit 5).
  ld a,%00100000
  ld [$ff00],a

  ; To minimize the effects of key bouncing, in which the contacts of
  ; the joypad cause oscillations between the high and low states, we
  ; read from P1 multiple times and only use the last value read.
  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]

  and  %00001111 ; pick out only the input lines...
  swap a         ; ...put the states into the high nibble...
  ld b,a         ; ...and save it away temporarily

  ; Now we want to read the buttons, and that means we disable the d-pad
  ; (by setting bit 4) and enable the buttons (by clearing bit 5).
  ld a,%00010000
  ld [$ff00],a

  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]

  and %00001111
  or  b         ; B contains the d-pad input in the high nibble, so we
                ; can we incorporate the button input in the low nibble

  ; Now, A contains the state of the d-pad and the buttons, but when a
  ; bit is cleared, that means the button is pressed. So, we take the
  ; complement, and we have an intuitive mapping of 1->pressed and
  ; 0->not pressed.
  cpl
  ld  [PAD],a

  ret

MACRO ReactToSingleKey
    ; parameters
    ;   \1 = arrow sprite number
    ;   \2 = bit of key being checked
    ;   \3 = label of next key reaction code block
SPRITE_ATTRIBUTE_LOCATION\@ EQU ARROWS+\1*4+3

  ld  a,[PAD]
  bit \2,a
  jp  z,.hide\@

  ld  a,[SPRITE_ATTRIBUTE_LOCATION\@]
  or  a,%00010000
  ld  [SPRITE_ATTRIBUTE_LOCATION\@],a
  jp  \3

.hide\@:
  ld  a,[SPRITE_ATTRIBUTE_LOCATION\@]
  and a,%11101111
  ld  [SPRITE_ATTRIBUTE_LOCATION\@],a

ENDM

react_to_input:
.react_to_up    : ReactToSingleKey 0, 6, .react_to_left
.react_to_left  : ReactToSingleKey 1, 5, .react_to_right
.react_to_right : ReactToSingleKey 2, 4, .react_to_down
.react_to_down  : ReactToSingleKey 3, 7, .react_to_b
.react_to_b     : ReactToSingleKey 4, 1, .react_to_a
.react_to_a     : ReactToSingleKey 5, 0, .react_to_select
.react_to_select: ReactToSingleKey 6, 2, .react_to_start
.react_to_start : ReactToSingleKey 7, 3, .react_done
.react_done     : ret

  ; = UTILITY FUNCTIONS ===============================================

memcpy:
  ; parameters:
  ;   hl = source address
  ;   de = destination address
  ;   bc = number of bytes to copy
  ; assumes:
  ;   bc > 0
  ld a,[hl] ; load a byte from the source
  ld [de],a ; store it in the destination
  inc hl    ; prepare to load another byte...
  inc de    ; ...and prepare to write it

  ; Now we'll decrement the counter and check if it's zero.
  ; Unfortunately, when a 16-bit register is decremented, the zero flag
  ; is not updated, so we need to check if the counter is zero manually.
  ; A 16-bit value is zero when its constituent 8-bit portions are both
  ; zero, that is when (b | c) == 0, where "|" is a bitwise OR.
  dec bc    ; decrement the counter
  ld  a,b
  or  c
  ret z     ; return if all bytes written

  jr memcpy

zeromem:
    ; parameters
    ;   de = destination address
    ;   bc = number of bytes to zero out
    ; assumes:
    ;   bc > 0
.zeromem_loop:
  ld a,0    ; we will only be writing zeros
  ld [de],a ; store one byte in the destination
  inc de    ; prepare to write another byte

  ; the same caveat applies as in memcpy
  dec bc    ; decrement the counter
  ld  a,b
  or  c
  ret z     ; return if all bytes written

  jr .zeromem_loop

lcd_off:
  ld  a,[$ff40] ; load the LCD Control register
  bit 7,a       ; check bit 7, whether the LCD is on
  ret z         ; if off, return immediately

wait_vblank:
  ld a,[$ff44]  ; load the vertical line the LCD is at
  cp 145
  jr nz, wait_vblank

  ; Technically, at this point, we are not just at VBlank, but exactly
  ; at the start of VBlank. This is because the previous instruction
  ; made sure that [LY] is exactly 145, the first value when entering
  ; VBlank.

  ld  a,[$ff40] ; load the LCD Control register
  res 7,a       ; clear bit 7, turning off the LCD
  ld  [$ff40],a ; write the new value back to the LCD Control register

  ret ; and done...

lcd_on:
  ld a,%10000011 ; LCD on
                 ; Ignored: Window Tile Map location
                 ; window off
                 ; BG & window Tile Data at $8800-$97ff
                 ; BG Tile Map at $9800-$9bff
                 ; sprite size 8x16
                 ; sprites on
                 ; BG on
  ld [$ff40],a   ; write it to the LCD Control register

  ret

  ; During the DMA transfer, the CPU can only access the High RAM
  ; (HRAM), so the transfer must be initiated from inside of HRAM.
  ; However, we can't directly place this procedure there at assembly
  ; time, so we'll put it here with the rest of the code, then copy it
  ; into HRAM at run time.
sprite_dma:
  ld  a,OAMBUF/$100
  ld  [$ff46],a
  ld  a,$28
.wait:
  dec a
  jr  nz,.wait
  ret
sprite_dma_end:

  ; We'll set aside some space in HRAM, but we'll have to store the
  ; actual data here at run time.
PUSHS
SECTION "HRAM",HRAM

 ; This is the procedure that will actually be called to initiate the
 ; DMA transfer.
hram_sprite_dma:
  DS sprite_dma_end-sprite_dma

POPS

  ; = DATA ============================================================

bgpal:
  DB %11100100 ; all colors, in order of decreasing darkness
               ; white is transparent
sppal:
  DB %00000000 ; all white - used for "hidden" sprite
  DB %11100100 ; all colors, in order of decreasing darkness

bgtiles:
  ; Clear - 1 tile
  DB $00,$00,$00,$00,$00,$00,$00,$00
  DB $00,$00,$00,$00,$00,$00,$00,$00

  ; D-Pad - 5 tiles
  DB $7e,$7e,$c3,$ff,$bf,$ff,$c3,$ff ; up
  DB $bf,$ff,$c3,$ff,$bf,$ff,$ff,$ff
  DB $7f,$7f,$d5,$ff,$ab,$ff,$ab,$ff ; left
  DB $ab,$ff,$ab,$ff,$ff,$ff,$7f,$7f
  DB $ff,$ff,$ff,$ff,$fd,$ff,$fc,$ff ; center
  DB $fc,$ff,$f9,$ff,$c3,$ff,$e7,$ff
  DB $fe,$fe,$d4,$fe,$ab,$ff,$ab,$ff ; right
  DB $ab,$ff,$ab,$ff,$ff,$ff,$fe,$fe
  DB $ff,$ff,$c3,$ff,$bf,$ff,$c3,$ff ; bottom
  DB $bf,$ff,$c3,$ff,$bf,$ff,$7e,$7e

  ; B Button - 4 tiles
  DB $00,$03,$00,$0f,$07,$1f,$0f,$3f ; top-left
  DB $1e,$7f,$3c,$7e,$39,$fd,$39,$fd
  DB $00,$c0,$70,$f0,$f8,$f8,$fc,$fc ; top-right
  DB $7e,$fe,$7e,$7e,$3f,$bf,$7f,$7f
  DB $39,$fd,$78,$fd,$7e,$7e,$7f,$7f ; bottom-left
  DB $3f,$3f,$1f,$1f,$0f,$0f,$03,$03
  DB $3f,$bf,$3f,$bf,$7e,$7e,$fe,$fe ; bottom-right
  DB $fc,$fc,$f8,$f8,$f0,$f0,$c0,$c0

  ; A Button - 4 tiles
  DB $00,$03,$00,$0f,$07,$1f,$0f,$3f ; top-left
  DB $1e,$7f,$3e,$7e,$39,$fd,$38,$fd
  DB $00,$c0,$70,$f0,$f8,$f8,$fc,$fc ; top-right
  DB $7e,$fe,$7e,$7e,$3f,$bf,$3f,$bf
  DB $38,$fc,$79,$fd,$7d,$7d,$7f,$7f ; bottom-left
  DB $3f,$3f,$1f,$1f,$0f,$0f,$03,$03
  DB $3f,$3f,$3f,$bf,$3e,$be,$fe,$fe ; bottom-right
  DB $fc,$fc,$f8,$f8,$f0,$f0,$c0,$c0

  ; Start/Select Buttons - 4 tiles
  DB $00,$00,$00,$00,$00,$00,$00,$00 ; top-left
  DB $00,$00,$00,$00,$00,$01,$01,$03
  DB $00,$00,$00,$00,$00,$00,$00,$30 ; top-right
  DB $3c,$78,$7c,$f8,$f8,$f0,$f0,$e0
  DB $03,$07,$07,$0f,$0f,$1f,$1f,$1e ; bottom-left
  DB $1e,$0c,$0c,$00,$00,$00,$00,$00
  DB $e0,$c0,$c0,$80,$80,$00,$00,$00 ; bottom-right
  DB $00,$00,$00,$00,$00,$00,$00,$00

  ; Select Text - 2 tiles
  DB $00,$00,$18,$18,$21,$21,$19,$19 ; left
  DB $05,$05,$18,$18,$00,$00,$00,$00
  DB $00,$00,$d0,$d0,$10,$10,$90,$90 ; right
  DB $10,$10,$cc,$cc,$00,$00,$00,$00

  ; Start Text - 2 tiles
  DB $00,$00,$19,$19,$20,$20,$18,$18 ; left
  DB $04,$04,$18,$18,$00,$00,$00,$00
  DB $00,$00,$c8,$c8,$94,$94,$98,$98 ; right
  DB $94,$94,$94,$94,$00,$00,$00,$00

arrows:
  ; arrows
  ;
  ; There are 2 unique arrows, each consisting of 1 tile. Four arrows
  ; can be used in the program by flipping these tiles horizontally or
  ; vertically. Note that the flipping of tiles means the images cannot
  ; have direction-sensitive highlights or shadows.
  DB $00,$00,$00,$00,$00,$00,$18,$18 ; pointing up
  DB $3c,$3c,$7e,$7e,$3c,$3c,$3c,$3c
  DB $00,$00,$20,$20,$f0,$f0,$f8,$f8 ; pointing right
  DB $f8,$f8,$f0,$f0,$20,$20,$00,$00

bg:
  ; define one map's worth:
  ;   256 x 256 pixels = 
  ;    32 x  32 tiles  = 1024 tiles
  ; at 16 tiles per line of code, that's 64 lines
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$81,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$8a
  DB $8b,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$82,$83,$84,$80,$80,$80,$80,$80,$80,$86,$87,$80,$8c
  DB $8d,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$85,$80,$80,$80,$80,$80,$80,$80,$88,$89,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$8e,$8f,$8e,$8f,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$90,$91,$90,$91,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$92,$93,$94,$95,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
  DB $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80

; vim: ft=rgbasm:tw=72:ts=2:sw=2
