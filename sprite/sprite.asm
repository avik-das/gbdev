; sprite.asm - Avik Das
;
; A very simple demo, based on the Game Boy Programming Tutorial by
; David Pello available at
; http://wiki.ladecadence.net/doku.php?id=tutorial_de_ensamblador.
; This demo can be assembled using RGBDS. The resulting output should
; be viewable in any compliant Game Boy emulator.
;
; This demo serves two purposes. Firstly, my goal is to learn Game Boy
; development, which I can only achieve by creating a program.
; Secondly, the entirety of the program will be in one file, in order
; to show at a glance all the different parts of the program.

  ; = DATA/VARIABLES ==================================================

  ; Here we set up some locations in memory to store data that will be
  ; used in the program. Typically, we will store data in the internal
  ; RAM, which 8KB to work with.

SECTION "RAM",BSS[$c000]

  ; The buttons that are joypad pressed on the joypad. The fields are
  ; as follows:
  ;   bit 7: down
  ;   bit 6: up
  ;   bit 5: left
  ;   bit 4: right
  ;   bit 3: start
  ;   bit 2: select
  ;   bit 1: B
  ;   bit 0: A
  ; When a bit is set, the corresponding button is pressed. This
  ; structure is updated by read_joypad.
PAD   : DB
OLDPAD: DB

MOVED : DB ; whether or not the player has moved
ANIFRM: DB ; the current frame of animation

WNDWON: DB ; whether or not the window is visible
BGSCRL: DB ; amount to scroll the background by

VBFLAG: DB ; whether or not we are in V-Blank

  ; Instead of directly manipulating values in the OAM during V-Blank,
  ; we store a copy of the OAM. Then, we can alter this copy at any
  ; time, not just during V-Blank, and when the OAM is indeed
  ; available, we initiate a DMA transfer from the copy to the OAM.
OAMBUF EQU $df00  ; allocate the last page in RAM for the copy
PLAYER EQU OAMBUF ; the player starts at the first sprite

  ; = INTERRUPT HANDLERS ==============================================

  ; These are simple interrupt handlers that simply call the actual
  ; procedures responsible for taking any action. The procedures will
  ; call "reti".

SECTION "vblank",HOME[$40]
  nop
  jp    vblank

SECTION "timer" ,HOME[$50]
  nop
  jp    timer

SECTION "start",HOME[$100]
  nop
  jp    start
  
  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
  
  DB "AVIKTEST",0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
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
  DB 0   ; checksum. computed by rgbfix. not important.

  ; = INITIALIZATION ==================================================

start:
  nop
  di           ; disable interrupts
  ld sp, $ffff ; put the stack at the top of the RAM

init:
  call lcd_off

  ; no need to load a background palette now because every V-Blank, the
  ; background palette is reset to the appropriate palette depending on
  ; whether or not the window is visible

  ; load the object palette 0
  ld a,[sppal] ; load the object palette 0 data
  ld [$ff48],a ; and store it into the object palette 0 register

  ; load the object palette 1
  ld a,[sppal+1] ; load the object palette 1 data
  ld [$ff49],a   ; and store it into the object palette 1 register

  ; reset the screen position
  ld a,0
  ld [$ff43],a ; scrollx will always be 0
  ld [BGSCRL],a
  call scroll_bg

  ; load the background tiles into the Tile Data Table
  ld hl,cloud  ; source address
  ld de,$8800  ; destination address
  ld bc,96     ; number of bytes to copy
  call memcpy

  ; don't forget to load the window tiles, which will be placed right
  ; after the background tiles, since the background and the window
  ; share the same Tile Data Table
  ld hl,rain   ; source address, destination address is retained from
  ld bc,48     ; the previous call to memcpy
  call memcpy

  ; load background into Background Tile Map
  ld hl,bg
  ld de,$9800
  ld bc,1024
  call memcpy

  ; load window into the Window Tile Map
  ld hl,window
  ld de,$9c00
  ld bc,1024
  call memcpy

  ld a,63
  ld [$ff4a],a
  ld a,7
  ld [$ff4b],a

  ; zero out the OAM buffer
  ld de,OAMBUF
  ld bc,256
  call zeromem

  ; load the sprite tiles into the Sprite Pattern Table
  ld hl,ghost ; source address
  ld de,$8000 ; destination address
  ld bc,256   ; number of bytes to copy
  call memcpy

  ; display the sprites on the screen by populating the Object
  ; Attribute Memory (OAM). Note that the actual Y-coordinate on the
  ; screen is the stored coordinate minus 16, and the actual X-
  ; coordinate is the stored coordinate minus 8.
  ld a,80 ; y-coordinate
  ld [PLAYER],a
  ld a,80 ; x-coordinate
  ld [PLAYER+1],a
  ld a, 0 ; pattern number
  ld [PLAYER+2],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [PLAYER+3],a

  ld a,80 ; y-coordinate
  ld [PLAYER+4],a
  ld a,88 ; x-coordinate
  ld [PLAYER+5],a
  ld a, 2 ; pattern number
  ld [PLAYER+6],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [PLAYER+7],a

  ld a,%11000111 ; LCD on
                 ; Window Tile Map at $9800-9fff
                 ; window off (for now)
                 ; BG & window Tile Data at $8800-$97ff
                 ; BG Tile Map at $9800-$9bff
                 ; sprite size 8x16
                 ; sprites on
                 ; BG & window on
  ld [$ff40],a   ; write it to the LCD Control register

  ; initialize the RAM variables
  ld a,0
  ld [   PAD],a
  ld [OLDPAD],a
  ld [ MOVED],a
  ld [ANIFRM],a
  ld [WNDWON],a
  ld [VBFLAG],a

  ; copy the sprite DMA procedure into HRAM
  ld hl,sprite_dma
  ld de,hram_sprite_dma
  ld bc,sprite_dma_end-sprite_dma
  call memcpy

  ; start the timer
  ld a,-128
  ld [$ff06],a
  ld a,%00000100
  ld [$ff07],a

  ; = MAIN LOOP =======================================================

  ld a,%00000101 ; enable V-Blank interrupt
                 ; enable timer   interrupt
  ld [$ffff],a
  ei

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
  call animate_sprite
  jr   loop

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
  call show_window
  call scroll_bg

  ld   a,1
  ld   [VBFLAG],a

  pop  hl
  pop  de
  pop  bc
  pop  af
  reti

timer:
  push af
  push bc
  push de
  push hl

  ld  a,[BGSCRL]
  inc a
  ld  [BGSCRL],a

.done:
  pop  hl
  pop  de
  pop  bc
  pop  af
  reti

  ; = MAIN LOOP FUNCTIONS =============================================

read_joypad:
  ; The state that was current before should now become the old state.
  ld  a,[PAD]
  ld  [OLDPAD],a

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

react_to_input:
  ld  a,0
  ld  [MOVED],a ; hasn't moved

  ld  a,[PAD]
  bit 4,a
  jp  z,.move_left

  ld  a,[PLAYER+1]
  cp  152
  jp  z,.move_left

  add 2
  ld  [PLAYER+1],a
  ld  a,[PLAYER+5]
  add 2
  ld  [PLAYER+5],a

  ld  a,1
  ld  [MOVED],a ; has moved

.move_left :
  ld  a,[PAD]
  bit 5,a
  jp  z,.move_up

  ld  a,[PLAYER+1]
  cp  8
  jp  z,.move_up

  sub 2
  ld  [PLAYER+1],a
  ld  a,[PLAYER+5]
  sub 2
  ld  [PLAYER+5],a

  ld  a,1
  ld  [MOVED],a ; has moved

.move_up   :
  ld  a,[PAD]
  bit 6,a
  jp  z,.move_down

  ld  a,[PLAYER]
  cp  16
  jp  z,.move_down

  sub 2
  ld  [PLAYER],a
  ld  a,[PLAYER+4]
  sub 2
  ld  [PLAYER+4],a

  ld  a,1
  ld  [MOVED],a ; has moved

.move_down :
  ld  a,[PAD]
  bit 7,a
  jp  z,.switch_palette

  ld  a,[PLAYER]
  cp  144
  jp  z,.switch_palette

  add 2
  ld  [PLAYER],a
  ld  a,[PLAYER+4]
  add 2
  ld  [PLAYER+4],a

  ld  a,1
  ld  [MOVED],a ; has moved

.switch_palette:
  ld  a,[PAD]
  bit 0,a
  jp  z,.flip_sprite

  ld  a,[OLDPAD]
  bit 0,a
  jp  nz,.flip_sprite ; only switch palettes if A wasn't pressed before

  ld  a,[PLAYER+3]
  xor %00010000
  ld  [PLAYER+3],a
  ld  a,[PLAYER+7]
  xor %00010000
  ld  [PLAYER+7],a

.flip_sprite:
  ld  a,[PAD]
  bit 1,a
  jp  z,.switch_bg_prio

  ld  a,[OLDPAD]
  bit 1,a
  jp  nz,.switch_bg_prio ; only flip if B wasn't pressed before

  ld  a,[PLAYER+3]
  xor %01000000
  ld  [PLAYER+3],a
  ld  a,[PLAYER+7]
  xor %01000000
  ld  [PLAYER+7],a

.switch_bg_prio:
  ld  a,[PAD]
  bit 2,a
  jp  z,.toggle_window

  ld  a,[OLDPAD]
  bit 2,a
  jp  nz,.toggle_window ; only switch if SEL wasn't pressed before

  ld  a,[PLAYER+3]
  xor %10000000
  ld  [PLAYER+3],a
  ld  a,[PLAYER+7]
  xor %10000000
  ld  [PLAYER+7],a

.toggle_window:
  ld  a,[PAD]
  bit 3,a
  jp  z,.move_return

  ld  a,[OLDPAD]
  bit 3,a
  jp  nz,.move_return ; only toggle if START wasn't pressed before

  ld  a,[WNDWON]
  xor 1
  ld  [WNDWON],a

.move_return
  ret

animate_sprite:
  ld  a,[ANIFRM]
  cp  0
  jp  nz,.inc_anifrm
  ld  a,[ MOVED]
  cp  0
  jp  nz,.inc_anifrm
  jp  .ani_return
.inc_anifrm:
  ld  a,[ANIFRM]
  inc a
  and %00111111
  ld  [ANIFRM],a
  ld  a,[ANIFRM]
  and %00001111
  jp  nz,.ani_return
  ld  a,[PLAYER+2]
  add 4
  and %00001111
  ld  [PLAYER+2],a
  add 2
  and %00001111
  ld  [PLAYER+6],a
.ani_return:
  ret

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
  ld a,0   ; we will only be writing zeros
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

show_window:
  ld  a,[WNDWON]
  cp  0
  jp  z,.hide

  ld  a,[$ff40]
  set 5,a       ; set the window visibility bit of LCDC
  ld  [$ff40],a
  ld  a,[wnpal] ; use the correct palette for the window
  ld  [$ff47],a
  jp  .done

.hide:
  ld  a,[$ff40]
  res 5,a       ; clear the window visibility bit of LCDC
  ld  [$ff40],a
  ld  a,[bgpal] ; use the correct palette for the background
  ld  [$ff47],a

.done:
  ret

scroll_bg:
  ld a,[BGSCRL]
  ld [$ff42],a ; set scrolly
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
  DB %00000100 ; white is transparent, with another non-transparent
               ; white. The only other important color is the second-
               ; lightest color.
wnpal:
  DB %00011001 ; light gray is transparent, with the only non-
               ; transparent color being dark gray.

sppal:
  DB %00011100 ; missing second darkest
  DB %11100000 ; missing second lightest

cloud:
  ; background cloud, 3x2 tiles
  DB $00,$00,$00,$00,$00,$00,$03,$00 ; tile 0
  DB $04,$03,$0b,$04,$17,$08,$17,$08
  DB $00,$00,$1c,$00,$22,$1c,$41,$3e ; tile 1
  DB $a8,$57,$55,$aa,$ea,$15,$d4,$2b
  DB $00,$00,$00,$00,$00,$00,$00,$00 ; tile 2
  DB $e0,$00,$10,$e0,$08,$f0,$08,$f0
  DB $0b,$04,$17,$08,$2f,$10,$2f,$10 ; tile 3
  DB $27,$18,$10,$0f,$0f,$00,$00,$00
  DB $e0,$1f,$d2,$2d,$a8,$57,$d5,$2a ; tile 4
  DB $08,$f7,$00,$ff,$ff,$00,$00,$00
  DB $88,$70,$08,$f0,$a4,$58,$04,$f8 ; tile 5
  DB $04,$f8,$08,$f0,$f0,$00,$00,$00

rain:
  ; window rain, 3x1 tiles
  DB $0a,$04,$0a,$04,$0a,$04,$0a,$04 ; tile 0
  DB $0a,$04,$0a,$04,$0a,$04,$0a,$04
  DB $28,$10,$28,$10,$28,$10,$28,$10 ; tile 1
  DB $28,$10,$28,$10,$28,$10,$28,$10
  DB $a0,$40,$a0,$40,$a0,$40,$a0,$40 ; tile 2
  DB $a0,$40,$a0,$40,$a0,$40,$a0,$40

ghost:
  ; foreground ghost
  ;
  ; There are a total of 4 tiles laid out in a 2x2 grid, but because
  ; the sprites used are 8x16 pixels in dimension, only two sprites are
  ; required.
  ;
  ; Furthermore, there are four frames of animation, so a total of 16
  ; tiles, or 8 sprites are needed.

  ; frame 0
  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; sprite 0
  DB $23,$1d,$27,$19,$27,$19,$61,$1f
  DB $ab,$55,$a5,$5a,$a1,$5f,$61,$1f
  DB $21,$1f,$20,$1f,$26,$19,$19,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; sprite 1
  DB $fc,$d8,$fc,$98,$fc,$98,$fe,$f8
  DB $ff,$5a,$ff,$aa,$ff,$fa,$fe,$f8
  DB $fc,$f8,$fc,$f8,$7c,$98,$98,$00

  ; frame 1
  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; sprite 0
  DB $23,$1d,$67,$19,$a7,$59,$a1,$5f
  DB $ab,$55,$65,$1a,$21,$1f,$21,$1f
  DB $24,$1b,$1a,$01,$01,$00,$00,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; sprite 1
  DB $fc,$d8,$fc,$98,$fc,$98,$fc,$f8
  DB $fc,$58,$fe,$a8,$ff,$fa,$ff,$fa
  DB $ff,$fa,$7e,$b8,$bc,$18,$18,$00

  ; frame 2
  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; sprite 0
  DB $23,$1d,$27,$19,$27,$19,$61,$1f
  DB $ab,$55,$a5,$5a,$a1,$5f,$61,$1f
  DB $21,$1f,$20,$1f,$26,$19,$19,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; sprite 1
  DB $fc,$d8,$fc,$98,$fc,$98,$fe,$f8
  DB $ff,$5a,$ff,$aa,$ff,$fa,$fe,$f8
  DB $fc,$f8,$fc,$f8,$7c,$98,$98,$00

  ; frame 3
  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; sprite 0
  DB $23,$1d,$27,$19,$27,$19,$21,$1f
  DB $2b,$15,$65,$1a,$a1,$5f,$a1,$5f
  DB $a0,$5f,$62,$1d,$25,$18,$18,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; sprite 1
  DB $fc,$d8,$fe,$98,$ff,$9a,$ff,$fa
  DB $ff,$5a,$fe,$a8,$fc,$f8,$fc,$f8
  DB $fc,$d8,$58,$80,$80,$00,$00,$00

bg:
  ; define one map's worth:
  ;   256 x 256 pixels = 
  ;    32 x  32 tiles  = 1024 tiles
  ; at 16 tiles per line of code, that's 64 lines
  ; TODO: is there a better way to specify this?
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84
  DB $80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80
  DB $81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81,$82,$80,$81
  DB $83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83
  DB $84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84,$85,$83,$84

window:
  ; define one map's worth, again 1024 tiles = 64 lines
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87
  DB $86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86
  DB $87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87,$88,$86,$87

; vim: ft=rgbasm:tw=72:ts=2:sw=2
