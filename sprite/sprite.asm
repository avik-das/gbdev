; sprite.agb - Avik Das
;
; A very simple demo, based on the Game Boy Programming Tutorial by
; David Pello available at
; http://wiki.ladecadence.net/doku.php?id=tutorial_de_ensamblador.
; This demo can be assembled using RGBDS. The resulting output should
; be viewable in any compliant Game Boy emulator, though the Linux
; version of VisualBoy Advance seems to fail (the Windows version runs
; the demo perfectly). The demo has been tested on mednafen as well.
;
; This demo serves two purposes. Firstly, my goal is to learn Game Boy
; development, which I can only achieve by creating a program.
; Secondly, the entirety of the program will be in one file, in order
; to show at a glance all the different parts of the program.

SECTION "start",HOME[$100]
  nop
  jp    start
  
  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
  
  DB "AVIKTEST",0,0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
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

  ; load the background palette
  ld a,[bgpal] ; load the background palette data
  ld [$ff47],a ; and store it into the background palette register

  ; load the object palette 0
  ld a,[sppal] ; load the object palette 0 data
  ld [$ff48],a ; and store it into the object palette 0 register

  ; reset the screen position
  ld a,0
  ld [$ff42],a ; set scrolly = 0
  ld [$ff43],a ; set scrollx = 0

  ; load the background tiles into the Tile Data Table
  ld hl,cloud  ; source address
  ld de,$8800  ; destination address
  ld b,96      ; number of bytes to copy
  call memcpy

  ; load background into Background Tile Map, all 1024 bytes.
  ; the 8-bit counter can't hold 1024, so we have to perform the copy
  ; over multiple calls.
  ld hl,bg
  ld de,$9800
  ld b,255
  call memcpy
  ld b,255
  call memcpy
  ld b,255
  call memcpy
  ld b,255
  call memcpy
  ld b,4
  call memcpy

  ; load the sprite tiles into the Sprite Pattern Table
  ld hl,ghost ; source address
  ld de,$8000 ; destination address
  ld b,64     ; number of bytes to copy
  call memcpy

  ; display the sprites on the screen by populating the Object
  ; Attribute Memory (OAM)
  ld a,16 ; y-coordinate
  ld [$FE00],a
  ld a, 8 ; x-coordinate
  ld [$FE01],a
  ld a, 0 ; pattern number
  ld [$FE02],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [$FE03],a

  ld a,16 ; y-coordinate
  ld [$FE04],a
  ld a,16 ; x-coordinate
  ld [$FE05],a
  ld a, 2 ; pattern number
  ld [$FE06],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [$FE07],a

  ld a,%10000111 ; LCD on
                 ; Window Tile Map at $9800-9bff (not important)
                 ; window off
                 ; BG & window Tile Data at $8800-$97ff
                 ; BG Tile Map at $9800-$9bff
                 ; sprite size 8x16
                 ; sprites on
                 ; BG & window on
  ld [$ff40],a   ; write it to the LCD Control register

  ; = MAIN LOOP =======================================================

loop:
  halt
  nop
  jr   loop

  ; = UTILITY FUNCTIONS ===============================================

memcpy:
  ; parameters:
  ;   hl = source address
  ;   de = destination address
  ;   b  = number of bytes to copy
  ; assumes:
  ;   b  > 0
  ld a,[hl] ; load a byte from the source
  ld [de],a ; store it in the destination
  inc hl    ; prepare to load another byte...
  inc de    ; ...and prepare to write it
  dec b     ; decrement the counter
  ret z     ; return if all bytes written
  jr memcpy

lcd_off:
  ld  a,[$ff40] ; load the LCD Control register
  bit 7,a       ; check bit 7, whether the LCD is on
  ret nz        ; if off, return immediately

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

  ; = DATA ============================================================

bgpal:
  DB %11100100 ; black to white

sppal:
  DB %00011100 ; missing second darkest

cloud:
  ; background cloud, 3x2 tiles
  DB $00,$00,$00,$00,$00,$00,$03,$00 ; tile 0
  DB $04,$00,$0b,$00,$17,$00,$17,$00
  DB $00,$00,$1c,$00,$22,$00,$41,$00 ; tile 1
  DB $a8,$00,$55,$00,$ea,$00,$d4,$00
  DB $00,$00,$00,$00,$00,$00,$00,$00 ; tile 2
  DB $e0,$00,$10,$00,$08,$00,$08,$00
  DB $0b,$00,$17,$00,$2f,$00,$2f,$00 ; tile 3
  DB $27,$00,$10,$00,$0f,$00,$00,$00
  DB $e8,$00,$d2,$00,$a8,$00,$d5,$00 ; tile 4
  DB $a8,$00,$00,$00,$ff,$00,$00,$00
  DB $88,$00,$08,$00,$a4,$00,$04,$00 ; tile 5
  DB $04,$00,$08,$00,$f0,$00,$00,$00
  ; TODO: rest of the tiles

ghost:
  ; foreground ghost, 2x2 tiles
  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; tile 0
  DB $23,$1d,$27,$19,$27,$19,$61,$1f
  DB $ab,$55,$a5,$5a,$a1,$5f,$61,$1f
  DB $21,$1f,$20,$1f,$26,$19,$19,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; tile 1
  DB $fc,$d8,$fc,$98,$fc,$98,$fe,$f8
  DB $ff,$5a,$ff,$aa,$ff,$fa,$fe,$f8
  DB $fc,$f8,$fc,$f8,$7c,$98,$98,$00
  ; TODO: rest of the tiles

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

; vim: ft=rgbasm:tw=72:sw=2
