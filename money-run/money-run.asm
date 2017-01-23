; money-run.asm - Avik Das
;
; A simple side-scroller, in which the character is asked to go from
; left to right, navigating over a series procedurally generated
; columns.
;
; The name is refers to the gameplay mechanic in which the player will
; be trying to collect money while avoiding getting stuck amongst the
; columns.

  ; = DATA/VARIABLES ==================================================

  ; Here we set up some locations in memory to store data that will be
  ; used in the program. Typically, we will store data in the internal
  ; RAM, which gives us 8KB to work with.

SECTION "RAM",BSS[$c000]

VBFLAG: DB ; whether or not we are in V-Blank

  ; = INTERRUPT HANDLERS ==============================================

  ; These are simple interrupt handlers that simply call the actual
  ; procedures responsible for taking any action. The procedures will
  ; call "reti".

SECTION "vblank",HOME[$40]
  nop
  jp    vblank

SECTION "start",HOME[$100]
  nop
  jp    start

  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

  DB "AVIKDAS",0,0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
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
  call load_bg

  call lcd_on
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

  ; DO STUFF

  jr   loop

  ; = INITIALIZATION FUNCTIONS ========================================

init_ram:
  ; initialize the RAM variables
  ld a,0
  ld [VBFLAG],a

  ret

load_bg:
  ld a,[bgpal] ; set the background palette
  ld [$ff47],a

  ; reset the screen position
  ld a,0
  ld [$ff42],a ; scrolly will always be 0
  ld [$ff43],a ; scrollx will always be 0 (TODO: not in the future)

  ; load the background tiles into the Tile Data Table
  ld hl,bgbox  ; source address
  ld de,$8800  ; destination address
  ld bc,96     ; number of bytes to copy
  call memcpy

  ; load background into Background Tile Map
  ld hl,bg
  ld de,$9800
  ld bc,1024
  call memcpy

  ret

  ; = INTERRUPT HANDLERS ==============================================

vblank:
  reti

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
  ld a,%11000111 ; LCD on
                 ; Window Tile Map at $9800-9fff
                 ; window off
                 ; BG & window Tile Data at $8800-$97ff
                 ; BG Tile Map at $9800-$9bff
                 ; sprite size 8x16
                 ; sprites on
                 ; BG on
  ld [$ff40],a   ; write it to the LCD Control register

  ret

  ; = DATA ============================================================

bgpal:
  DB %11100100 ; white is transparent, lightest to darkest.

bgbox:
  ; background box, 2x2 tiles
  DB $7f,$7f,$9f,$f0,$8f,$f8,$c7,$fc
  DB $e3,$be,$f1,$9f,$f8,$8f,$fc,$87
  DB $fe,$fe,$f9,$0f,$f1,$1f,$e3,$3f
  DB $c5,$7f,$8b,$fd,$97,$f9,$6f,$f1
  DB $fe,$87,$f9,$8f,$f1,$9f,$e2,$bf
  DB $c5,$fe,$8b,$fc,$97,$f8,$7f,$7f
  DB $3f,$e1,$1f,$f1,$8f,$f9,$c7,$7d
  DB $e3,$3f,$f1,$1f,$f9,$0f,$fe,$fe

bgsky:
  ; background sky, 1x1 tile
  DB $00,$00,$00,$00,$00,$00,$00,$00
  DB $00,$00,$00,$00,$00,$00,$00,$00

bg:
  ; define one map's worth:
  ;   256 x 256 pixels =
  ;    32 x  32 tiles  = 1024 tiles
  ; at 16 tiles per line of code, that's 64 lines
  ; TODO: generate this based on some height data
  DB $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
  DB $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
  DB $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
  DB $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
  DB $84,$84,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $84,$84,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83
  DB $82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83,$82,$83

; vim: ft=rgbasm:tw=72:cc=72:ts=2:sw=2
