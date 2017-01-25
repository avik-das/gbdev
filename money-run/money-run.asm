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

  ; The background is 256 pixels wide, with each column taking up 16
  ; pixels, giving us 16 separate heights to work with.
  ;
  ; Now, while the background is also 256 pixels tall, we only see the
  ; top 144 pixels. Again, with each row taking up 16 pixels, that means
  ; each height is between 0 and 9. We can allow a single height to take
  ; 4 bits, i.e. 2 heights per byte. (In reality, we don't allow a
  ; column to take up the entire screen height, nor allow an empty
  ; column, so we could get away with 3 bits per column, but that's
  ; awkward.)
  ;
  ; Thus, we need to allocate space for 8 bytes.
HEIGHTS: DS 8

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

  call generate_initial_heights
  call load_all_columns_into_bg_tile_map

  ret

generate_initial_heights:
  ; TODO: in the future, we want to procedurally generate these
  ; heights, but for now, hard-code them as increasing, then
  ; decreasing, steps.
  ;
  ; When we start doing procedural generation, we'll also need to be
  ; able to set a single height at a time, but for now, just set them
  ; all at once.
  ld hl,HEIGHTS

  ld [hl],$12
  inc hl
  ld [hl],$34
  inc hl
  ld [hl],$56
  inc hl
  ld [hl],$78
  inc hl
  ld [hl],$87
  inc hl
  ld [hl],$65
  inc hl
  ld [hl],$43
  inc hl
  ld [hl],$21

  ret

load_all_columns_into_bg_tile_map:
  ld a,$0
_load_all_columns_loop:
  push af
  call load_column_into_bg_tile_map
  pop af
  inc a
  cp $10
  jr nz, _load_all_columns_loop

  ret

load_column_into_bg_tile_map:
  ; parameters:
  ;   a = the index of the column to load
  ld d,a        ; save a, because we'll need the original value later

  ld hl,HEIGHTS ; start with the start address of the height map
  srl a         ; divide the column index by 2, since there are two
                ; columns per byte
  ld b,$00      ; bc = $00AA, where AA = index / 2
  ld c,a
  add hl,bc     ; actual address = start of height map + $00AA

  ld a,d        ; grab the original index again
  srl a         ; divide it by 2, which sets the carry flag if the
                ; index was odd
  ld a,[hl]     ; grab the two heights from the height map. Loading
                ; from memory doesn't affect the carry flag, so we'll
                ; check against the carry flag set by the division.
  jr c, _load_column_height_done
  swap a        ; bring the high nibble down if the index was even

_load_column_height_done:
  and $0f       ; preserve only the low nibble
  ld b,a        ; save the height in b

  sla d         ; multiply the column index by 2, since in the tile
                ; map, each column takes up two tiles
  ld h,$98      ; HL = $98DD, where DD = index * 2
  ld l,d

  jr load_column_into_bg_tile_map_at_address ; tail call

  ; = INTERRUPT HANDLERS ==============================================

vblank:
  reti

  ; = UTILITY FUNCTIONS ===============================================

load_column_into_bg_tile_map_at_address:
  ; parameters:
  ;   hl = start address of bg tile map
  ;   b  = number of rows filled with boxes
  ld a,$9   ; compute the number of rows filled with sky
  sub b     ;   b = 9 - b
  ld b,a

  ld d,$00  ; DE = $001f
  ld e,$1f  ;   the amount to increment HL by each tile row, minus one
            ;   (so two increments are needed for each logical row).
            ;   the minus one is because we'll increment by one in
            ;   order to fill out the left and right sides of the box
            ;   on each of the top and bottom portions of the box.

  ld c,$0
_load_column_loop:
  ld a,c    ; if c == b: load a box tile
  sub b
  jr nc, _load_column_box

  ld a,$84  ; otherwise, load a sky tile
  ld [hl],a ; fill out the top-left
  inc hl    ; fill out the top-right
  ld [hl],a
  add hl,de ; fill out the bottom-left
  ld [hl],a
  inc hl    ; fill out the bottom-right
  ld [hl],a
  add hl,de ; get ready for the next load
  jr _load_column_graphic_done

_load_column_box:
  ld a,$80  ; load a box tile
  ld [hl],a ; fill out the top-left
  inc hl    ; fill out the top-right
  inc a     ;   with the next tile
  ld [hl],a
  add hl,de ; fill out the bottom-left
  inc a     ;   with the next tile
  ld [hl],a
  inc hl    ; fill out the bottom-right
  inc a     ;   with the next tile
  ld [hl],a
  add hl,de ; get ready for the next load

_load_column_graphic_done:
  inc c     ; only bother filling out the top 9 rows, as that's all
  ld a,c    ; that will be visible on screen
  cp $9
  jr nz, _load_column_loop
  ret

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

; vim: ft=rgbasm:tw=72:cc=72:ts=2:sw=2
