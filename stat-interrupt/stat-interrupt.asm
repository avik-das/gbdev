; stat-interrupt.asm - Avik Das
;

  ; = DATA/VARIABLES ==================================================

SECTION "RAM",WRAM0[$c000]

SCRBAS: DB ; the base offset into the background scroll table
SCROFF: DB ; display line-specific offset into the scroll table
WNDWON: DB ; whether the window is being drawn or not

VBFLAG: DB ; whether or not we are in V-Blank

  ; = INTERRUPT HANDLERS ==============================================

  ; These are simple interrupt handlers that simply call the actual
  ; procedures responsible for taking any action. The procedures will
  ; call "reti".

SECTION "vblank",ROM0[$40]
  nop
  jp    vblank

SECTION "stat"  ,ROM0[$48]
  nop
  jp    stat

SECTION "timer" ,ROM0[$50]
  nop
  jp    timer

SECTION "start" ,ROM0[$100]
  nop
  jp    start
  
  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
  
  DB "HBLANK",0,0,0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
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
  call clear_oam

  call lcd_on
  call start_timer

  ld a,%00001000 ; enable H-Blank   interrupt
  ld [$ff41],a

  ld a,%00000111 ; enable V-Blank   interrupt
                 ; enable LCDC STAT interrupt
                 ; enable timer     interrupt
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

  jr   loop

  ; = INITIALIZATION FUNCTIONS ========================================

init_ram:
  ; initialize the RAM variables
  ld a,0
  ld [SCRBAS],a
  ld [SCROFF],a
  ld [WNDWON],a
  ld [VBFLAG],a

  ret

load_bg:
  ; load the background palette
  ld  a,[bgpal]
  ld  [$ff47],a

  ; reset the screen position
  ld a,0
  ld [$ff42],a ; scrolly = 0
  ld [$ff43],a ; scrollx = 0

  ; load the background tiles into the Tile Data Table
  ld hl,tiles  ; source address
  ld de,$8800  ; destination address
  ld bc,32     ; number of bytes to copy
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

  ; offset the window layer
  ld a,0       ; y offset
  ld [$ff4a],a
  ld a,7       ; x offset (actual offset is value-7)
  ld [$ff4b],a

  ret

clear_oam:
  ; zero out the OAM buffer
  ld de,$fe00
  ld bc,256
  call zeromem

  ret

start_timer:
  ; The timer will be incremented 4096 times each second, and each time
  ; it overflows, it will be reset to 0. This means that the timer will
  ; overflow every (1/4096) * 256 = 0.0625s.
  ld a,0         ; the value of rTIMA after it overflows
  ld [$ff06],a
  ld a,%00000100 ; enable the timer
                 ; increment rTIMA at 4096Hz
  ld [$ff07],a

  ret

  ; = INTERRUPT HANDLERS ==============================================

vblank:
  push af
  push bc
  push de
  push hl

  ld   a,1
  ld   [VBFLAG],a

  ; reset the background scroll index
  ld   a,0
  ld   [SCROFF],a

  ; at the top of the screen, the window is visible
  call show_window

  ; However, at line 16, the window should not be visible. To achieve
  ; this, we set the value of rLYC, which will cause the Coincidence
  ; Flag of rSTAT (the LCDC Status register) to be set when the scanline
  ; is 16 (i.e. when rLY = rLYC). We will be able to check this in the
  ; H-Blank interrupt.
  ld   a,16
  ld   [$ff45],a
  ld   a,1
  ld   [WNDWON],a

  pop  hl
  pop  de
  pop  bc
  pop  af
  reti

stat:
  push af
  push bc
  push de
  push hl

  ; First, we ensure that we're in an H-Blank interrupt. Technically, we
  ; also care about the LYC=LY Coincidence, but instead of using a
  ; separate interrupt for that, we can just check the Coincidence Flag
  ; during the H-Blank interrupt. Thus, we will not process the
  ; interrupt unless we are in an H-Blank, that is when the Mode Flag is
  ; exactly zero.
  ld   a,[$ff41] ; load rSTAT
  and  %00000011  ; extract the mode flag
  cp   0          ; ensure that it's zero
  jr   nz,.done

  ; add the base scroll offset and the line-specific scroll offset to
  ; get the final index into the scroll table
  ld   a,[SCRBAS]
  ld   b,a
  ld   a,[SCROFF]
  add  a,b
  
  ; note that the above result wraps around at 256, which is what we
  ; want, because the table is only 256 bytes long

  ; 16-bit addition
  ld   hl,sintbl  ; load the base address
                  ; the offset is in a
  ld   c,a        ; transfer the offset into bc...
  ld   b,0        ; ...by making c=a and b=0...
  add  hl,bc      ; ...and add the two 16-bit values

  ld   a,[hl]     ; load the scroll value from the table
  ld   [$ff43],a  ; and scroll the background

  ; increment the offset
  ld   hl,SCROFF
  inc  [hl]

  ; Now we check the Coincidence Flag to see if LYC=LY. If so, we will
  ; then toggle the window, and if necessary, modify rLYC.
  ld   a,[$ff41]  ; load rSTAT
  bit  2,a        ; check the Coincidence Flag
  jr   z,.done    ; if cleared, don't bother with the window

  ; at this point, we know that LYC=LY

  ; If the window is currently being shown, then we need to turn it off,
  ; then set rLYC to 128. This will ensure that we again see the
  ; Coincidence Flag set later down the screen, where we'll need to show
  ; the window again.
  ld   a,[WNDWON]
  cp   0
  jr   nz,.hide_window

  ; On the other hand, if the window is not visible, we're already near
  ; the bottom of the screen, so we just need to show the window.
  ; rLYC is reset by the V-Blank interrupt handler.
  call show_window
  jr   .done

.hide_window
  ld   a,128
  ld   [$ff45],a  ; set rLYC=128
  ld   a,0
  ld   [WNDWON],a ; the window should no longer be visible
  call hide_window

.done:
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

  ld   hl,SCRBAS
  inc  [hl]

  pop  hl
  pop  de
  pop  bc
  pop  af
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
  ld a,%11000111 ; LCD on
                 ; Window Tile Map at $9800-9fff
                 ; window off (for now)
                 ; BG & window Tile Data at $8800-$97ff
                 ; BG Tile Map at $9800-$9bff
                 ; sprite size 8x16
                 ; sprites on
                 ; BG & window on
  ld [$ff40],a   ; write it to the LCD Control register

  ret

show_window:
  ld  a,[$ff40]
  set 5,a
  ld  [$ff40],a
  ret

hide_window:
  ld  a,[$ff40]
  res 5,a
  ld  [$ff40],a
  ret

  ; = DATA ============================================================

bgpal:
  DB %11100100 ; dark to light

tiles:
  ; light stripe, 1 tile
  DB $83,$00,$07,$00,$0e,$00,$1c,$00
  DB $38,$00,$70,$00,$e0,$00,$c1,$00
  ; dark, reversed stripe, 1 tile
  DB $c1,$ff,$e0,$ff,$70,$ff,$38,$ff
  DB $1c,$ff,$0e,$ff,$07,$ff,$83,$ff

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

window:
  ; define one map's worth, again 1024 tiles = 64 lines
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
  DB $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81

  ; Generate a 256 byte sine table with values between 0 and 128. The
  ; assembler functions used operate on 16.16 fixed-point numbers, that
  ; is 32-bit numbers that have the upper 16 bits for the integer part
  ; and the lower 16 bits for the fractional part.
  ;
  ; Sine values are between [-1.0,1.0], so by multiplying and shifting
  ; the right values, we bring the values into [0,128].
sintbl:
DEF angle = 0
REPT 256
  DB (64 * SIN(angle) + 64) >> 16
  DEF angle += 256
ENDR

; vim: ft=rgbasm:tw=72:ts=2:sw=2
