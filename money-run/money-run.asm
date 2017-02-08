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
RANDHEIGHT: DB ; a random number used for generating the heights

BGSCRL: DB ; amount to scroll the background by

  ; The player struct, which defines the state of the player.
  ;
  ; The Y-coordinate and downward velocity are are stored as two bytes
  ; each, in a 8.8 fixed point format. This means that the first byte
  ; is the integer portion of the number, and the second byte contains
  ; the fractional portion.
  ;
  ; The X-coordinate does not need to be stored in fixed point format,
  ; since no forces are applied horizontally, so no numeric integration
  ; needs to be done on the X-coordinate.
PLAYER_STATE:
  DB   ; the x position
  DS 2 ; the y position
  DS 2 ; the downward velocity

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
  jp vblank

SECTION "timer",HOME[$50]
  nop
  jp timer

SECTION "start",HOME[$100]
  nop
  jp start

  ; = CATRIDGE HEADER =================================================

  ; Nintendo logo. must be exactly as given
  DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

  DB "MONEYRUN",0,0,0,0,0,0,0 ; title, upper case ASCII, 15 bytes
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
  ; For now, initialize the PRNG with a hard-coded value, which will
  ; of course make everything deterministic. Later, we can change this
  ; to use some user-dependent data as the seed.
  ld a,$37
  ld [RANDHEIGHT],a

  call lcd_off

  call init_ram
  call copy_dma_routine
  call load_bg
  call load_obj

  call lcd_on
  call start_timer

  ld a,%00000101 ; enable V-Blank interrupt
                 ; enable timer   interrupt
  ld [$ffff],a
  ei

  ; = MAIN LOOP =======================================================

loop:
  halt
  nop

  ld a,[VBFLAG]
  or 0
  jr z,loop
  ld a,0
  ld [VBFLAG],a

  call read_joypad
  call react_to_input
  call apply_forces
  call position_player

  jr loop

  ; = INITIALIZATION FUNCTIONS ========================================

init_ram:
  ; initialize the RAM variables
  ld a,0
  ld [PAD],a
  ld [OLDPAD],a
  ld [BGSCRL],a
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
  ld a,[bgpal] ; set the background palette
  ld [$ff47],a

  ; reset the screen position
  ld a,0
  ld [$ff42],a ; scrolly will always be 0
  ld [$ff43],a ; scrollx starts at 0

  ; load the background tiles into the Tile Data Table
  ld hl,bgbox  ; source address
  ld de,$8800  ; destination address
  ld bc,96     ; number of bytes to copy
  call memcpy

  call generate_initial_heights
  call load_all_columns_into_bg_tile_map

  ret

generate_initial_heights:
  ld hl,HEIGHTS ; the base address of the height map

  ld a,$0
_generate_initial_heights_loop:
  push af       ; preserve A
  call next_two_rand_heights
  ld [hl],a     ; generate two random heights and place it into the
                ; height map
  inc hl        ; go to the next byte in the height map

  pop af        ; restore A
  inc a         ; only generate data for 8 bytes
  cp $8
  jr nz, _generate_initial_heights_loop

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

  jp load_column_into_bg_tile_map_at_address ; tail call

load_obj:
  ; Set up the initial player state.
  ;
  ; The x position is the actual position on screen, plus 16. This
  ; allows the player to go partally off screen without the position
  ; wrapping around.
  ;
  ; The y position is the actual pixel coordinates on screen, as
  ; opposed to being offset by 16 pixels as is the case in the OAM
  ; structure.
  ld a,144
  ld [PLAYER_STATE],a   ; X-coordinate
  ld a,0
  ld [PLAYER_STATE+1],a ; Y-coordinate (integer part)
  ld [PLAYER_STATE+2],a ; Y-coordinate (fractional part)
  ld [PLAYER_STATE+3],a ; downward velocity (integer part)
  ld [PLAYER_STATE+4],a ; downward velocity (fractional part)

  ; load the object palette 0
  ld a,[sppal] ; load the object palette 0 data
  ld [$ff48],a ; and store it into the object palette 0 register

  ; zero out the OAM buffer
  ld de,OAMBUF
  ld bc,256
  call zeromem

  ; load the sprite tiles into the Sprite Pattern Table
  ld hl,ghost ; source address
  ld de,$8000 ; destination address
  ld bc,256   ; number of bytes to copy
  call memcpy

  ; Display the sprites on the screen by populating the Object
  ; Attribute Memory (OAM). The X- and Y-coordinates will be filled in
  ; by "position_player" later.
  ld a,0         ; pattern number
  ld [PLAYER+2],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [PLAYER+3],a

  ; our player requires two sprites, so initialize the second one
  ld a,2         ; pattern number
  ld [PLAYER+6],a
  ld a,%00000000 ; priority: on top of background
                 ; no y-flip
                 ; no x-flip
                 ; palette 0
                 ; 4 LSB ignored
  ld [PLAYER+7],a

  jp position_player ; tail call

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

  ; Note that the DMA procedure must be initiated from High RAM. The
  ; mechanism for that is detailed alongside the definition of this
  ; initiation procedure.
  call hram_sprite_dma
  call scroll_bg

  ld a,1
  ld [VBFLAG],a

  pop hl
  pop de
  pop bc
  pop af
  reti

timer:
  push hl

  ld hl,BGSCRL
  inc [hl]

  ld hl,PLAYER_STATE
  dec [hl]

  pop hl
  reti

  ; = MAIN LOOP FUNCTIONS =============================================

read_joypad:
  ; The state that was current before should now become the old state.
  ld a,[PAD]
  ld [OLDPAD],a

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

  and %00001111 ; pick out only the input lines...
  swap a        ; ...put the states into the high nibble...
  ld b,a        ; ...and save it away temporarily

  ; Now we want to read the buttons, and that means we disable the d-pad
  ; (by setting bit 4) and enable the buttons (by clearing bit 5).
  ld a,%00010000
  ld [$ff00],a

  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]
  ld a,[$ff00]

  and %00001111
  or b          ; B contains the d-pad input in the high nibble, so we
                ; can we incorporate the button input in the low nibble

  ; Now, A contains the state of the d-pad and the buttons, but when a
  ; bit is cleared, that means the button is pressed. So, we take the
  ; complement, and we have an intuitive mapping of 1->pressed and
  ; 0->not pressed.
  cpl
  ld [PAD],a

  ret

react_to_input:
.move_right:
  ld a,[PAD]
  bit 4,a
  jr z,.move_left

  ld a,[PLAYER_STATE]
  sub 175
  jr nc,.move_left

  add 176
  ld [PLAYER_STATE],a

.move_left:
  ld a,[PAD]
  bit 5,a
  jr z,.jump

  ld a,[PLAYER_STATE]
  sub 1
  ld [PLAYER_STATE],a

.jump:
  ld a,[PAD]
  bit 0,a
  jr z,.done

  ld a,[OLDPAD]
  bit 0,a
  jr nz,.done ; only respond to A if A wasn't pressed before

  ld a,[PLAYER_STATE+3]
  cp 0
  jr nz,.done

  ld a,[PLAYER_STATE+4]
  cp 0
  jr nz,.done

  ld a,$fc
  ld [PLAYER_STATE+3],a
  ld a,$70
  ld [PLAYER_STATE+4],a

.done:
  ret

apply_forces:
  ; In simple Newtonian mechanics, over time:
  ;
  ; 1. The downward speed of an object increases linearly with time,
  ;    proportional to the acceleration due to gravity.
  ;
  ; 2. The Y-position increases linearly, proportional to the downward,
  ;    velocity. This means it actually increases quadratically with
  ;    time:
  ;
  ;        y(t) = g * t^2 + v0 * t + y0
  ;
  ; This results in the following updates:
  ;
  ;     y' = y' + v * t * k
  ;     v' = v' + g * t * k
  ;
  ; Where "k" is a constant to account for the fact that we're dealing
  ; with pixels and frames, not meters and seconds. Because the
  ; timestep is constant (always one frame), we can set "t * k = 1",
  ; and set "g" to whatever value makes sense for us.

  ; y' = y' + v
  ld bc,PLAYER_STATE+2
  ld de,PLAYER_STATE+4
  call add_16bit_numbers_in_memory

  call check_is_grounded
  jr z,.not_grounded

  ; Since the player is on or below the ground, reset the player to the
  ; correct height, and set the downward velocity to "0". Register a
  ; contains the height to which to reset the player, as an integer.
  sub 16 ; subtract 16 because the height refers to the player's feet
  ld [PLAYER_STATE+1],a
  ld a,0
  ld [PLAYER_STATE+2],a
  ld [PLAYER_STATE+3],a
  ld [PLAYER_STATE+4],a
  jr .grounded

.not_grounded:
  ; v' = v' + g
  ; register b = g in the above equation. The value can be adjusted
  ; based on trial and error to get the right feel for the game.
  ld hl,PLAYER_STATE+4
  ld b,32
  call add_fractional_to_16bit_number

.grounded:
  ret

check_is_grounded:
  ; output:
  ;   a = the height where the player should be placed, if the player
  ;       is on or below the ground. The output is "0" if the player is
  ;       above the height of the tallest column under it.
  ;
  ; Sets the zero flag based on if "a" is "0" or not.
  ld a,[PLAYER_STATE]  ; to get the player's real x position, start
  sub 16               ;   with the position on screen, and add the
  ld hl,BGSCRL         ;   level scroll offset
  add a,[hl]
  ld c,a               ; keep the position around for later

  and %00011111        ; if the last five bits are zero, then we're
  jr z,.on_even_column ;   cleanly on an even-numbered column
  and %00001111        ; otherwise, if the last four bits are zero,
  jr z,.on_odd_column  ;   then the fifth bit must have been one, and
                       ;   we're cleanly on an odd-numbered column

  ; At this point, we know at least one of the last four digits is non-
  ; zero, so we're between columns. That means we have to check both
  ; nibbles of one byte in the height map, or one nibble each in two
  ; adjacent bytes.
  ld a,c
  and %00010000
  jr z,.between_even_and_odd_column

.between_odd_and_even_column:
  ; The column under the left part of the player is an odd-numbered
  ; column, and the column under the right part of the player is an
  ; even-numbered column. That means we have to check the low nibble
  ; of one height map entry, and the high nibble of the next one.
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  and %00001111 ; get the low nibble of the height
  ld d,a

  ld a,c        ; reload the x position
  add 15        ; get the x position at the right edge of the player
  call load_height_map_entry_for_x_position
  and %11110000 ; get the high nibble of the height
  swap a        ;   storing it in the low nibble
  ld b,d

  call max
  call height_to_num_pixels_above_column
  jr .check_against_player_y_position

.on_even_column:
  ; The player is exclusively over an even-number column.
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  and %11110000 ; get the high nibble of the height
  swap a        ;   storing it in the low nibble
  call height_to_num_pixels_above_column
  jr .check_against_player_y_position

.on_odd_column:
  ; The player is exclusively over an odd-number column.
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  and %00001111 ; get the low nibble of the height
  call height_to_num_pixels_above_column
  jr .check_against_player_y_position

.between_even_and_odd_column:
  ; The column under the left part of the player is an even-numbered
  ; column, and the column under the right part of the player is an
  ; odd-numbered column. That means we have to check the low and high
  ; nibbles of the same height map entry.
  ; of one height map entry, and the high nibble of the next one.
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  push af       ; save the height map entry
  and %00001111 ; get the low nibble of the height
  ld d,a

  pop af
  and %11110000 ; get the high nibble of the height
  swap a        ;   storing it in the low nibble
  ld b,d

  call max
  call height_to_num_pixels_above_column

.check_against_player_y_position:
  ; At this point, no matter which branch was executed, register a
  ; contains the number of pixels above the player's feet on the
  ; highest column under the player.
  ;
  ; Now, we just compare that value against the position of the
  ; player's feet, and we're done.
  ld b,a
  ld a,[PLAYER_STATE+1] ; load the player's y position and add 16 to
  add 16                ;   get the position of the player's feet
  sub b                 ; compare it against the ground height as
  jr c,.not_grounded    ;   computed above (number of pixels above the
                        ;   the player's feet)
  ld a,b                ; either we're below the ground or on the
  jr .set_z_flag        ;   ground. Either way, the output is the
                        ;   ground position.

.not_grounded:
  ld a,0

.set_z_flag:
  cp 0
  ret

load_height_map_entry_for_x_position:
  ; parameters:
  ;   a = the real x position of the player in the level, modulo 256.
  ;       This is the x position of the player on the screen, plus the
  ;       offset of the background.
  ; output:
  ;   a = the height map entry corresponding to the given x position.
  ;       This entry is one byte, and it consists of the heights for
  ;       two adjacent columns.
  srl a         ; divide the x position by 16 to get the column number
  srl a
  srl a
  srl a
  srl a         ; and again by 2 to get the index into the height map
  ld hl,HEIGHTS ; load the base address of the height map
  add a,l       ;   add the index into the height map and save it into
  ld l,a        ;   "l" so that "hl" points to the height that we then
  ld a,[hl]     ;   load into "a"
  ret

height_to_num_pixels_above_column:
  ; parameters:
  ;   a = the height of a column, as extracted from the height map
  ; output:
  ;   a = the number of pixels that make up the sky above the column
  ld b,a        ; compute (9 - height), as that tells us the number of
  ld a,9        ;   sky tiles above the column
  sub b
  sla a         ; multiply by 16 to get the number of pixels above the
  sla a         ;   column
  sla a
  sla a
  ret

position_player:
  ; Note that the actual Y-coordinate on the screen is the value stored
  ; in the OAM structure minus 16, and the actual X-coordinate is the
  ; stored value minus 8. Furthermore, the two sprites that comprise
  ; the player have the same Y-coordinate, but two different
  ; X-coordinates (one being 8 more than the other).

  ; Because the coordinates are stored in 8.8 fixed point format, we
  ; can simply ignore the second byte in each pair, since the entire
  ; first byte is the integer portion.

  ; load and write the X-coordinates
  ld a,[PLAYER_STATE]
  sub a,8
  ld [PLAYER+1],a
  add a,8
  ld [PLAYER+5],a

  ; load and write the Y-coordinate
  ld a,[PLAYER_STATE+1]
  add a,16
  ld [PLAYER],a
  ld [PLAYER+4],a

  ret

scroll_bg:
  ld a,[BGSCRL]
  ld [$ff43],a ; set scrollx
  ret

  ; During the DMA transfer, the CPU can only access the High RAM
  ; (HRAM), so the transfer must be initiated from inside of HRAM.
  ; However, we can't directly place this procedure there at assembly
  ; time, so we'll put it here with the rest of the code, then copy it
  ; into HRAM at run time.
sprite_dma:
  ld a,OAMBUF/$100
  ld [$ff46],a
  ld a,$28
.wait:
  dec a
  jr nz,.wait
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
  ; is not updated, so we need to check if the counter is zero
  ; manually. A 16-bit value is zero when its constituent 8-bit
  ; portions are both zero, that is when (b | c) == 0, where "|" is a
  ; bitwise OR.
  dec bc    ; decrement the counter
  ld a,b
  or c
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
  ld a,b
  or c
  ret z     ; return if all bytes written

  jr .zeromem_loop

lcd_off:
  ld a,[$ff40] ; load the LCD Control register
  bit 7,a      ; check bit 7, whether the LCD is on
  ret z        ; if off, return immediately

wait_vblank:
  ld a,[$ff44]  ; load the vertical line the LCD is at
  cp 145
  jr nz, wait_vblank

  ; Technically, at this point, we are not just at VBlank, but exactly
  ; at the start of VBlank. This is because the previous instruction
  ; made sure that [LY] is exactly 145, the first value when entering
  ; VBlank.

  ld a,[$ff40] ; load the LCD Control register
  res 7,a      ; clear bit 7, turning off the LCD
  ld [$ff40],a ; write the new value back to the LCD Control register

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

rand:
  ; An 8-bit pseudo-random number generator. Taken from
  ; http://www.z80.info/pseudo-random.txt
  ;
  ; R = an integer in the range [1, 256]
  ; R -> (33 * R) mod 257
  ;
  ; parameters:
  ;   hl = address where to take the seed and store the next seed
  ; output:
  ;   a = next random number
  ld a,[hl]
  ld b,a

  rrca ; multiply by 32
  rrca
  rrca
  xor $1f

  add a,b
  sbc a,$ff

  ld [hl],a
  ret

next_two_rand_heights:
  ; Generate the next two random heights to put into the height map.
  ; The result is stored as a single byte, with each nibble containing
  ; a separate height between the range of [1, 8] inclusive.
  ;
  ; output:
  ;   a = next two random heights
  push hl          ; preserve HL
  ld hl,RANDHEIGHT ; the PRNG seed is in RANDHEIGHT
  call rand        ; generate a pseudo-random number in the range
                   ; [1, 256]
  and %00110011    ; preserve the two least-significant bits of each
  add a,%00100010  ; nibble. This brings the value in each nibble down
                   ; to the range of [0, 3]. Then add "2" to each
                   ; nibble, bringing each one into the range [2, 5].
  pop hl           ; restore HL
  ret

add_fractional_to_16bit_number:
  ; Adds a fractional value to 16-bit number stored in a 8.8 fixed
  ; point format. This entails adding the given value to the second
  ; byte (fractional part) of the number, then propagating the carry to
  ; the first byte (the integer part).
  ;
  ; No corresponding function is necessary to add an integer value, as
  ; that can be accomplished purely by adding to the first byte,
  ; without dealing with any carry.
  ;
  ; Stores the resulting number back at the location from which the
  ; original value was read.
  ;
  ; parameters:
  ;   hl = address of *second* byte of the number to add to
  ;   b  = the fractional number to add
  ; output:
  ;   [hl - 1] = [hl - 1] + b
  ld a,[hl]   ; load the fractional part
  add a,b     ; add the fractional part, setting the carry flag
  ld [hl],a   ; store the fractional part
  dec hl      ; move the address to the integer part
              ;   this does not set the carry

  jr nc,.done ; no need to do anything to the integer part if there was
              ;   no carry from the fractional part
  inc [hl]    ; but if there was a carry, increment the integer part

.done:
  ret

max:
  ; Given two 8-bit integers, returns the larger of the two.
  ;
  ; parameters:
  ;   a = the first integer
  ;   b = the second integer
  ; output:
  ;   a = max(a, b)
  sub b              ; carry => a - b < 0 => a < b
  jr c,.b_is_larger

.a_is_larger:
  add b              ; restore a to its original value
  ret

.b_is_larger:
  ld a,b
  ret

add_16bit_numbers_in_memory:
  ; Adds two 16-bit numbers stored in a 8.8 fixed point format, storing
  ; the result back into the memory location of the first of the two
  ; numbers.
  ;
  ; parameters:
  ;   bc = address of the *second* byte of the first number
  ;   de = address of the *second* byte of the second number
  ; output:
  ;   [bc - 1] = [bc - 1] + [de - 1]
  ld a,[bc] ; load the fractional part of the first number
  ld h,a    ;   into "h"
  ld a,[de] ; load the fractional part of the second number into "a"
  add a,h   ; a += h. This sets the carry flag.
  ld [bc],a ; store the result back into the fractional part of the
            ;   first number

  dec bc    ; move the the integer part of both numbers
  dec de    ; this doesn't affect the carry flag

  ld a,[bc] ; load the integer part of the first number
  ld h,a    ;   into "h"
  ld a,[de] ; load the integer part of the second number into "a"
  adc a,h   ; a += h + c
  ld [bc],a ; store the result back into the integer part of the first
            ;   number
  ret

  ; = DATA ============================================================

bgpal:
  DB %11100100 ; white is transparent, lightest to darkest.

sppal:
  DB %00011100 ; missing second darkest

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

ghost:
  ; foreground ghost
  ;
  ; There are a total of 4 tiles laid out in a 2x2 grid, but because
  ; the sprites used are 8x16 pixels in dimension, only two sprites are
  ; required.

  DB $03,$00,$0c,$03,$10,$0f,$11,$0f ; sprite 0
  DB $23,$1d,$27,$19,$27,$19,$61,$1f
  DB $ab,$55,$a5,$5a,$a1,$5f,$61,$1f
  DB $21,$1f,$20,$1f,$26,$19,$19,$00
  DB $c0,$00,$30,$c0,$f8,$f0,$f8,$f0 ; sprite 1
  DB $fc,$d8,$fc,$98,$fc,$98,$fe,$f8
  DB $ff,$5a,$ff,$aa,$ff,$fa,$fe,$f8
  DB $fc,$f8,$fc,$f8,$7c,$98,$98,$00

; vim: ft=rgbasm:tw=72:cc=72:ts=2:sw=2
