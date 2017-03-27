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
  ; We can exploit the fact that not all eight bits in one byte are
  ; used to store a height, but storing multiple heights in one byte
  ; results in much more awkward code. This is a space-time trade-off.
HEIGHTS: DS 16
RANDHEIGHT: DB ; a random number used for generating the heights

BGSCRL: DB ; amount to scroll the background by

  ; The player struct, which defines the state of the player.
  ;
  ; The Y-coordinate and downward velocity are are stored as two bytes
  ; each, in a 8.8 fixed point format. This means that the first byte
  ; is the integer portion of the number, and the second byte contains
  ; the fractional portion. The Y-coordinate is the actual pixel
  ; coordinate on screen, as opposed to being offset by 16 pixels as is
  ; the case in the OAM structure.
  ;
  ; The X-coordinate does not need to be stored in fixed point format,
  ; since no forces are applied horizontally, so no numeric integration
  ; needs to be done on the X-coordinate. The X-coordinate is the
  ; actual position on screen, plus 16. This allows the player to go
  ; partally off screen without the position wrapping around.
  ;
PLAYER_STATE:
  DB   ; the x position
  DS 2 ; the y position
  DS 2 ; the downward velocity

SCROLLFLAG: DB ; whether or not to scroll the world
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
  call lcd_off

  call init_ram
  call copy_dma_routine
  call load_bg_tileset
  call init_player_obj

  call lcd_on
  call start_timer

  ld a,%00000101 ; enable V-Blank interrupt
                 ; enable timer   interrupt
  ld [$ffff],a
  ei

  ; = MENU INITIALIZATION + MENU LOOP =================================

switch_to_menu:
  di
  call lcd_off

  call init_ram_for_menu
  call load_bg_for_menu
  call place_player_in_menu

  call lcd_on
  ei

menu_loop:
  halt
  nop

  ld a,[VBFLAG]
  or 0
  jr z,menu_loop
  ld a,0
  ld [VBFLAG],a

  call read_joypad
  call react_to_input_in_menu

  jr menu_loop

  ; = GAMEPLAY INITIALIZATION + GAMEPLAY LOOP =========================

start_game:
  di
  ; TODO: now, that we have the menu, we need to seed the PRNG based on
  ; how long the player waited until starting the game.
  ld a,$37
  ld [RANDHEIGHT],a

  call lcd_off

  call init_ram_for_gameplay
  call generate_initial_bg_for_gameplay
  call set_player_initial_position_for_gameplay

  call lcd_on
  ei

gameplay_loop:
  halt
  nop

  ld a,[VBFLAG]
  or 0
  jr z,gameplay_loop
  ld a,0
  ld [VBFLAG],a

  call read_joypad
  call react_to_input_during_gameplay
  call apply_forces
  call position_player
  call scroll_world_if_needed

  call check_for_game_over

  jr gameplay_loop

  ; = INITIALIZATION FUNCTIONS ========================================

init_ram:
  ; Initialize the RAM variables, but only the ones used all throughout
  ; the program. There are other variables only used in one mode of the
  ; program (e.g. menu vs. gameplay) that are initialized at the
  ; beginning of that mode.
  ld a,0
  ld [PAD],a
  ld [OLDPAD],a
  ld [VBFLAG],a

  ret

init_ram_for_menu:
  ld a,0
  ld [BGSCRL],a

  ret

init_ram_for_gameplay:
  ld a,0
  ld [BGSCRL],a

  ret

copy_dma_routine:
  ; copy the sprite DMA procedure into HRAM
  ld hl,sprite_dma
  ld de,hram_sprite_dma
  ld bc,sprite_dma_end-sprite_dma
  call memcpy

  ret

load_bg_tileset:
  ld a,[bgpal] ; set the background palette
  ld [$ff47],a

  ; load the background tiles into the Tile Data Table
  ld hl,bgbox  ; source address
  ld de,$8800  ; destination address
  ld bc,624    ; number of bytes to copy
  call memcpy

  ret

load_bg_for_menu:
  ; reset the screen position, both scrolly and scrollx
  ld a,0
  ld [$ff42],a
  ld [$ff43],a

  ; Zero out the background by loading only sky tiles. Some of the
  ; tiles will be overwritten by other tiles.

  ld b,$04    ; we will write $0400 (1024) bytes
  ld c,$00
  ld hl,$9800 ; we will be writing to the BG tile map
.bg_reset_loop:
  ld a,$84    ; we will only be writing sky tiles
  ld [hl],a   ; store one byte in the destination
  inc hl      ; prepare to write another byte

  ; the same caveat applies as in memcpy
  dec bc    ; decrement the counter
  ld a,b
  or c
  jr z,.bg_reset_loop_done ; return if all bytes written

  jr .bg_reset_loop

.bg_reset_loop_done:

  ; Boxes

  ld a,$80      ; box top-left tiles
  ld [$9908],a
  ld [$9948],a
  ld [$994a],a
  ld [$994c],a
  ld [$9986],a
  ld [$9988],a
  ld [$998a],a
  ld [$998c],a

  ld a,$81      ; box top-right tiles
  ld [$9909],a
  ld [$9949],a
  ld [$994b],a
  ld [$994d],a
  ld [$9987],a
  ld [$9989],a
  ld [$998b],a
  ld [$998d],a

  ld a,$82      ; box bottom-left tiles
  ld [$9928],a
  ld [$9968],a
  ld [$996a],a
  ld [$996c],a
  ld [$99a6],a
  ld [$99a8],a
  ld [$99aa],a
  ld [$99ac],a

  ld a,$83      ; box bottom-right tiles
  ld [$9929],a
  ld [$9969],a
  ld [$996b],a
  ld [$996d],a
  ld [$99a7],a
  ld [$99a9],a
  ld [$99ab],a
  ld [$99ad],a

  ; Game title

  ld hl,bgtitlemap      ; source address
  ld de,$9845           ; destination address
  ld bc,10              ; number of bytes to copy
  call memcpy

  ld hl,bgtitlemap + 10 ; source address
  ld de,$9865           ; destination address
  ld bc,10              ; number of bytes to copy
  call memcpy

  ld hl,bgtitlemap + 20 ; source address
  ld de,$9887           ; destination address
  ld bc,6               ; number of bytes to copy
  call memcpy

  ld hl,bgtitlemap + 26 ; source address
  ld de,$98a7           ; destination address
  ld bc,6               ; number of bytes to copy
  call memcpy

  ; Press start text

  ld hl,bgpressstartmap ; source address
  ld de,$99e4           ; destination address
  ld bc,12              ; number of bytes to copy
  call memcpy

  ret

generate_initial_bg_for_gameplay:
  ; reset the screen position
  ld a,0
  ld [$ff42],a ; scrolly will always be 0
  ld [$ff43],a ; scrollx starts at 0

  call generate_initial_heights
  call load_all_columns_into_bg_tile_map

  ret

generate_initial_heights:
  ld hl,HEIGHTS ; the base address of the height map

  ld a,$0
_generate_initial_heights_loop:
  push af       ; preserve A
  call next_rand_height
  ld [hl],a     ; generate two random heights and place it into the
                ; height map
  inc hl        ; go to the next byte in the height map

  pop af        ; restore A
  inc a         ; only generate data for 16 bytes
  cp $16
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
  ld b,$00      ; bc = $00AA, where AA = index
  ld c,a
  add hl,bc     ; actual address = start of height map + $00AA

  ld a,[hl]     ; grab the height from the height map.
  ld b,a        ; save the height in b

  sla d         ; multiply the column index by 2, since in the tile
                ; map, each column takes up two tiles
  ld h,$98      ; HL = $98DD, where DD = index * 2
  ld l,d

  jp load_column_into_bg_tile_map_at_address ; tail call

init_player_obj:
  ; Set up the initial player state, so that it can be placed as
  ; necessary. This doesn't actually position the player, as the
  ; starting position differs based on if we're on the menu or not.

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

  ret

place_player_in_menu:
  ld a,101
  ld [PLAYER_STATE],a   ; X-coordinate
  ld a,64
  ld [PLAYER_STATE+1],a ; Y-coordinate (integer part)

  ; All other portions of the player state are not set because they are
  ; not used during the menu.

  jp position_player    ; tail call

set_player_initial_position_for_gameplay:
  ld a,144
  ld [PLAYER_STATE],a   ; X-coordinate
  ld a,0
  ld [PLAYER_STATE+1],a ; Y-coordinate (integer part)
  ld [PLAYER_STATE+2],a ; Y-coordinate (fractional part)
  ld [PLAYER_STATE+3],a ; downward velocity (integer part)
  ld [PLAYER_STATE+4],a ; downward velocity (fractional part)

  jp position_player    ; tail call

start_timer:
  ; The timer will be incremented 4096 times each second, and each time
  ; it overflows, it will be reset to 196. This means that the timer
  ; will overflow every (1/4096) * (256 - 196) = ~0.0156s.
  ld a,192       ; the value of rTIMA after it overflows
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
  push af

  ld a,1
  ld [SCROLLFLAG],a

  pop af
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

react_to_input_in_menu:
.start_game:
  ld a,[PAD]
  bit 3,a
  jr z,.done

  jp start_game

.done:
  ret

react_to_input_during_gameplay:
  ; When moving horizontally, the player's height needs to be checked
  ; against the column the player will move into. For that, we'll need
  ; the player's x position, which we will manipulate before checking
  ; the height map.
  call load_player_real_x_position
  ld c,a

.move_right:
  ld a,[PAD]
  bit 4,a
  jr z,.move_left

  ; Check if the player is allowed to move right
  ld a,c        ; reload the x position and get the position the right
  add 17        ;   side of the player would end up at
  call load_height_map_entry_for_x_position
  call height_to_num_pixels_above_column
  add 1         ; add 1 to the number of pixels above the column
                ;   because being exactly on the ground is okay
  call is_player_above_column
  jr nz,.move_right_slower ; If we can't move right by 2 pixels, try to
                           ;   move right by only 1 pixel.

  ld a,[PLAYER_STATE]
  sub 176
  jr nc,.move_left

  add 178
  ld [PLAYER_STATE],a
  jr .move_left

.move_right_slower:
  ; Check if the player is allowed to move right
  ld a,c        ; reload the x position and get the position the right
  add 16        ;   side of the player would end up at
  call load_height_map_entry_for_x_position
  call height_to_num_pixels_above_column
  add 1         ; add 1 to the number of pixels above the column
                ;   because being exactly on the ground is okay
  call is_player_above_column
  jr nz,.move_left

  ld a,[PLAYER_STATE]
  sub 176
  jr nc,.move_left

  add 177
  ld [PLAYER_STATE],a
  jr .move_left

.move_left:
  ld a,[PAD]
  bit 5,a
  jr z,.jump

  ; Check if the player is allowed to move left
  ld a,c        ; reload the x position and get the position the left
  sub 1         ;   side of the player would end up at
  call load_height_map_entry_for_x_position
  call height_to_num_pixels_above_column
  add 1         ; add 1 to the number of pixels above the column
                ;   because being exactly on the ground is okay
  call is_player_above_column
  jr nz,.jump

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
  call load_player_real_x_position
  ld c,a               ; keep the position around for later

  and %00001111        ; if the last four bits are zero, then we're
  jr z,.on_column      ;   cleanly on a column

  ; At this point, we know at least one of the last three digits is
  ; non-zero, so we're between columns. That means we have to check
  ; two adjacent columns.

.between_columns:
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  ld d,a

  ld a,c        ; reload the x position
  add 15        ; get the x position at the right edge of the player
  call load_height_map_entry_for_x_position
  ld b,d

  call max
  call height_to_num_pixels_above_column
  jr is_player_above_column ; tail call

.on_column:
  ; The player is exclusively over a column.
  ld a,c        ; reload the x position
  call load_height_map_entry_for_x_position
  call height_to_num_pixels_above_column
  ; drop down into "is_player_above_column" function

is_player_above_column:
  ; parameters:
  ;   a = number of pixels above the column to check against
  ;
  ; Sets the zero flag if the player's feet are above the specified
  ; number of pixels, and unsets the flag otherwise (i.e. z := "is
  ; player not grounded").
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

load_player_real_x_position:
  ; output:
  ;   a = the real x position of the player's left side, accounting for
  ;       how much the background has scrolled (modulo any wraparound).
  ld a,[PLAYER_STATE]  ; to get the player's real x position, start
  sub 15               ;   with the position on screen, and add the
  ld hl,BGSCRL         ;   level scroll offset
  add a,[hl]
  ret

scroll_world_if_needed:
  ld a,[SCROLLFLAG]
  cp 0
  jr z,.no_scroll

  ld a,0
  ld [SCROLLFLAG],a

  ld hl,BGSCRL
  inc [hl]

  ld hl,PLAYER_STATE
  dec [hl]

.no_scroll:
  ret

check_for_game_over:
  ; The player's x position is stored as the on-screen x position plus
  ; 16, so that the player can be partially to the left of the screen.
  ; This means that when the player has fully gone past the left of the
  ; screen, the position has wrapped around to 255.

  ; Because the player can't go too far to the right of the screen, we
  ; can safely assume the player's position is near 255 only when the
  ; player has gone to the left of the screen. Accounting for a little
  ; bit of buffer, we can safely check to see if the position is near
  ; 255, and if so, end the game.
  ld a,[PLAYER_STATE]
  sub 250
  jr c,.done

  jp switch_to_menu

.done:
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

next_rand_height:
  ; Generate the next random height to put into the height map. The
  ; result is in the range of [2, 5] inclusive.
  ;
  ; output:
  ;   a = next random height
  push hl          ; preserve HL
  ld hl,RANDHEIGHT ; the PRNG seed is in RANDHEIGHT
  call rand        ; generate a pseudo-random number in the range
                   ; [1, 256]
  and %00000011    ; preserve the two least-significant bits of the
  add a,%00000010  ; height. This brings the value  down to the range
                   ; of [0, 3]. Then add "2", bringing the value into
                   ; the range [2, 5].
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

bgbigfont:
  ; backgound menu title, 7 2x2 tites
  DB $00,$00,$38,$38,$6c,$6c,$5e,$5e  ; M
  DB $5f,$5f,$5f,$5f,$5f,$5f,$5f,$5f
  DB $00,$00,$1c,$1c,$36,$36,$6e,$6e
  DB $de,$de,$de,$de,$fe,$fe,$fe,$fe
  DB $5b,$5b,$5b,$5b,$59,$59,$78,$78
  DB $78,$78,$78,$78,$38,$38,$00,$00
  DB $de,$de,$de,$de,$9e,$9e,$1e,$1e
  DB $1e,$1e,$1e,$1e,$1c,$1c,$00,$00
  DB $00,$00,$07,$07,$1f,$1f,$36,$36  ; O
  DB $2c,$2c,$6c,$6c,$5c,$5c,$5c,$5c
  DB $00,$00,$e0,$e0,$f8,$f8,$7c,$7c
  DB $3c,$3c,$3e,$3e,$3e,$3e,$3e,$3e
  DB $5c,$5c,$7c,$7c,$7c,$7c,$3c,$3c
  DB $3e,$3e,$1f,$1f,$07,$07,$00,$00
  DB $3e,$3e,$2e,$2e,$2e,$2e,$2c,$2c
  DB $6c,$6c,$f8,$f8,$e0,$e0,$00,$00
  DB $00,$00,$38,$38,$6e,$6e,$5f,$5f  ; N
  DB $5f,$5f,$5f,$5f,$5b,$5b,$5b,$5b
  DB $00,$00,$1c,$1c,$1e,$1e,$16,$16
  DB $16,$16,$16,$16,$96,$96,$96,$96
  DB $59,$59,$59,$59,$58,$58,$78,$78
  DB $78,$78,$78,$78,$38,$38,$00,$00
  DB $d6,$d6,$d6,$d6,$f6,$f6,$fe,$fe
  DB $7e,$7e,$7e,$7e,$1c,$1c,$00,$00
  DB $00,$00,$3f,$3f,$67,$67,$5e,$5e  ; E
  DB $58,$58,$70,$70,$70,$70,$3f,$3f
  DB $00,$00,$fc,$fc,$fe,$fe,$3e,$3e
  DB $0e,$0e,$04,$04,$00,$00,$80,$80
  DB $3f,$3f,$78,$78,$50,$50,$58,$58
  DB $7e,$7e,$7f,$7f,$3f,$3f,$00,$00
  DB $80,$80,$04,$04,$0e,$0e,$1e,$1e
  DB $36,$36,$ee,$ee,$fc,$fc,$00,$00
  DB $00,$00,$38,$38,$68,$68,$58,$58  ; Y
  DB $58,$58,$7c,$7c,$3e,$3e,$3f,$3f
  DB $00,$00,$1c,$1c,$1e,$1e,$16,$16
  DB $16,$16,$36,$36,$6c,$6c,$dc,$dc
  DB $1f,$1f,$0f,$0f,$03,$03,$03,$03
  DB $03,$03,$03,$03,$01,$01,$00,$00
  DB $f8,$f8,$e0,$e0,$c0,$c0,$c0,$c0
  DB $c0,$c0,$c0,$c0,$80,$80,$00,$00
  DB $00,$00,$3f,$3f,$6f,$6f,$5c,$5c  ; R
  DB $58,$58,$58,$58,$5c,$5c,$5f,$5f
  DB $00,$00,$e0,$e0,$f8,$f8,$7c,$7c
  DB $3c,$3c,$2c,$2c,$68,$68,$d8,$d8
  DB $5f,$5f,$5f,$5f,$5b,$5b,$79,$79
  DB $78,$78,$78,$78,$38,$38,$00,$00
  DB $f0,$f0,$c0,$c0,$e0,$e0,$f0,$f0
  DB $f8,$f8,$7c,$7c,$38,$38,$00,$00
  DB $00,$00,$38,$38,$68,$68,$58,$58  ; U
  DB $58,$58,$58,$58,$58,$58,$58,$58
  DB $00,$00,$1c,$1c,$1e,$1e,$1e,$1e
  DB $1e,$1e,$1e,$1e,$1e,$1e,$1e,$1e
  DB $58,$58,$5c,$5c,$7c,$7c,$3f,$3f
  DB $3f,$3f,$1f,$1f,$07,$07,$00,$00
  DB $16,$16,$36,$36,$2e,$2e,$ec,$ec
  DB $dc,$dc,$f8,$f8,$e0,$e0,$00,$00

bgsmallfont:
  DB $00,$00,$3c,$3c,$42,$42,$42,$42  ; P
  DB $7c,$7c,$40,$40,$40,$40,$00,$00
  DB $00,$00,$3c,$3c,$42,$42,$42,$42  ; R
  DB $7c,$7c,$48,$48,$44,$44,$00,$00
  DB $00,$00,$3c,$3c,$42,$42,$42,$42  ; E
  DB $7c,$7c,$40,$40,$3e,$3e,$00,$00
  DB $00,$00,$3c,$3c,$42,$42,$40,$40  ; S
  DB $3c,$3c,$02,$02,$3c,$3c,$00,$00
  DB $00,$00,$7e,$7e,$08,$08,$08,$08  ; T
  DB $08,$08,$08,$08,$08,$08,$00,$00
  DB $00,$00,$18,$18,$24,$24,$42,$42  ; A
  DB $7e,$7e,$42,$42,$42,$42,$00,$00

bgtitlemap:
  ; The tile map for the menu title. This is just the four lines that
  ; make up the title, and that too only the part of the lines that
  ; have actual tiles in them. The bytes are copied into the correct
  ; location in the tile map area of memory.
  DB $85,$86,$89,$8a,$8d,$8e,$91,$92,$95,$96  ; Line 0
  DB $87,$88,$8b,$8c,$8f,$90,$93,$94,$97,$98  ; Line 1
  DB         $99,$9a,$9d,$9e,$8d,$8e          ; Line 2
  DB         $9b,$9c,$9f,$a0,$8f,$90          ; Line 3

bgpressstartmap:
  ; The tile map for "PRESS START" text. This is just the line that
  ; makes up the text, and that too only part of the line. The bytes
  ; are copied into the correct location int he tile map area of
  ; memory.
  DB $a1,$a2,$a3,$a4,$a4,$84,$84,$a4,$a5,$a6,$a2,$a5

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
