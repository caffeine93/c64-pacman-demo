;Screen RAM $0400-$07E7
;Color RAM $D800-$DBE7

;Screen RAM start address
screen_RAM = $0400

;CIA1 Interrupt Control Register
cia1_icr = $DC0D
;CIA2 Interrupt Control Register
cia2_icr = $DD0D
;VIC-II's Interrupt Control Register
vic_icr = $D01A
;VIC-II's Control Register 1
vic_cr1 = $D011
;VIC-II's Raster Position Register
vic_raster_position = $D012
;VIC-II's Interrupt Request Register
vic_irr = $D019
;VIC-II's Sprite to Sprite Collision Detect Register
vic_scr = $D01E
;VIC-II's Sprite Display Enable Register
vic_sprites_enable = $D015
;VIC-II's Sprite to Background Display Priority Register
vic_sprites_priority = $D01B
;VIC-II's Sprites Multi-Color Mode Select
vic_sprites_multicolor_mode = $D01C
;VIC-II's Background Color Register
vic_background_color = $D021
;VIC-II's Border Color Register
vic_border_color = $D020
;VIC-II's Multi-Color Register 1
vic_sprites_multicolor1 = $D025
;VIC-II's Multi-Color Register 2
vic_sprites_multicolor2 = $D026
;VIC-II's Sprite0 Individual Color
vic_sprite0_color = $D027
;VIC-II's Sprite1 Individual Color
vic_sprite1_color = $D028
;VIC-II's Sprite2 Individual Color
vic_sprite2_color = $D029
;VIC-II's Sprite3 Individual Color
vic_sprite3_color = $D02A
;VIC-II's Sprite4 Individual Color
vic_sprite4_color = $D02B
;VIC-II's Sprite5 Individual Color
vic_sprite5_color = $D02C
;VIC-II's Sprite6 Individual Color
vic_sprite6_color = $D02D
;VIC-II's Sprite7 Individual Color
vic_sprite7_color = $D02E
;VIC-II's Sprites Expand 2x Horizontal (X)
vic_sprites_expand_x = $D01D
;VIC-II's Sprites Expand 2x Vertical (Y)
vic_sprites_expand_y = $D017
;VIC-II's Sprites 0-7 MSBs of X coordinates ("Bit8")
vic_sprites_x_position_msb = $D010
;VIC-II's Sprite 0 X Position Register [PAL 0-504 with "Bit8"]
vic_sprite0_x_position = $D000
;VIC-II's Sprite 0 Y Position Register [PAL 0-255]
vic_sprite0_y_position = $D001
;VIC-II's Sprite 1 X Position Register [PAL 0-504 with "Bit8"]
vic_sprite1_x_position = $D002
;VIC-II's Sprite 1 Y Position Register [PAL 0-255]
vic_sprite1_y_position = $D003
;VIC-II's Sprite 2 X Position Register [PAL 0-504 with "Bit8"]
vic_sprite2_x_position = $D004
;VIC-II's Sprite 2 Y Position Register [PAL 0-255]
vic_sprite2_y_position = $D005
;VIC-II's Sprite 3 X Position Register [PAL 0-504 with "Bit8"]
vic_sprite3_x_position = $D006
;VIC-II's Sprite 3 Y Position Register [PAL 0-255]
vic_sprite3_y_position = $D007

;KERNAL's Interrupt Vector low byte
kernal_interrupt_vector_lbyte = $0314
;KERNAL's Interrupt Vector high byte
kernal_interrupt_vector_hbyte = $0315
;KERNAL's routine to restore CPU registers after the interrupt
kernal_interrupt_clearup = $EA81


;Sprite Pointers at the end of Screen RAM
sprite0_pointer = screen_ram + $3f8
sprite1_pointer = $07F9
sprite2_pointer = $07FA
sprite3_pointer = $07FB
sprite4_pointer = $07FD

;****Custom variables and addresses****







;Brickwall's number of frames
sprite_brickwall_frames = $01
;Brickwall's address in the sprite area
sprite_brickwall_pointer = $80 + sprite_blinky_frames

;Sprites' shared background color
sprites_background_color = $00
;Sprites' shared multicolor1
sprites_multicolor1 = $06
;Sprites' shared multicolor2
sprites_multicolor2 = $01
;Blinky's individual sprite color
sprite_blinky_color = $02
;Brickwall's individual sprite color
sprite_brickwall_color = $02
;PacMan's individual sprite color
sprite_pacman_color = $07
;Frozen ghost's individual color
sprite_frozeng_color = $02

memory_frozeng_frame2 = $2200;$88*$40
memory_frozeng_frame3 = $2240;89 * $40
memory_frozeng_ftemp = $2280;$8a * $40


;*****Game constants and memory locations*****

;***Ghosts***

;Address where the state (normal/frozen) of the ghosts is stored
is_ghost_frozen = $0313
;Address where the boolean used to check for ghosts transformation is stored
is_ghost_transforming = $0334
;Address where the number of delayed refreshes is stored
ghost_transform_refreshes = $0335
;Constant representing the number of delayed refreshes is stored
ghost_transform_needed_r = $7f
;Address where the boolean indicating whether a ghost is blocked from moving is stored
ghost_blocked = $0337

;Blinky's number of frames
sprite_blinky_frames = $02
;Current frame of the Blinky's ghost animation
sprite_blinky_current_frame = $fb
;Blinky's address in the sprite area
sprite_blinky_pointer = $2000 / $40

;Frozen ghost's address in the sprite area
sprite_frozeng_pointer = $83 + $04


;Number of screen refreshes passed since the last frame switch of the Blinky's sprite
animation_blinky_refreshes = $fc
animation_blinky_delayed = $08


;***PacMan***

;Address where the boolean indicating whether PacMan is blocked from moving is stored
pacman_blocked = $0336

;PacMan's number of frames
sprite_pacman_frames = $0338
;Number of PacMan frames when he is alive and eating
pacman_frames_alive = $04
;Number of frames when PacMan will die
pacman_frames_dead = $07
;Current frame of the PacMan's character animation
sprite_pacman_current_frame = $fd
;PacMan's address in the sprite memory area
sprite_pacman_pointer = $80 + $03
;PacMan's dead animation address in the sprite memory area
sprite_pac_dead_pointer = $8f; absolute address $23C0

;Number of screen refreshes passed since the last frame switch of the Blinky's sprite
animation_pacman_refreshes = $fe
animation_pacman_delayed = $05


;Character data resides starting at $3800
*=$3800
incbin  "pacharset.cst",1,1
incbin  "pacmaze1.sdd",1,1

;Load Sprite Data into memory location starting at $2000

*=$2000

incbin  "blinky1.spt",1,2,true
incbin  "brickwall.spt",1,1,true
incbin  "pacman.spt",1,4,true
incbin  "frozenghost.spt",1,4,true
incbin  "questionbubble.spt",1,4,true
incbin  "pacman.spt",5,12,true

;The program starts at $1001

*=$1001


program_start           sei                                     ;Set Interrupt Disabled flag in P register


                        jsr     initialize_sprites              ;Jump to sprite initialization subroutine and return back
                        jsr     clear_screen                    ;Jump to clear the screen and return back
                        jsr     write_text                      ;Jump to write lines of text and return back
                        
                        lda     #$04                            ;Save $01 to A, used to mark when to and when not to update the sprite frame
                        sta     animation_blinky_refreshes      ;Save the boolean value to the allocated memory address

                        lda     #$01                            ;$01 means Blinky is frozen
                        sta     is_ghost_frozen                 ;Store it into the memory to keep track of its state
                        sta     ghost_transform_refreshes       ;Store $01 to ghost transformation refreshes counter
                        
                        lda     #$00                            ;$00 means Blinky is not transforming between frozen and normal state
                        sta     is_ghost_transforming           ;Store it to the memory to keep track of it
                        sta     pacman_blocked
                        sta     ghost_blocked

                        ldy     #$7f                            ;$7f = 01111111
                        sty     cia1_icr                        ;Write to CIA1 Interrupt Control Register, turn off interrupts by Bit7=0
                        sty     cia2_icr                        ;Write to CIA2 Interrupt Control Register, turn off interrupts by Bit7=0
                        lda     cia1_icr                        ;Reading the ICR register cancels all pending CIA1 IRQs
                        lda     cia2_icr                        ;Reading the ICR register cancels all pending CIA2 IRQs

                        lda     #$05                            ;$00=00000101, Bit0=1 means receive an interrupt on raster beam position, Bit2=0 means on sprites collision
                        sta     vic_icr                         ;Save the byte into VIC-II's Interrupt Control Register

                        lda     #$00                            ;VIC-II IRQ should trigger on row 0
                        sta     vic_raster_position             ;Write to raster position register of VIC-II
                
                        lda     vic_cr1                         ;Load VIC-II's "Control Register 1" into A
                        and     #$7f                            ;Bit7 of VIC-II's CR1 is actually "Bit8" of $D012 raster position register
                        sta     vic_cr1                         ;Clear the Bit7 to have the correct row 0 selected for VIC-II's IRQ

                        lda     #<irq                           ;Load low byte of raster IRQ's address
                        ldx     #>irq                           ;Load high byte of raster IRQ's address
                        sta     kernal_interrupt_vector_lbyte   ;Save low byte of custom raster IRQ to KERNAL's interrupt vector's low byte of address
                        stx     kernal_interrupt_vector_hbyte   ;Save high byte of custom raster IRQ to KERNAL's interrupt vector's high byte of address

                        cli                                     ;Clear Interrupt Disabled flag in P register

                        jmp     *


initialize_sprites      lda     #$01                            ;Load $01 into A, this is the starting frame of Blinky's sprite
                        sta     sprite_blinky_current_frame     ;Store it as Blinky's current (and in this case, starting) frame

                        lda     #sprite_frozeng_pointer         ;Load Blinky's frozen ghost sprite pointer into A
                        sta     sprite2_pointer                 ;Store it into the second sprite's (Sprite 1) pointer location

                        lda     #sprite_pacman_pointer          ;Load PacMan's sprite pointer into A
                        sta     sprite0_pointer                 ;Store it into the first sprite's (Sprite0) pointer location

                        lda     #pacman_frames_alive
                        sta     sprite_pacman_frames            ;

                        lda     #sprite_brickwall_pointer       ;Load Brickwall sprite pointer into A
                        sta     sprite1_pointer                 ;Store it into the first sprite's (Sprite 0) pointer location
                        sta     sprite3_pointer                 ;Store it into the third sprite's (Sprite 2) pointer location

                        lda     #$0f                            ;$07=00001111
                        sta     vic_sprites_enable              ;Store this into VIC-II's Sprite Display Enable register to enable Sprite 0,1,2

                        lda     #$0f                            ;$01=00001111
                        sta     vic_sprites_multicolor_mode     ;Store this into VIC-II's Sprites Multi-Color Mode Select register to enable multicolor for Sprite 0,1,2 

                        lda     #$00                            ;$00=00000000, Bit=0 means Sprite0,1,2 will have priority over background color
                        sta     vic_sprites_priority            ;Store it to the VIC-II's Sprite to Background Display Priority Register

                        lda     #sprites_background_color       ;Load chosen sprites' shared background color
                        sta     vic_background_color            ;Store it to the VIC-II's Background Color register

                        lda     #sprites_multicolor1            ;Load chosen sprites' shared multicolor1
                        sta     vic_sprites_multicolor1         ;Store it to the VIC-II's Multi-Color Register 1

                        lda     #sprites_multicolor2            ;Load chosen sprites' shared multicolor2
                        sta     vic_sprites_multicolor2         ;Store it to the VIC-II's Multi-Color Register 2

                        lda     #sprite_frozeng_color           ;Load Blinky's frozen ghost's chosen individual color
                        sta     vic_sprite2_color               ;Store to VIC-II's Sprite1 Individual Color register

                        lda     #sprite_pacman_color            ;Load PacMan's individual color
                        sta     vic_sprite0_color               ;Store to VIC-II's Sprite0 Individual Color register

                        lda     #sprite_brickwall_color         ;Load Brickwall's chosen individual color
                        sta     vic_sprite1_color               ;Store to VIC-II's Sprite0 Individual Color register
                        sta     vic_sprite3_color               ;Store to VIC-II's Sprite2 Individual Color register

                        lda     #$0A                            ;$06=00001010, Sprite 1 should be expanded by X and Y
                        sta     vic_sprites_expand_x            ;Expand X by saving the byte to Expand X register of VIC-II
                        sta     vic_sprites_expand_y            ;Expand Y by saving the byte to Expand Y register of VIC-II

                        lda     #$88
                        sta     vic_sprite1_x_position          ;Store it to Sprite 1's X position register
                        
                        lda     #$98
                        sta     vic_sprite1_y_position          ;Store it to Sprite 1's Y position register

                        lda     #$b8
                        sta     vic_sprite3_x_position          ;Store it to Sprite 3's X position register
                        
                        lda     #$81
                        sta     vic_sprite3_y_position          ;Store it to Sprite 3's Y position register

reset_actors_pos        lda     #$05                            ;$00 = 00000101, Bit0 = 0 so Sprite0 (Blinky) will have X-coordinate's "Bit8" = 0
                        sta     vic_sprites_x_position_msb      ;Blinky will be on the left side of the screen because X coord "Bit8" = 0

                        lda     #$f8
                        sta     vic_sprite0_x_position          ;Store it to Sprite 0's X position register
                        
                        lda     #$9a
                        sta     vic_sprite0_y_position          ;Store it to Sprite 0's Y position register

                        lda     #$a8
                        sta     vic_sprite2_x_position          ;Store it to Sprite 2's X position register
                        
                        lda     #$9a
                        sta     vic_sprite2_y_position          ;Store it to Sprite 2's Y position register

                        rts


clear_screen            ldx     #$00                            ;Load black color ($00) to register X
                        stx     vic_border_color                ;Write to border color

loop_clear              lda     #$20                            ;Load spacebar symbol ($20) into A
                        sta     $0400,x                         ;Store spacebar symbol to Screen RAM addresses $0400-$04FF
                        sta     $0500,x                         ;Store spacebar symbol to Screen RAM addresses $0500-$05FF
                        sta     $0600,x                         ;Store spacebar symbol to Screen RAM addresses $0600-$06FF
                        sta     $06E8,x                         ;Store spacebar symbol to Screen RAM addresses $06E8-$07E7

                        lda     #$02                            ;Load black color ($00) to A
                        sta     $D800,x                         ;Store black color to Color RAM addresses $D800-$D8FF
                        sta     $D900,x                         ;Store black color to Color RAM addresses $D900-$D9FF
                        sta     $DA00,x                         ;Store black color to Color RAM addresses $DA00-$DAFF
                        sta     $DAE8,x                         ;Store black color to Color RAM addresses $DAE8-$DBE7

                        inx                                     ;Increment iterator in register X
                        bne     loop_clear                      ;If overflow to zero hasn't happened yet, continue the loop

                        rts

write_text              ldx     #$00                            ;Load iterator into register X
                        
loop_text               lda     intro_text_line1,x              ;Load char from the first line at location of iterator into A
                        sta     $0428,x                         ;Save the loaded char into location relative to position in the string
                        lda     intro_text_line2,x              ;Load char from the second line at location of iterator into A
                        sta     $0478,x                         ;Save the loaded char into location relative to position in the string
                        lda     intro_text_line3,x              ;Load char from the third line at location of iterator into A
                        sta     $0770,x                         ;Save the loaded char into location relative to position in the string

                        inx                                     ;Increment iterator in register X
                        cpx     #$28                            ;Compare the current iteration with the 40 ($28) iterations
                        bne     loop_text                       ;If all the iterations are not completed, loop again

                        
                        rts

reset_blinky_x          lda     vic_sprites_x_position_msb      ;Load the current MSB bits of X-coordinates of sprites
                        eor     #$04                            ;EOR $02=00000100 ensures the toggle of the Blinky's X-coordinate's MSB
                        sta     vic_sprites_x_position_msb      ;Store the updated MSB value back to its location

update_blinky           lda     ghost_blocked
                        bne     animate_blinky

                        dec     vic_sprite2_x_position          ;Decrement Blinky's X position
                        beq     reset_blinky_x                  ;If it has reached 0, toggle its MSB

                        
animate_blinky          lda     animation_blinky_refreshes      ;Load Blinky's number of screen refreshed passed since last frame switch
                        cmp     #animation_blinky_delayed       ;Compare with the number of refreshes Blinky needs to wait for his frame to switch forward
                        bne     skip_blinky_animation           ;If the delay has not passed, skip the animation by returning early from the subroutine

                        lda     is_ghost_frozen                 ;If the delay has passed, load Blinky's current state
                        beq     continue_animate_blinky         ;If he's not frozen, go on to animate him
                        lda     ghost_transform_refreshes       ;
                        and     #$fe                            ;$fe=11111110
                        beq     continue_animate_blinky         ;
                        inc     ghost_transform_refreshes
                        
continue_animate_blinky lda     #$01                            ;If the delay has passed, reset the delayed screen refreshes counter
                        sta     animation_blinky_refreshes      ;Store it to the memory location we use to track it
                        lda     sprite_blinky_current_frame     ;Load Blinky's current frame that is being displayed
                        cmp     #sprite_blinky_frames           ;Check if it is the last frame
                        bne     next_frame_blinky               ;If it is not the last frame, move on to the next one

reset_blinky_frames     lda     #$01                            ;Load $01 to A, this is the first frame of Blinky's sprites
                        sta     sprite_blinky_current_frame     ;Store it to Blinky's current frame

                        lda     is_ghost_frozen                 ;Load the value that keeps track of Blinky's frozen state
                        beq     reset_blinky_u_frames           ;If Blinky is not in frozen state, branch to set normal ghost's appearance
                        
                        lda     #sprite_frozeng_color           ;Load Blinky's frozen color
                        sta     vic_sprite2_color               ;Store it to Sprite2 color register of VIC-II
                        lda     #sprite_frozeng_pointer         ;Load the first of Blinky's frozen sprites pointer
                        sta     sprite2_pointer                 ;Store it as the current Sprite1 pointer in Screen RAM, this resets Blinky's appearance to the starting frame

                        rts
                        

reset_blinky_u_frames   lda     #sprite_blinky_color            ;Load Blinky's normal color
                        sta     vic_sprite2_color               ;Store it to Sprite2 color register of VIC-II
                        lda     #sprite_blinky_pointer          ;Load the first of Blinky's sprites pointer
                        sta     sprite2_pointer                 ;Store it as the current Sprite1 pointer in Screen RAM, this resets Blinky's appearance to the starting frame

                        rts
                        
next_frame_blinky       inc     sprite2_pointer                 ;Increment the memory location stored in the Sprite 0 pointer register to point to the next frame
                        inc     sprite_blinky_current_frame     ;Increment the value used to keep track of Blinky's current frame

skip_blinky_animation   inc     animation_blinky_refreshes
                        rts
;
reset_pacman_x          lda     vic_sprites_x_position_msb      ;Load the current MSB bits of X-coordinates of sprites
                        eor     #$01                            ;EOR $02=00000100 ensures the toggle of the Blinky's X-coordinate's MSB
                        sta     vic_sprites_x_position_msb      ;Store the updated MSB value back to its location

update_pacman           lda     pacman_blocked                  ;If PacMan is blocked by the wall
                        bne     animate_pacman                  ;In that case, do not move PacMan any more

                        dec     vic_sprite0_x_position          ;Decrement PacMan's X position
                        beq     reset_pacman_x                  ;If it has reached 0, toggle its MSB

animate_pacman          lda     animation_pacman_refreshes      ;Load PacMan's number of screen refreshed passed since last frame switch
                        cmp     #animation_pacman_delayed       ;Compare with the number of refreshes Blinky needs to wait for his frame to switch forward
                        bne     skip_pacman_animation           ;If the delay has not passed, skip the animation by returning early from the subroutine
                        lda     #$01                            ;If the delay has passed, reset the delayed screen refreshes counter
                        sta     animation_pacman_refreshes      ;Store it to the memory location we use to track it
                        lda     sprite_pacman_current_frame     ;Load PacMan's current frame that is being displayed
                        cmp     sprite_pacman_frames            ;Check if it is the last frame
                        bne     next_frame_pacman               ;If it is not the last frame, move on to the next one

reset_pacman_frames     lda     ghost_blocked
                        beq     reload_alive_pac_frames
                        lda     #$0e
                        sta     vic_sprites_enable
                        rts

reload_alive_pac_frames lda     #$01                            ;Load $01 to A, this is the first frame of Blinky's sprites
                        sta     sprite_pacman_current_frame     ;Store it to PacMan's current frame
                        lda     #sprite_pacman_pointer          ;Load the first of PacMan's sprites pointer
                        sta     sprite0_pointer                 ;Store it as the current Sprite1 pointer in Screen RAM, this resets Blinky's appearance to the starting frame

                        rts
                        
next_frame_pacman       inc     sprite0_pointer                 ;Increment the memory location stored in the Sprite 0 pointer register to point to the next frame
                        inc     sprite_pacman_current_frame     ;Increment the value used to keep track of Blinky's current frame

skip_pacman_animation   inc     animation_pacman_refreshes
                        rts

ghost_transform         ldx     #$00                            ;Initialize memory copy loop iterator

sprite_copy_loop        lda     memory_frozeng_frame2,x         ;Load X byte of frame 2 of frozen ghost into A
                        sta     memory_frozeng_ftemp,x          ;Store X byte of frame 2 into X byte of free temporary memory
                        lda     memory_frozeng_frame3,x         ;Load X byte of frame 3 of frozen ghost into A
                        sta     memory_frozeng_frame2,x         ;Overwrite frozen frame 2 byte X with frame3 byte X
                        lda     memory_frozeng_ftemp            ;Load the "backup" temoporary byte of frame 2
                        sta     memory_frozeng_frame3           ;Overwrote frozen frame 3 byte X with that frame
                        inx                                     ;Increment iterator
                        cpx     #$40                            ;If we have reached #$40, means all 64 bytes were copied
                        bne     sprite_copy_loop                ;If we have not, loop again

                        rts


irq                     lda     vic_irr                         ;To acknowledge and clear the interrupt from VIC-II, its Interrupt Request Register must be read
                        sta     vic_irr                         ;And written back into the same location

                        and     #$04                            ;$04=00000100, AND on A checks for Bit3=1
                        beq     raster_irq                      ;If Bit3=1, means a sprite collision interrupt has happened, otherwise it was a raster line interrupt
                        lda     vic_scr                         ;Load VIC-II's Sprite Collision Detection register
                        cmp     #$09                            ;Check if it's the needed collision now that other bits are cleared
                        beq     pacman_wall_irq                 ;If it is, jump to interrupt handler for this situation
                        cmp     #$0d                            ;Otherwise check for Blinky-PacMan collision, $0d=00001101
                        bne     return_from_irq                 ;If it's neither this one, return from IRQ

blinky_pacman_irq       lda     ghost_blocked                   ;Check if the collision was already handled
                        bne     return_from_irq                 ;If so, return from IRQ
                        lda     #$01                            ;Otherwise, $01 will be a flag to signal that collision has occured and the ghost (Blinky) was blocked
                        sta     ghost_blocked                   ;Store to memory location
                        sta     sprite_pacman_current_frame     ;Initialize PacMan's frame to first one
                        lda     #pacman_frames_dead             ;Load the number of frames required by the dieing PacMan sprite animation
                        sta     sprite_pacman_frames            ;Store it at the memory location of PacMan's sprite frame number
                        lda     #sprite_pac_dead_pointer        ;Load the address of dieing PacMan animation's first sprite
                        sta     sprite0_pointer                 ;Redirect Sprite 0 pointer to the fetched address
                        jmp     kernal_interrupt_clearup        ;Return from IRQ

pacman_wall_irq         lda     is_ghost_frozen                 ;Load the value used to track Blinky's state
                        cmp     #$00                            ;Check if it's 0 which would mean that PacMan has already hit the wall and Blinky unfroze
                        beq     return_from_irq                 ;If it did already happen, branch to KERNAL's interrupt clear routine

                        lda     ghost_transform_refreshes       ;If this is the first interrupt triggered on collision between PacMan and the wall or collision occured earlier but Blinky is still transitioning
                        cmp     ghost_transform_needed_r        ;Check if the transformation has some to an end in which case it will hold the final refresh number
                        beq     unfreeze_blinky                 ;If it has just ended, unfreeze Blinky
                        cmp     #$01                            ;Check if this is the first (and only time) to perform initialization of transformation sequence
                        bne     return_from_irq                 ;If it's not, it means the transformation was initiated and is taking place right now, in that case no action is needed on this collision IRQ call
                        lda     #$01                            ;$01 = PacMan is blocked by the wall
                        sta     pacman_blocked                  ;Store it to memory to keep track of it
                        jsr     ghost_transform                 ;Otherwise, if this is the first time, initialization must be performed
                        inc     ghost_transform_refreshes       ;Increment the refresh, even though it did not happen, just to prevent calling init routine in the next collision IRQ
                        jmp     kernal_interrupt_clearup        ;Jump to KERNAL's interrupt clearup routine


unfreeze_blinky         lda     #$00                            
                        sta     is_ghost_frozen                 ;Then set the value to 0 to indicate that Blinky should unfreeze and PacMan stop moving
                        sta     ghost_transform_refreshes       ;Prevent further transformation refresh counting
                        jsr     ghost_transform                 ;Revert the effects of ghost transformation
                        jsr     reset_blinky_frames             ;Blinky starts with the first frame

return_from_irq         jmp     kernal_interrupt_clearup        ;Jump to KERNAL's interrupt clearup routine


raster_irq              jsr     update_blinky                   ;Update Blinky's position on the screen and animation, return when done
                        jsr     update_pacman                   ;Update PacMan's position on the screen and animation, return when done
                        
                        ;Once all the subroutines calls are done, top&bottom border exploit should be performed

                        lda     #$00                            ;Load $00 into A, this is a blank byte to prevent VIC-II from drawing garbage found in the last byte of active RAM bank
                        sta     $3FFF                           ;Store the byte into the latest byte of the VIC-II's active selected RAM bank (bank 3)

wait_scanline_249       lda     #$F9                            ;Wait until scanline 249 ($F9)
                        cmp     vic_raster_position             ;Reading the VIC-II's Raster Position register returns the current raster line
                        bne     wait_scanline_249               ;If it's not the 249th line, wait more

                        lda     vic_cr1                         ;Load the VIC-II's Control Register 1 to change the ROWS mode
                        and     #$f7                            ;$f7 = 11110111, this clears the Bit3, thus setting ROWS mode to 24
                        sta     vic_cr1                         ;Store the updated value back into VIC-II's Control Register 1

wait_scanline_255       lda     #$ff                            ;Wait until scanline 255 ($FF)
                        cmp     vic_raster_position             ;Reading the VIC-II's Raster Position register returns the current raster line
                        bne     wait_scanline_255               ;If it's not the 255th line, wait more

                        lda     vic_cr1                         ;Load the VIC-II's Control Register 1 to change the ROWS mode
                        ora     #$08                            ;$08 = 00001000, this sets Bit3 high again, this returning ROWS mode to 25
                        sta     vic_cr1                         ;Store the updated value back into VIC-II's Control Register 1


                        jmp     kernal_interrupt_clearup        ;Jump to KERNAL's interrupt clearup routine

                        

intro_text_line1        text    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','K','i','n','g','d','o','m',' ','o','f',' ','S','p','r','i','t','e','s',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '
intro_text_line2        text    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','G','h','a','s','t','l','y',' ','G','h','o','s','t','s',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '
intro_text_line3        text    ' ',' ',' ',' ',' ',' ',' ',' ','C','o','p','y','r','i','g','h','t',' ','(','c',')',' ','c','a','f','f','e','i','n','e','9','3',' ',' ',' ',' ',' ',' ',' ',' '