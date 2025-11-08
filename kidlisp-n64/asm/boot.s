# Minimal N64 ROM in pure MIPS assembly
# Based on classic N64 homebrew examples

.set noreorder
.set noat

.section .boot
.global _start

_start:
    # Set up stack pointer
    li $sp, 0x80400000
    
    # Initialize video - set up VI registers
    li $t0, 0xA4400000      # VI base address
    
    # VI_CTRL - enable video, 16-bit color
    li $t1, 0x3002
    sw $t1, 0x0($t0)
    
    # VI_ORIGIN - framebuffer address (0x80400000 in RDRAM)
    li $t1, 0x00400000
    sw $t1, 0x4($t0)
    
    # VI_WIDTH - 320 pixels wide
    li $t1, 320
    sw $t1, 0x8($t0)
    
    # VI_V_INTR - vertical interrupt at line 2
    li $t1, 2
    sw $t1, 0xC($t0)
    
    # VI_V_CURRENT - current scanline (read-only, but we can check)
    
    # VI_BURST - color burst timing
    li $t1, 0x03E52239
    sw $t1, 0x14($t0)
    
    # VI_V_SYNC - vertical sync (525 for NTSC)
    li $t1, 0x020D
    sw $t1, 0x18($t0)
    
    # VI_H_SYNC - horizontal sync
    li $t1, 0x00000C15
    sw $t1, 0x1C($t0)
    
    # VI_H_SYNC_LEAP
    li $t1, 0x0C150C15
    sw $t1, 0x20($t0)
    
    # VI_H_VIDEO - horizontal video timing
    li $t1, 0x006C02EC
    sw $t1, 0x24($t0)
    
    # VI_V_VIDEO - vertical video timing (240 lines)
    li $t1, 0x002501FF
    sw $t1, 0x28($t0)
    
    # VI_V_BURST
    li $t1, 0x000E0204
    sw $t1, 0x2C($t0)
    
    # VI_X_SCALE - horizontal scale (1:1)
    li $t1, 0x00000200
    sw $t1, 0x30($t0)
    
    # VI_Y_SCALE - vertical scale (1:1)
    li $t1, 0x00000400
    sw $t1, 0x34($t0)

    # Clear framebuffer with a test pattern
    li $a0, 0x80400000      # Framebuffer start
    li $a1, 320*240         # Number of pixels (320x240)
    li $a2, 0xF800          # Red color (16-bit RGB5551)
    
clear_loop:
    sh $a2, 0($a0)          # Store halfword (16-bit pixel)
    addiu $a0, $a0, 2       # Move to next pixel
    addiu $a1, $a1, -1      # Decrement counter
    bnez $a1, clear_loop    # Loop if not zero
    nop                     # Branch delay slot

# Main loop - draw animated pattern
main_loop:
    li $t0, 0x80400000      # Framebuffer base
    li $t1, 0               # Y counter
    
y_loop:
    li $t2, 0               # X counter
    
x_loop:
    # Calculate color based on position
    # color = ((x ^ y) & 0x1F) << 11  (red channel)
    xor $t3, $t2, $t1       # x XOR y
    andi $t3, $t3, 0x1F     # Mask to 5 bits
    sll $t3, $t3, 11        # Shift to red position
    ori $t3, $t3, 0x0001    # Set alpha bit
    
    # Store pixel
    sh $t3, 0($t0)
    addiu $t0, $t0, 2       # Next pixel
    
    # Increment X
    addiu $t2, $t2, 1
    li $t4, 320
    bne $t2, $t4, x_loop
    nop
    
    # Increment Y
    addiu $t1, $t1, 1
    li $t4, 240
    bne $t1, $t4, y_loop
    nop
    
    # Simple delay
    li $t0, 0x100000
delay_loop:
    addiu $t0, $t0, -1
    bnez $t0, delay_loop
    nop
    
    # Loop forever
    j main_loop
    nop

.section .rodata
.align 4
rom_header:
    # N64 ROM header (simplified)
    .word 0x80371240    # PI_BSD_DOM1
    .word 0x0000000F    # Clock rate
    .word _start        # Entry point
    .word 0x00001444    # Release
    .word 0x00000000    # CRC1
    .word 0x00000000    # CRC2
    .space 8            # Reserved
    .ascii "TEST ROM        " # Game title (20 bytes)
    .space 7            # Reserved
    .ascii "N"          # Media format
    .ascii "  "         # Game ID
    .ascii "E"          # Region
    .byte 0             # Version
