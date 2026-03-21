/*
 * Minimal N64 ROM in Pure Assembly
 * Commercial-grade compatibility for n64wasm
 */

.set noreorder
.set noat

/* Memory Map */
.equ SCREEN_WIDTH,  320
.equ SCREEN_HEIGHT, 240
.equ FRAMEBUFFER,   0x80400000
.equ VI_BASE,       0xA4400000
.equ VI_STATUS,     0x00
.equ VI_ORIGIN,     0x04
.equ VI_WIDTH,      0x08
.equ VI_V_INTR,     0x0C
.equ VI_V_CURRENT,  0x10
.equ VI_BURST,      0x14
.equ VI_V_SYNC,     0x18
.equ VI_H_SYNC,     0x1C
.equ VI_H_SYNC_LEAP, 0x20
.equ VI_H_VIDEO,    0x24
.equ VI_V_VIDEO,    0x28
.equ VI_V_BURST,    0x2C
.equ VI_X_SCALE,    0x30
.equ VI_Y_SCALE,    0x34

/* ROM Header will be inserted here by build script */

.section .text
.global _start

_start:
    /* Set up stack */
    li $sp, 0x80400000
    
    /* Initialize Video Interface */
    jal init_video
    nop
    
    /* Clear framebuffer to green */
    li $a0, 0x07E0      /* Green color (RGB565) */
    jal clear_screen
    nop
    
    /* Draw a red box */
    li $a0, 100         /* x */
    li $a1, 80          /* y */
    li $a2, 120         /* width */
    li $a3, 80          /* height */
    li $t0, 0xF800      /* Red color */
    jal draw_box
    nop
    
    /* Main loop */
main_loop:
    /* Wait for vblank */
    jal wait_vblank
    nop
    
    /* Loop forever */
    j main_loop
    nop

/*
 * Initialize Video Interface for 320x240, 16bpp
 */
init_video:
    li $t0, VI_BASE
    
    /* VI_STATUS - 16-bit color, anti-alias off */
    li $t1, 0x0000320E
    sw $t1, VI_STATUS($t0)
    
    /* VI_ORIGIN - framebuffer address */
    li $t1, (FRAMEBUFFER & 0x00FFFFFF)
    sw $t1, VI_ORIGIN($t0)
    
    /* VI_WIDTH - 320 pixels */
    li $t1, SCREEN_WIDTH
    sw $t1, VI_WIDTH($t0)
    
    /* VI_V_INTR - vertical interrupt at line 2 */
    li $t1, 0x00000002
    sw $t1, VI_V_INTR($t0)
    
    /* VI_BURST */
    li $t1, 0x03E52239
    sw $t1, VI_BURST($t0)
    
    /* VI_V_SYNC - 525 for NTSC */
    li $t1, 0x0000020D
    sw $t1, VI_V_SYNC($t0)
    
    /* VI_H_SYNC */
    li $t1, 0x00000C15
    sw $t1, VI_H_SYNC($t0)
    
    /* VI_H_SYNC_LEAP */
    li $t1, 0x0C150C15
    sw $t1, VI_H_SYNC_LEAP($t0)
    
    /* VI_H_VIDEO */
    li $t1, 0x006C02EC
    sw $t1, VI_H_VIDEO($t0)
    
    /* VI_V_VIDEO - 240 lines */
    li $t1, 0x002501FF
    sw $t1, VI_V_VIDEO($t0)
    
    /* VI_V_BURST */
    li $t1, 0x000E0204
    sw $t1, VI_V_BURST($t0)
    
    /* VI_X_SCALE - 1:1 horizontal */
    li $t1, 0x00000200
    sw $t1, VI_X_SCALE($t0)
    
    /* VI_Y_SCALE - 1:1 vertical */
    li $t1, 0x00000400
    sw $t1, VI_Y_SCALE($t0)
    
    jr $ra
    nop

/*
 * Clear screen with color
 * $a0 = color (RGB565)
 */
clear_screen:
    li $t0, FRAMEBUFFER
    li $t1, (SCREEN_WIDTH * SCREEN_HEIGHT)
    
clear_loop:
    sh $a0, 0($t0)
    addiu $t0, $t0, 2
    addiu $t1, $t1, -1
    bnez $t1, clear_loop
    nop
    
    jr $ra
    nop

/*
 * Draw filled rectangle
 * $a0 = x, $a1 = y, $a2 = width, $a3 = height
 * $t0 = color (must be set before call)
 */
draw_box:
    move $t1, $a1           /* y counter */
    add $t2, $a1, $a3       /* y_end */
    
box_y_loop:
    move $t3, $a0           /* x counter */
    add $t4, $a0, $a2       /* x_end */
    
box_x_loop:
    /* Calculate framebuffer address: FB + (y * 320 + x) * 2 */
    li $t5, SCREEN_WIDTH
    mult $t1, $t5
    mflo $t5
    add $t5, $t5, $t3
    sll $t5, $t5, 1
    li $t6, FRAMEBUFFER
    add $t6, $t6, $t5
    
    /* Write pixel */
    sh $t0, 0($t6)
    
    /* Next x */
    addiu $t3, $t3, 1
    bne $t3, $t4, box_x_loop
    nop
    
    /* Next y */
    addiu $t1, $t1, 1
    bne $t1, $t2, box_y_loop
    nop
    
    jr $ra
    nop

/*
 * Wait for vertical blank
 */
wait_vblank:
    li $t0, VI_BASE
wait_vblank_loop:
    lw $t1, VI_V_CURRENT($t0)
    andi $t1, $t1, 0x3FF
    li $t2, 240
    blt $t1, $t2, wait_vblank_loop
    nop
    
    jr $ra
    nop

/* Padding to align data */
.align 4
