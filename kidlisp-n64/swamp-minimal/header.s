.section .boot, "ax"

.global __start
__start:
    .word 0x80371240          /* PI BSD Domain 1 register */
    .word 0x0000000F          /* Clockrate setting */
    .word 0x8000049C          /* Entry point (nuBoot address) */
    .word 0x0000144C          /* Release */
    .word 0x00000000, 0x00000000  /* CRC (filled by makemask) */
    .ascii "SWAMP-MINIMAL      "  /* Image name (20 bytes) */
    .word 0x00000000          /* Unknown */
    .word 0x0000004E          /* Cartridge */
    .ascii "NM"               /* Cartridge ID */
    .byte 0x45                /* Region: America */
    .byte 0x00                /* Version */
