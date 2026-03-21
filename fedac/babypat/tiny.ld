/* Minimal EFI linker script — no gnu-efi */
OUTPUT_FORMAT("elf64-x86-64")
ENTRY(efi_main)

SECTIONS {
    . = 0;
    .text : { *(.text .text.*) }
    .rodata : { *(.rodata .rodata.*) }
    .data : { *(.data .data.*) }
    .bss : { *(.bss .bss.*) }
    .reloc : { }
    /DISCARD/ : { *(.comment) *(.note.*) *(.eh_frame*) }
}
