#!/usr/bin/env python3
"""Build a minimal PE32+ EFI application from a raw x86_64 binary."""
import struct, sys

def mkpe(code_path, out_path):
    code = open(code_path, 'rb').read()
    # Align code to 512 bytes
    code_aligned = code + b'\0' * ((-len(code)) % 512)

    # --- DOS Header (64 bytes) ---
    dos = bytearray(64)
    struct.pack_into('<H', dos, 0, 0x5A4D)      # e_magic = MZ
    struct.pack_into('<I', dos, 60, 64)          # e_lfanew -> PE header at offset 64

    # --- PE Signature (4 bytes) ---
    pe_sig = b'PE\0\0'

    # --- COFF Header (20 bytes) ---
    code_section_offset = 64 + 4 + 20 + 112 + 40  # dos + sig + coff + opt + 1 section header
    # Round up to 512
    header_size = ((code_section_offset + 511) // 512) * 512

    coff = bytearray(20)
    struct.pack_into('<H', coff, 0, 0x8664)      # Machine = AMD64
    struct.pack_into('<H', coff, 2, 1)            # NumberOfSections = 1
    struct.pack_into('<H', coff, 16, 112)         # SizeOfOptionalHeader
    struct.pack_into('<H', coff, 18, 0x0206)      # Characteristics: EXEC | LINE_NUMS_STRIPPED | LARGE_ADDRESS_AWARE

    # --- Optional Header PE32+ (112 bytes) ---
    opt = bytearray(112)
    struct.pack_into('<H', opt, 0, 0x020B)        # PE32+ magic
    struct.pack_into('<I', opt, 16, header_size)   # AddressOfEntryPoint = start of .text
    struct.pack_into('<I', opt, 20, header_size)   # BaseOfCode
    struct.pack_into('<Q', opt, 24, 0)             # ImageBase
    struct.pack_into('<I', opt, 32, 512)           # SectionAlignment
    struct.pack_into('<I', opt, 36, 512)           # FileAlignment
    struct.pack_into('<I', opt, 56, header_size + len(code_aligned))  # SizeOfImage
    struct.pack_into('<I', opt, 60, header_size)   # SizeOfHeaders
    struct.pack_into('<H', opt, 68, 10)            # Subsystem = EFI Application
    struct.pack_into('<I', opt, 76, 0x100000)      # SizeOfStackReserve
    struct.pack_into('<I', opt, 84, 0x100000)      # SizeOfStackCommit
    struct.pack_into('<I', opt, 92, 0x100000)      # SizeOfHeapReserve
    struct.pack_into('<I', opt, 100, 0x100000)     # SizeOfHeapCommit
    struct.pack_into('<I', opt, 108, 0)            # NumberOfRvaAndSizes = 0

    # --- Section Header (.text, 40 bytes) ---
    sec = bytearray(40)
    sec[0:6] = b'.text\0'
    struct.pack_into('<I', sec, 8, len(code))      # VirtualSize
    struct.pack_into('<I', sec, 12, header_size)    # VirtualAddress
    struct.pack_into('<I', sec, 16, len(code_aligned))  # SizeOfRawData
    struct.pack_into('<I', sec, 20, header_size)    # PointerToRawData
    struct.pack_into('<I', sec, 36, 0x60000020)     # Characteristics: CODE | EXECUTE | READ

    # --- Assemble ---
    hdr = dos + pe_sig + bytes(coff) + bytes(opt) + bytes(sec)
    hdr_padded = hdr + b'\0' * (header_size - len(hdr))

    with open(out_path, 'wb') as f:
        f.write(hdr_padded)
        f.write(code_aligned)

    total = header_size + len(code_aligned)
    print(f"  {out_path}: {total} bytes (code={len(code)}, headers={header_size})")

if __name__ == '__main__':
    mkpe(sys.argv[1], sys.argv[2])
