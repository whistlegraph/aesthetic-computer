# babypat — bare-metal UEFI musical keyboard
ARCH      = x86_64
TARGET    = babypat.efi

EFI_INC   = /usr/include/efi
EFI_INCS  = -I$(EFI_INC) -I$(EFI_INC)/$(ARCH) -I$(EFI_INC)/protocol
EFI_LIB   = /usr/lib
EFI_CRT   = $(EFI_LIB)/crt0-efi-$(ARCH).o
EFI_LDS   = $(EFI_LIB)/elf_$(ARCH)_efi.lds

CC        = gcc
CFLAGS    = -Os -fno-stack-protector -fpic -fshort-wchar -mno-red-zone \
            -Wall -DGNU_EFI_USE_MS_ABI -DEFI_FUNCTION_WRAPPER $(EFI_INCS)
LDFLAGS   = -nostdlib -znocombreloc -T $(EFI_LDS) -shared -Bsymbolic \
            -L$(EFI_LIB) $(EFI_CRT)
LIBS      = -lefi -lgnuefi

.PHONY: all clean test

all: $(TARGET)
	@echo "  $(TARGET): $$(wc -c < $(TARGET) | tr -d ' ') bytes"

babypat.o: babypat.c
	$(CC) $(CFLAGS) -c -o $@ $<

babypat.so: babypat.o
	ld $(LDFLAGS) $^ -o $@ $(LIBS)

$(TARGET): babypat.so
	objcopy -j .text -j .sdata -j .data -j .dynamic \
	        -j .dynsym -j .rel -j .rela -j .reloc \
	        --target=efi-app-$(ARCH) $< $@

# Build a bootable USB image (FAT12, minimal)
babypat.img: $(TARGET)
	dd if=/dev/zero of=$@ bs=1M count=4
	mkfs.vfat -F 12 $@
	mmd -i $@ ::EFI ::EFI/BOOT
	mcopy -i $@ $(TARGET) ::EFI/BOOT/BOOTX64.EFI
	@echo "  $@: $$(wc -c < $@ | tr -d ' ') bytes (bootable image)"

# Test in QEMU with OVMF
test: babypat.img
	qemu-system-x86_64 \
	  -bios /usr/share/OVMF/OVMF_CODE.fd \
	  -drive format=raw,file=babypat.img \
	  -m 256M \
	  -pcspk passthrough \
	  -display gtk

# --- Tiny build (no gnu-efi, hand-rolled PE) ---
TINY_TARGET = babypat-tiny.efi
TINY_CFLAGS = -Os -ffreestanding -fno-stack-protector -fno-pic -fno-pie \
              -fno-asynchronous-unwind-tables -mno-red-zone -mno-sse \
              -fno-builtin -nostdinc -mabi=ms -Wall \
              -I/usr/lib/gcc/x86_64-redhat-linux/15/include

babypat-tiny.o: babypat-tiny.c
	$(CC) $(TINY_CFLAGS) -c -o $@ $<

babypat-tiny.elf: babypat-tiny.o
	ld -nostdlib -T tiny.ld -o $@ $<

babypat-tiny.bin: babypat-tiny.elf
	objcopy -O binary $< $@

$(TINY_TARGET): babypat-tiny.bin mkpe.py
	python3 mkpe.py $< $@

babypat-tiny.img: $(TINY_TARGET)
	dd if=/dev/zero of=$@ bs=1k count=128
	mkfs.vfat -F 12 $@
	mmd -i $@ ::EFI ::EFI/BOOT
	mcopy -i $@ $(TINY_TARGET) ::EFI/BOOT/BOOTX64.EFI
	@echo "  $@: $$(wc -c < $@ | tr -d ' ') bytes (bootable image)"

tiny: $(TINY_TARGET)
	@echo "  $(TINY_TARGET): $$(wc -c < $(TINY_TARGET) | tr -d ' ') bytes"

clean:
	rm -f *.o *.so *.elf *.bin $(TARGET) $(TINY_TARGET) *.img
