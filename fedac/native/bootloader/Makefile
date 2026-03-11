# UEFI Splash Chainloader — gnu-efi based
ARCH      = x86_64
TARGET    = splash.efi

# gnu-efi paths
EFI_INC   = /usr/include/efi
EFI_INCS  = -I$(EFI_INC) -I$(EFI_INC)/$(ARCH) -I$(EFI_INC)/protocol
EFI_LIB   = /usr/lib
EFI_CRT   = $(EFI_LIB)/crt0-efi-$(ARCH).o
EFI_LDS   = $(EFI_LIB)/elf_$(ARCH)_efi.lds

CC        = gcc
CFLAGS    = -fno-stack-protector -fpic -fshort-wchar -mno-red-zone \
            -Wall -DGNU_EFI_USE_MS_ABI -DEFI_FUNCTION_WRAPPER $(EFI_INCS)
LDFLAGS   = -nostdlib -znocombreloc -T $(EFI_LDS) -shared -Bsymbolic \
            -L$(EFI_LIB) $(EFI_CRT)
LIBS      = -lefi -lgnuefi

.PHONY: all clean

all: $(TARGET)

splash.o: splash.c font8x8.h
	$(CC) $(CFLAGS) -c -o $@ $<

splash.so: splash.o
	ld $(LDFLAGS) $^ -o $@ $(LIBS)

$(TARGET): splash.so
	objcopy -j .text -j .sdata -j .data -j .dynamic \
	        -j .dynsym -j .rel -j .rela -j .reloc \
	        --target=efi-app-$(ARCH) $< $@
	@echo "Built: $@ ($$(wc -c < $@ | tr -d ' ') bytes)"

clean:
	rm -f splash.o splash.so $(TARGET)
