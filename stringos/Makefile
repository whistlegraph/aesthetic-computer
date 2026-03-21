ARCH            = x86_64
TARGET          = $(ARCH)-elf

CC              = gcc
LD              = ld

CFLAGS          = -I/usr/include/efi -I/usr/include/efi/$(ARCH) -I/usr/include/efi/protocol -fpic -ffreestanding -fshort-wchar -mno-red-zone
LDFLAGS         = -nostdlib -znocombreloc -T /usr/lib/elf_$(ARCH)_efi.lds -shared -Bsymbolic -L /usr/lib -l:libgnuefi.a -l:libefi.a

SRC             = main.c
OBJ             = $(SRC:.c=.o)
BIN             = main.efi

all: $(BIN)

%.efi: %.so
	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym  -j .rel -j .rela -j .reloc --target=efi-app-$(ARCH) $^ $@

%.so: $(OBJ)
	$(LD) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.so *.efi


