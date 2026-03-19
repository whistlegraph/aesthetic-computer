// babypat.c — bare-metal UEFI synesthesia toy
// Press a-z: screen floods with color, speaker beeps the note.
// Everything else is off. Only power button exits.
// No GUI, no text, no menus. Just color and sound.

#include <efi.h>
#include <efilib.h>

// --- Framebuffer ---
static UINT32 *fb;
static UINTN fb_size;
static int prgb;

static UINT32 rgb(UINT8 r, UINT8 g, UINT8 b) {
    return prgb ? ((UINT32)r | (UINT32)g<<8 | (UINT32)b<<16)
                : ((UINT32)b | (UINT32)g<<8 | (UINT32)r<<16);
}

static void flood(UINT32 c) {
    for (UINTN i = 0; i < fb_size; i++) fb[i] = c;
}

// --- PC Speaker ---
static inline void outb(UINT16 port, UINT8 val) {
    __asm__ __volatile__("outb %0, %1" : : "a"(val), "Nd"(port));
}
static inline UINT8 inb(UINT16 port) {
    UINT8 v; __asm__ __volatile__("inb %1, %0" : "=a"(v) : "Nd"(port)); return v;
}

static void beep(UINT32 freq) {
    UINT32 d = 1193182 / freq;
    outb(0x43, 0xB6);
    outb(0x42, d & 0xFF);
    outb(0x42, (d >> 8) & 0xFF);
    outb(0x61, inb(0x61) | 3);
}

static void hush(void) { outb(0x61, inb(0x61) & ~3); }

// --- Note table ---
// 24 keys: a-z mapped to two chromatic octaves (notepat layout)
// key -> {semitone 0-11, octave offset 0-1, r, g, b}
static const struct { char k; UINT8 n, o, r, g, b; } N[] = {
    // Octave 4: c v d s e f w g r a q b
    {'c', 0,0, 255, 30, 30}, {'v', 1,0, 255, 80,  0}, {'d', 2,0, 255,150,  0},
    {'s', 3,0, 200,200,  0}, {'e', 4,0, 230,220,  0}, {'f', 5,0,  30,200, 30},
    {'w', 6,0,   0,200,180}, {'g', 7,0,  30,100,255}, {'r', 8,0,  80, 50,255},
    {'a', 9,0, 140, 30,220}, {'q',10,0, 200, 30,150}, {'b',11,0, 200, 50,255},
    // Octave 5: h t i y j k u l o m p n
    {'h', 0,1, 255, 30, 30}, {'t', 1,1, 255, 80,  0}, {'i', 2,1, 255,150,  0},
    {'y', 3,1, 200,200,  0}, {'j', 4,1, 230,220,  0}, {'k', 5,1,  30,200, 30},
    {'u', 6,1,   0,200,180}, {'l', 7,1,  30,100,255}, {'o', 8,1,  80, 50,255},
    {'m', 9,1, 140, 30,220}, {'p',10,1, 200, 30,150}, {'n',11,1, 200, 50,255},
};

// C4..B4 frequencies (octave 5 = double)
static const UINT16 F[] = {262,277,294,311,330,349,370,392,415,440,466,494};

// --- Entry ---
EFI_STATUS EFIAPI efi_main(EFI_HANDLE img, EFI_SYSTEM_TABLE *st) {
    InitializeLib(img, st);
    uefi_call_wrapper(BS->SetWatchdogTimer, 4, 0, 0, 0, NULL);
    uefi_call_wrapper(ST->ConOut->EnableCursor, 2, ST->ConOut, FALSE);

    // GOP
    EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
    EFI_GUID guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
    if (EFI_ERROR(uefi_call_wrapper(BS->LocateProtocol, 3, &guid, NULL, (void**)&gop)))
        return EFI_UNSUPPORTED;
    fb = (UINT32*)(UINTN)gop->Mode->FrameBufferBase;
    fb_size = (UINTN)gop->Mode->Info->PixelsPerScanLine * gop->Mode->Info->VerticalResolution;
    prgb = (gop->Mode->Info->PixelFormat == PixelRedGreenBlueReserved8BitPerColor);

    // Start black
    flood(0);

    UINT8 fade = 0; // brightness counter

    for (;;) {
        EFI_INPUT_KEY key;
        EFI_STATUS s = uefi_call_wrapper(ST->ConIn->ReadKeyStroke, 2, ST->ConIn, &key);

        if (!EFI_ERROR(s) && key.UnicodeChar >= 'a' && key.UnicodeChar <= 'z') {
            char ch = (char)key.UnicodeChar;
            for (int i = 0; i < 24; i++) {
                if (N[i].k == ch) {
                    flood(rgb(N[i].r, N[i].g, N[i].b));
                    beep(F[N[i].n] << N[i].o);
                    fade = 20;
                    break;
                }
            }
        }

        // Fade to black
        if (fade > 0) {
            fade--;
            if (fade == 0) { flood(0); hush(); }
        }

        uefi_call_wrapper(BS->Stall, 1, 16000);
    }
}
