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

// --- USB Log ---
static EFI_FILE_HANDLE logfile;

static void log_open(EFI_HANDLE img) {
    EFI_LOADED_IMAGE *li;
    EFI_GUID li_guid = EFI_LOADED_IMAGE_PROTOCOL_GUID;
    if (EFI_ERROR(uefi_call_wrapper(BS->HandleProtocol, 3, img, &li_guid, (void**)&li)))
        return;
    EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *fs;
    EFI_GUID fs_guid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
    if (EFI_ERROR(uefi_call_wrapper(BS->HandleProtocol, 3, li->DeviceHandle, &fs_guid, (void**)&fs)))
        return;
    EFI_FILE_HANDLE root;
    if (EFI_ERROR(uefi_call_wrapper(fs->OpenVolume, 2, fs, &root)))
        return;
    uefi_call_wrapper(root->Open, 5, root, &logfile, L"\\EFI\\babypat.log",
        EFI_FILE_MODE_CREATE | EFI_FILE_MODE_WRITE | EFI_FILE_MODE_READ, 0);
}

static void log_str(CHAR8 *s) {
    if (!logfile) return;
    UINTN len = 0;
    while (s[len]) len++;
    uefi_call_wrapper(logfile->Write, 3, logfile, &len, s);
    uefi_call_wrapper(logfile->Flush, 1, logfile);
}

static void log_hex(CHAR8 *label, UINTN val) {
    CHAR8 buf[80];
    CHAR8 *p = buf;
    while (*label) *p++ = *label++;
    *p++ = '0'; *p++ = 'x';
    for (int i = 60; i >= 0; i -= 4) {
        UINT8 nibble = (val >> i) & 0xF;
        if (nibble || i == 0 || p > buf + 4)
            *p++ = nibble < 10 ? '0' + nibble : 'a' + nibble - 10;
    }
    *p++ = '\r'; *p++ = '\n'; *p = 0;
    log_str(buf);
}

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

    // Log
    log_open(img);
    log_str("babypat boot\r\n");

    // GOP
    EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
    EFI_GUID guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
    EFI_STATUS gop_status = uefi_call_wrapper(BS->LocateProtocol, 3, &guid, NULL, (void**)&gop);
    if (EFI_ERROR(gop_status)) {
        log_str("GOP FAILED\r\n");
        log_hex("  status=", gop_status);
        return EFI_UNSUPPORTED;
    }
    fb = (UINT32*)(UINTN)gop->Mode->FrameBufferBase;
    fb_size = (UINTN)gop->Mode->Info->PixelsPerScanLine * gop->Mode->Info->VerticalResolution;
    prgb = (gop->Mode->Info->PixelFormat == PixelRedGreenBlueReserved8BitPerColor);

    log_str("GOP OK\r\n");
    log_hex("  fb=", (UINTN)fb);
    log_hex("  fb_size=", fb_size);
    log_hex("  stride=", gop->Mode->Info->PixelsPerScanLine);
    log_hex("  height=", gop->Mode->Info->VerticalResolution);
    log_hex("  width=", gop->Mode->Info->HorizontalResolution);
    log_hex("  pixfmt=", gop->Mode->Info->PixelFormat);
    log_hex("  mode=", gop->Mode->Mode);
    log_hex("  max_mode=", gop->Mode->MaxMode);

    // Start black
    flood(0);
    log_str("flood black done\r\n");

    UINT8 fade = 0; // brightness counter

    // Reset keyboard input
    uefi_call_wrapper(ST->ConIn->Reset, 2, ST->ConIn, FALSE);

    // Signal we're alive: brief white flash
    flood(rgb(255, 255, 255));
    uefi_call_wrapper(BS->Stall, 1, 300000);
    flood(0);

    for (;;) {
        // Block until ANY key event
        UINTN idx;
        uefi_call_wrapper(BS->WaitForEvent, 3, 1, &ST->ConIn->WaitForKey, &idx);

        EFI_INPUT_KEY key;
        EFI_STATUS s = uefi_call_wrapper(ST->ConIn->ReadKeyStroke, 2, ST->ConIn, &key);

        if (!EFI_ERROR(s)) {
            // Flash magenta on ANY key to prove input works
            flood(rgb(255, 0, 255));
            uefi_call_wrapper(BS->Stall, 1, 200000);

            if (key.UnicodeChar >= 'a' && key.UnicodeChar <= 'z') {
                char ch = (char)key.UnicodeChar;
                for (int i = 0; i < 24; i++) {
                    if (N[i].k == ch) {
                        flood(rgb(N[i].r, N[i].g, N[i].b));
                        beep(F[N[i].n] << N[i].o);
                        fade = 20;
                        break;
                    }
                }
            } else {
                // Non a-z key: show cyan briefly
                flood(rgb(0, 255, 255));
                uefi_call_wrapper(BS->Stall, 1, 500000);
                flood(0);
            }
        }

        // Fade to black
        if (fade > 0) {
            fade--;
            if (fade == 0) { flood(0); hush(); }
        }
    }
}
