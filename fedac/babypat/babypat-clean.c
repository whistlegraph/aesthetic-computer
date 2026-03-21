// babypat-clean.c — bare-metal UEFI synesthesia toy
// Press a-z: screen floods with color, fades to black.
// No logging, no Print(), no sound (modern laptops lack PIT speaker).
#include <efi.h>
#include <efilib.h>

static UINT32 *fb;
static UINTN fb_n;
static int is_rgb;

static UINT32 pack(UINT8 r, UINT8 g, UINT8 b) {
    return is_rgb ? ((UINT32)r | (UINT32)g<<8 | (UINT32)b<<16)
                  : ((UINT32)b | (UINT32)g<<8 | (UINT32)r<<16);
}

static void flood(UINT32 c) {
    for (UINTN i = 0; i < fb_n; i++) fb[i] = c;
}

// Notes: 24 keys, notepat layout, 2 octaves
static const struct { char k; UINT8 n, o, r, g, b; } N[] = {
    {'c', 0,0, 255, 30, 30}, {'v', 1,0, 255, 80,  0}, {'d', 2,0, 255,150,  0},
    {'s', 3,0, 200,200,  0}, {'e', 4,0, 230,220,  0}, {'f', 5,0,  30,200, 30},
    {'w', 6,0,   0,200,180}, {'g', 7,0,  30,100,255}, {'r', 8,0,  80, 50,255},
    {'a', 9,0, 140, 30,220}, {'q',10,0, 200, 30,150}, {'b',11,0, 200, 50,255},
    {'h', 0,1, 255, 30, 30}, {'t', 1,1, 255, 80,  0}, {'i', 2,1, 255,150,  0},
    {'y', 3,1, 200,200,  0}, {'j', 4,1, 230,220,  0}, {'k', 5,1,  30,200, 30},
    {'u', 6,1,   0,200,180}, {'l', 7,1,  30,100,255}, {'o', 8,1,  80, 50,255},
    {'m', 9,1, 140, 30,220}, {'p',10,1, 200, 30,150}, {'n',11,1, 200, 50,255},
};

EFI_STATUS EFIAPI efi_main(EFI_HANDLE img, EFI_SYSTEM_TABLE *st) {
    InitializeLib(img, st);
    uefi_call_wrapper(BS->SetWatchdogTimer, 4, 0, 0, 0, NULL);
    uefi_call_wrapper(ST->ConOut->EnableCursor, 2, ST->ConOut, FALSE);
    uefi_call_wrapper(ST->ConOut->ClearScreen, 1, ST->ConOut);

    // GOP
    EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
    EFI_GUID guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
    if (EFI_ERROR(uefi_call_wrapper(BS->LocateProtocol, 3, &guid, NULL, (void**)&gop)))
        return EFI_UNSUPPORTED;

    fb = (UINT32*)(UINTN)gop->Mode->FrameBufferBase;
    fb_n = (UINTN)gop->Mode->Info->PixelsPerScanLine * gop->Mode->Info->VerticalResolution;
    is_rgb = (gop->Mode->Info->PixelFormat == PixelRedGreenBlueReserved8BitPerColor);

    flood(0);

    // Reset input
    uefi_call_wrapper(ST->ConIn->Reset, 2, ST->ConIn, FALSE);

    UINT16 fade = 0;
    UINT8 cr = 0, cg = 0, cb = 0;

    for (;;) {
        // Poll for key (non-blocking)
        EFI_INPUT_KEY key;
        EFI_STATUS s = uefi_call_wrapper(ST->ConIn->ReadKeyStroke, 2, ST->ConIn, &key);

        if (!EFI_ERROR(s) && key.UnicodeChar >= 'a' && key.UnicodeChar <= 'z') {
            char ch = (char)key.UnicodeChar;
            for (int i = 0; i < 24; i++) {
                if (N[i].k == ch) {
                    cr = N[i].r; cg = N[i].g; cb = N[i].b;
                    flood(pack(cr, cg, cb));
                    fade = 400;
                    break;
                }
            }
        }

        // Smooth fade to black
        if (fade > 0) {
            fade--;
            if (fade == 0) {
                flood(0);
            } else if (fade < 200) {
                UINT8 t = (UINT8)((fade * 255) / 200);
                flood(pack((cr * t) >> 8, (cg * t) >> 8, (cb * t) >> 8));
            }
        }

        uefi_call_wrapper(BS->Stall, 1, 16000);
    }
}
