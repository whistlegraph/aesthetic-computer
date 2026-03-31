// splash.c — UEFI chainloader that shows "Aesthetic.Computer" then boots the kernel
// Replaces the OEM (Lenovo) logo with a custom splash screen.
// Built with gnu-efi, output is splash.efi (copied as BOOTX64.EFI).
// The real kernel lives at \EFI\BOOT\KERNEL.EFI on the same partition.

#include <efi.h>
#include <efilib.h>

#include "font8x8.h"

#define KERNEL_PATH L"\\EFI\\BOOT\\KERNEL.EFI"
#define LOADER_PATH L"\\EFI\\BOOT\\LOADER.EFI"

// --- GOP helpers ---

static EFI_GRAPHICS_OUTPUT_PROTOCOL *gop = NULL;
static UINT32 hres, vres, ppsl;
static UINT32 *fb;
static int pixel_is_rgb = 0; // 0 = BGR (default), 1 = RGB

static EFI_STATUS init_gop(void) {
    EFI_GUID gop_guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
    EFI_STATUS status = uefi_call_wrapper(BS->LocateProtocol, 3,
                             &gop_guid, NULL, (void **)&gop);
    if (EFI_ERROR(status)) return status;

    hres = gop->Mode->Info->HorizontalResolution;
    vres = gop->Mode->Info->VerticalResolution;
    ppsl = gop->Mode->Info->PixelsPerScanLine;
    fb = (UINT32 *)(UINTN)gop->Mode->FrameBufferBase;

    // Detect pixel format
    if (gop->Mode->Info->PixelFormat == PixelRedGreenBlueReserved8BitPerColor)
        pixel_is_rgb = 1;
    // PixelBlueGreenRedReserved8BitPerColor = 0 (default)
    // PixelBitMask: would need mask parsing, assume BGR for now

    return EFI_SUCCESS;
}

// Pack RGB into framebuffer pixel (handles BGR vs RGB format)
static UINT32 pack_color(UINT8 r, UINT8 g, UINT8 b) {
    if (pixel_is_rgb)
        return ((UINT32)r) | ((UINT32)g << 8) | ((UINT32)b << 16);
    else
        return ((UINT32)b) | ((UINT32)g << 8) | ((UINT32)r << 16);
}

static void fill_screen(UINT32 color) {
    UINTN total = (UINTN)ppsl * vres;
    for (UINTN i = 0; i < total; i++)
        fb[i] = color;
}

static void draw_pixel(UINT32 x, UINT32 y, UINT32 color) {
    if (x < hres && y < vres)
        fb[y * ppsl + x] = color;
}

// Draw one 8x8 character scaled by `scale`
static void draw_char(unsigned char c, UINT32 x, UINT32 y, UINT32 scale, UINT32 color) {
    if (c < 32 || c > 127) return;
    const char *glyph = font8x8_basic[c];
    for (UINT32 row = 0; row < 8; row++) {
        unsigned char bits = (unsigned char)glyph[row];
        for (UINT32 col = 0; col < 8; col++) {
            if (bits & (1 << col)) {
                for (UINT32 sy = 0; sy < scale; sy++)
                    for (UINT32 sx = 0; sx < scale; sx++)
                        draw_pixel(x + col * scale + sx,
                                   y + row * scale + sy, color);
            }
        }
    }
}

static UINT32 measure_string(const char *s, UINT32 scale) {
    UINT32 len = 0;
    while (s[len]) len++;
    return len * 8 * scale;
}

static void draw_string(const char *s, UINT32 x, UINT32 y,
                         UINT32 scale, UINT32 color) {
    while (*s) {
        draw_char((unsigned char)*s, x, y, scale, color);
        x += 8 * scale;
        s++;
    }
}

// --- Chainload ---

static EFI_STATUS chainload(EFI_HANDLE ImageHandle, CHAR16 *path) {
    EFI_STATUS status;
    EFI_HANDLE new_image;
    EFI_LOADED_IMAGE *loaded_image;
    EFI_GUID lip_guid = EFI_LOADED_IMAGE_PROTOCOL_GUID;
    EFI_DEVICE_PATH *dev_path;

    status = uefi_call_wrapper(BS->HandleProtocol, 3,
                               ImageHandle, &lip_guid, (void **)&loaded_image);
    if (EFI_ERROR(status)) return status;

    dev_path = FileDevicePath(loaded_image->DeviceHandle, path);
    if (!dev_path) return EFI_NOT_FOUND;

    status = uefi_call_wrapper(BS->LoadImage, 6,
                               FALSE, ImageHandle, dev_path, NULL, 0, &new_image);
    FreePool(dev_path);
    if (EFI_ERROR(status)) return status;

    // Transfer control — does not return on success
    status = uefi_call_wrapper(BS->StartImage, 3, new_image, NULL, NULL);
    return status;
}

// --- Entry point ---

EFI_STATUS EFIAPI efi_main(EFI_HANDLE ImageHandle,
                            EFI_SYSTEM_TABLE *SystemTable) {
    EFI_STATUS status;

    InitializeLib(ImageHandle, SystemTable);

    // Disable UEFI watchdog timer (prevents auto-reset if boot takes long)
    uefi_call_wrapper(BS->SetWatchdogTimer, 4, 0, 0, 0, NULL);

    // Disable firmware console output (prevents POST text from appearing)
    uefi_call_wrapper(ST->ConOut->EnableCursor, 2, ST->ConOut, FALSE);
    uefi_call_wrapper(ST->ConOut->ClearScreen, 1, ST->ConOut);

    // Initialize GOP
    status = init_gop();
    if (EFI_ERROR(status)) goto chain;

    // Clear screen to black
    fill_screen(pack_color(0, 0, 0));

    // Draw "Aesthetic.Computer" centered
    {
        const char *text = "Aesthetic.Computer";
        UINT32 scale = (hres >= 2560) ? 4 : (hres >= 1920) ? 3 : 2;
        UINT32 tw = measure_string(text, scale);
        UINT32 x = (hres - tw) / 2;
        UINT32 y = (vres - 8 * scale) / 2;
        // Soft pink/lavender: R=220 G=180 B=200
        UINT32 color = pack_color(220, 180, 200);
        draw_string(text, x, y, scale, color);
    }

chain:
    // Try systemd-boot first (for Mac split-kernel boot), fall back to direct kernel
    status = chainload(ImageHandle, LOADER_PATH);
    // LOADER.EFI not found or failed — try direct kernel (non-Mac boot)
    status = chainload(ImageHandle, KERNEL_PATH);

    // Both failed — show error
    Print(L"Boot failed: %r\n", status);
    uefi_call_wrapper(BS->Stall, 1, 5000000);
    return status;
}
