// test-boot.c — minimal EFI app to test if GOP works
// Should show: white screen for 2 seconds, then red for 2 seconds, then halt
#include <efi.h>
#include <efilib.h>

EFI_STATUS EFIAPI efi_main(EFI_HANDLE img, EFI_SYSTEM_TABLE *st) {
    InitializeLib(img, st);
    uefi_call_wrapper(BS->SetWatchdogTimer, 4, 0, 0, 0, NULL);

    // Try console output first (always works)
    Print(L"babypat test-boot starting...\r\n");

    // GOP
    EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
    EFI_GUID guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
    EFI_STATUS s = uefi_call_wrapper(BS->LocateProtocol, 3, &guid, NULL, (void**)&gop);
    if (EFI_ERROR(s)) {
        Print(L"GOP failed: %r\r\n", s);
        uefi_call_wrapper(BS->Stall, 1, 5000000);
        return s;
    }

    UINT32 *fb = (UINT32*)(UINTN)gop->Mode->FrameBufferBase;
    UINTN n = (UINTN)gop->Mode->Info->PixelsPerScanLine * gop->Mode->Info->VerticalResolution;
    int is_rgb = (gop->Mode->Info->PixelFormat == PixelRedGreenBlueReserved8BitPerColor);

    Print(L"GOP OK: %dx%d fb=%lx fmt=%d\r\n",
        gop->Mode->Info->HorizontalResolution,
        gop->Mode->Info->VerticalResolution,
        gop->Mode->FrameBufferBase,
        gop->Mode->Info->PixelFormat);

    // White
    UINT32 white = 0xFFFFFFFF;
    for (UINTN i = 0; i < n; i++) fb[i] = white;
    uefi_call_wrapper(BS->Stall, 1, 2000000);

    // Red
    UINT32 red = is_rgb ? 0x000000FF : 0x00FF0000;
    for (UINTN i = 0; i < n; i++) fb[i] = red;
    uefi_call_wrapper(BS->Stall, 1, 2000000);

    // Green
    UINT32 green = 0x0000FF00;
    for (UINTN i = 0; i < n; i++) fb[i] = green;

    // Halt
    for (;;) uefi_call_wrapper(BS->Stall, 1, 1000000);
}
