#include <efi.h>
#include <efilib.h>

EFI_STATUS
EFIAPI
efi_main(EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable) {
    InitializeLib(ImageHandle, SystemTable);
    EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
    EFI_STATUS status;

    // Locate the graphics output protocol
    status = uefi_call_wrapper(BS->LocateProtocol, 3, &GraphicsOutputProtocol, NULL, (void **)&gop);
    if (EFI_ERROR(status)) {
        Print(L"Unable to locate GOP\n");
        return status;
    }

    // Assume the display is at least 200px tall and wide for simplicity
    UINTN line_length = 200;
    for (UINTN i = 0; i < line_length; i++) {
        // Calculate the position to set the pixel
        UINTN x = i;
        UINTN y = i;
        UINT32 color = 0x00FFFF00; // Yellow in RGB reserved format

        // Draw the pixel
        UINTN pos = x + (y * gop->Mode->Info->PixelsPerScanLine);
        ((UINT32 *)gop->Mode->FrameBufferBase)[pos] = color;
    }

    // Wait for key press before exiting
    SystemTable->ConIn->Reset(SystemTable->ConIn, FALSE);
    EFI_INPUT_KEY Key;
    while ((SystemTable->ConIn->ReadKeyStroke(SystemTable->ConIn, &Key)) == EFI_NOT_READY);

    return EFI_SUCCESS;
}

