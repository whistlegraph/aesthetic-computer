// babypat-tiny.c — bare-metal UEFI synesthesia toy (no gnu-efi)
// Press a-z: screen floods with color, speaker beeps the note.
// Only a-z keys do anything. Power button to exit.
// No runtime library. Direct UEFI protocol calls. ~1 KB of code.

#include <stdint.h>

// --- UEFI types (hand-rolled, no efi.h) ---
typedef uint64_t UINTN;
typedef uint64_t EFI_STATUS;
typedef void *EFI_HANDLE;
typedef uint16_t CHAR16;
typedef struct { uint32_t a; uint16_t b, c; uint8_t d[8]; } EFI_GUID;
typedef struct { uint16_t ScanCode; CHAR16 UnicodeChar; } EFI_INPUT_KEY;

typedef struct _STI {
    EFI_STATUS (*Reset)(struct _STI *, uint8_t);
    EFI_STATUS (*ReadKeyStroke)(struct _STI *, EFI_INPUT_KEY *);
    void *WaitForKey;
} EFI_STI; // SimpleTextInput

typedef struct _STO {
    void *Reset;
    void *OutputString;
    void *TestString;
    void *QueryMode;
    void *SetMode;
    void *SetAttribute;
    EFI_STATUS (*ClearScreen)(struct _STO *);
    void *SetCursorPosition;
    EFI_STATUS (*EnableCursor)(struct _STO *, uint8_t);
} EFI_STO; // SimpleTextOutput

// GOP
typedef struct {
    uint32_t Ver, HRes, VRes;
    uint32_t PixFmt; // 0=RGB, 1=BGR
    uint32_t Mask[4];
    uint32_t PPSL;
} EFI_GOP_INFO;
typedef struct {
    uint32_t MaxMode, Mode;
    EFI_GOP_INFO *Info;
    UINTN InfoSz;
    uint64_t FBBase;
    UINTN FBSize;
} EFI_GOP_MODE;
typedef struct {
    void *QueryMode, *SetMode, *Blt;
    EFI_GOP_MODE *Mode;
} EFI_GOP;

// Boot Services — every field must be present to get correct offsets
typedef struct {
    char _hdr[24];
    void *RaiseTPL;                  // +24
    void *RestoreTPL;                // +32
    void *AllocatePages;             // +40
    void *FreePages;                 // +48
    void *GetMemoryMap;              // +56
    void *AllocatePool;              // +64
    void *FreePool;                  // +72
    void *CreateEvent;               // +80
    void *SetTimer;                  // +88
    void *WaitForEvent;              // +96
    void *SignalEvent;               // +104
    void *CloseEvent;                // +112
    void *CheckEvent;                // +120
    void *InstallProtoIface;         // +128
    void *ReinstallProtoIface;       // +136
    void *UninstallProtoIface;       // +144
    void *HandleProtocol;            // +152
    void *_Reserved;                 // +160
    void *RegisterProtoNotify;       // +168
    void *LocateHandle;              // +176
    void *LocateDevicePath;          // +184
    void *InstallConfigTable;        // +192
    void *LoadImage;                 // +200
    void *StartImage;                // +208
    void *Exit;                      // +216
    void *UnloadImage;               // +224
    void *ExitBootServices;          // +232
    void *GetNextMonotonicCount;     // +240
    EFI_STATUS (*Stall)(UINTN us);   // +248
    EFI_STATUS (*SetWatchdogTimer)(UINTN, uint64_t, UINTN, CHAR16*); // +256
    void *ConnectCtrl;               // +264
    void *DisconnectCtrl;            // +272
    void *OpenProtocol;              // +280
    void *CloseProtocol;             // +288
    void *OpenProtoInfo;             // +296
    void *ProtocolsPerHandle;        // +304
    void *LocateHandleBuffer;        // +312
    EFI_STATUS (*LocateProtocol)(EFI_GUID*, void*, void**); // +320
} EFI_BS;

typedef struct {
    char _hdr[24];
    CHAR16 *FirmwareVendor;          // +24
    uint32_t FirmwareRevision;       // +32 (+4 padding)
    EFI_HANDLE ConsoleInHandle;      // +40
    EFI_STI *ConIn;                  // +48
    EFI_HANDLE ConsoleOutHandle;     // +56
    EFI_STO *ConOut;                 // +64
    void *StdErrHandle;              // +72
    void *StdErr;                    // +80
    void *RuntimeServices;           // +88
    EFI_BS *BS;                      // +96
} EFI_ST;

// --- GOP GUID ---
static EFI_GUID gop_guid = {
    0x9042a9de, 0x23dc, 0x4a38,
    {0x96, 0xfb, 0x7a, 0xde, 0xd0, 0x80, 0x51, 0x6a}
};

// --- Globals ---
static uint32_t *fb;
static UINTN fb_n;
static int is_rgb;

static uint32_t rgb(uint8_t r, uint8_t g, uint8_t b) {
    return is_rgb ? ((uint32_t)r | (uint32_t)g << 8 | (uint32_t)b << 16)
                  : ((uint32_t)b | (uint32_t)g << 8 | (uint32_t)r << 16);
}

static void flood(uint32_t c) {
    for (UINTN i = 0; i < fb_n; i++) fb[i] = c;
}

// --- PC Speaker ---
static inline void outb(uint16_t port, uint8_t val) {
    __asm__ __volatile__("outb %0, %1" : : "a"(val), "Nd"(port));
}
static inline uint8_t inb(uint16_t port) {
    uint8_t v;
    __asm__ __volatile__("inb %1, %0" : "=a"(v) : "Nd"(port));
    return v;
}
static void beep(uint32_t freq) {
    uint32_t d = 1193182 / freq;
    outb(0x43, 0xB6);
    outb(0x42, d & 0xFF);
    outb(0x42, (d >> 8) & 0xFF);
    outb(0x61, inb(0x61) | 3);
}
static void hush(void) { outb(0x61, inb(0x61) & ~3); }

// --- USB Log ---
// UEFI Simple File System + File protocol (hand-rolled)
typedef struct _EFI_FP {
    uint64_t Revision;
    EFI_STATUS (*Open)(struct _EFI_FP *self, struct _EFI_FP **out, CHAR16 *name, uint64_t mode, uint64_t attr);
    EFI_STATUS (*Close)(struct _EFI_FP *self);
    void *Delete;
    EFI_STATUS (*Read)(struct _EFI_FP *self, UINTN *sz, void *buf);
    EFI_STATUS (*Write)(struct _EFI_FP *self, UINTN *sz, void *buf);
    void *GetPosition, *SetPosition, *GetInfo, *SetInfo;
    EFI_STATUS (*Flush)(struct _EFI_FP *self);
} EFI_FP;

typedef struct {
    uint64_t Revision;
    EFI_STATUS (*OpenVolume)(void *self, EFI_FP **root);
} EFI_SFS;

typedef struct {
    uint32_t Revision;
    EFI_HANDLE ParentHandle;
    void *SystemTable;
    EFI_HANDLE DeviceHandle;
    // ... more fields but we only need DeviceHandle
} EFI_LI;

static EFI_GUID li_guid = {0x5B1B31A1, 0x9562, 0x11d2, {0x8E,0x3F,0x00,0xA0,0xC9,0x69,0x72,0x3B}};
static EFI_GUID sfs_guid = {0x964E5B22, 0x6459, 0x11D2, {0x8E,0x39,0x00,0xA0,0xC9,0x69,0x72,0x3B}};

static EFI_FP *logfile;
static EFI_ST *g_st;

static void log_open(EFI_HANDLE img, EFI_ST *st) {
    g_st = st;
    EFI_LI *li;
    if (st->BS->LocateProtocol(&li_guid, 0, (void**)&li)) return;
    // Use HandleProtocol via OpenProtocol field... but we don't have it in BS
    // Instead: use LocateProtocol for SFS (boot volume)
    EFI_SFS *sfs;
    if (st->BS->LocateProtocol(&sfs_guid, 0, (void**)&sfs)) return;
    EFI_FP *root;
    if (sfs->OpenVolume(sfs, &root)) return;
    root->Open(root, &logfile, u"\\EFI\\babypat.log",
        0x8000000000000003ULL, 0); // CREATE | WRITE | READ
}

static void log_str(char *s) {
    if (!logfile) return;
    UINTN len = 0;
    while (s[len]) len++;
    logfile->Write(logfile, &len, s);
    logfile->Flush(logfile);
}

static void log_num(char *label, uint64_t val) {
    char buf[80];
    char *p = buf;
    while (*label) *p++ = *label++;
    // Simple hex
    *p++ = '0'; *p++ = 'x';
    int started = 0;
    for (int i = 60; i >= 0; i -= 4) {
        uint8_t nib = (val >> i) & 0xF;
        if (nib || started || i == 0) {
            *p++ = nib < 10 ? '0' + nib : 'a' + nib - 10;
            started = 1;
        }
    }
    *p++ = '\r'; *p++ = '\n'; *p = 0;
    log_str(buf);
}

// --- Notes: 24 keys, notepat layout, 2 octaves ---
static const struct { char k; uint8_t n, o, r, g, b; } N[] = {
    {'c', 0,0, 255, 30, 30}, {'v', 1,0, 255, 80,  0}, {'d', 2,0, 255,150,  0},
    {'s', 3,0, 200,200,  0}, {'e', 4,0, 230,220,  0}, {'f', 5,0,  30,200, 30},
    {'w', 6,0,   0,200,180}, {'g', 7,0,  30,100,255}, {'r', 8,0,  80, 50,255},
    {'a', 9,0, 140, 30,220}, {'q',10,0, 200, 30,150}, {'b',11,0, 200, 50,255},
    {'h', 0,1, 255, 30, 30}, {'t', 1,1, 255, 80,  0}, {'i', 2,1, 255,150,  0},
    {'y', 3,1, 200,200,  0}, {'j', 4,1, 230,220,  0}, {'k', 5,1,  30,200, 30},
    {'u', 6,1,   0,200,180}, {'l', 7,1,  30,100,255}, {'o', 8,1,  80, 50,255},
    {'m', 9,1, 140, 30,220}, {'p',10,1, 200, 30,150}, {'n',11,1, 200, 50,255},
};
static const uint16_t F[] = {262,277,294,311,330,349,370,392,415,440,466,494};

// --- Entry ---
EFI_STATUS efi_main(EFI_HANDLE img, EFI_ST *st) {
    st->BS->SetWatchdogTimer(0, 0, 0, 0);
    st->ConOut->EnableCursor(st->ConOut, 0);
    st->ConOut->ClearScreen(st->ConOut);

    log_open(img, st);
    log_str("babypat-tiny boot\r\n");

    EFI_GOP *gop;
    if (st->BS->LocateProtocol(&gop_guid, 0, (void **)&gop)) {
        log_str("GOP FAILED\r\n");
        return 1;
    }

    fb = (uint32_t *)(UINTN)gop->Mode->FBBase;
    fb_n = (UINTN)gop->Mode->Info->PPSL * gop->Mode->Info->VRes;
    is_rgb = (gop->Mode->Info->PixFmt == 0);

    log_str("GOP OK\r\n");
    log_num("  fb=", gop->Mode->FBBase);
    log_num("  pixels=", fb_n);
    log_num("  w=", gop->Mode->Info->HRes);
    log_num("  h=", gop->Mode->Info->VRes);
    log_num("  stride=", gop->Mode->Info->PPSL);
    log_num("  pixfmt=", gop->Mode->Info->PixFmt);

    flood(0);
    log_str("flood black done\r\n");

    uint8_t fade = 0;
    for (;;) {
        EFI_INPUT_KEY key;
        if (!st->ConIn->ReadKeyStroke(st->ConIn, &key)
            && key.UnicodeChar >= 'a' && key.UnicodeChar <= 'z') {
            for (int i = 0; i < 24; i++) {
                if (N[i].k == (char)key.UnicodeChar) {
                    flood(rgb(N[i].r, N[i].g, N[i].b));
                    beep(F[N[i].n] << N[i].o);
                    fade = 20;
                    break;
                }
            }
        }
        if (fade > 0 && --fade == 0) { flood(0); hush(); }
        st->BS->Stall(16000);
    }
}
