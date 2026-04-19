// main_grand.m — Notepat Grand (macOS native Cocoa app).
// Complete replacement for the SDL3/QuickJS-based Notepat runtime: a
// fullscreen transparent overlay with two one-octave piano keyboards drawn
// in the bottom-left and bottom-right of the screen via CoreGraphics, audio
// through Apple's AUMIDISynth (audio_musicdevice.c).
//
// Mouse events pass through empty areas to whatever app is underneath, so
// this really is a HUD you can drop on top of any workflow. Keyboard input
// is only captured while the overlay is the key window — click on a piano
// key area once to activate, or use the global Ctrl+Alt+Cmd+N hotkey to
// raise + focus.

#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import <math.h>

#import "audio.h"
#import "synth_types.h"

// ── Key → MIDI mapping ──────────────────────────────────────────────────────
// Left piano keys (lower octave) use the left side of the QWERTY keyboard;
// right piano keys (upper octave) use the right side. Blacks sit on the row
// above the whites, the natural layout of a piano on a typewriter keyboard.

typedef struct { int keyCode; int midi; } PianoKeyMap;

static const PianoKeyMap g_left_white[7] = {
    { kVK_ANSI_A, 60 },  // C4
    { kVK_ANSI_S, 62 },  // D4
    { kVK_ANSI_D, 64 },  // E4
    { kVK_ANSI_F, 65 },  // F4
    { kVK_ANSI_G, 67 },  // G4
    { kVK_ANSI_H, 69 },  // A4
    { kVK_ANSI_J, 71 },  // B4
};
// -1 means "no black key between this white and the next" (E→F, B→C).
static const PianoKeyMap g_left_black[7] = {
    { kVK_ANSI_W, 61 },  // C#4
    { kVK_ANSI_E, 63 },  // D#4
    { 0,          -1 },  // (E→F)
    { kVK_ANSI_T, 66 },  // F#4
    { kVK_ANSI_Y, 68 },  // G#4
    { kVK_ANSI_U, 70 },  // A#4
    { 0,          -1 },  // (B→C)
};

static const PianoKeyMap g_right_white[7] = {
    { kVK_ANSI_K,         72 },  // C5
    { kVK_ANSI_L,         74 },  // D5
    { kVK_ANSI_Semicolon, 76 },  // E5
    { kVK_ANSI_Quote,     77 },  // F5
    { kVK_ANSI_RightBracket, 79 }, // G5
    { kVK_ANSI_Backslash, 81 },  // A5
    { kVK_Return,         83 },  // B5
};
static const PianoKeyMap g_right_black[7] = {
    { kVK_ANSI_O,         73 },  // C#5
    { kVK_ANSI_P,         75 },  // D#5
    { 0,                  -1 },
    { kVK_ANSI_LeftBracket, 78 }, // F#5
    { kVK_ANSI_Equal,     80 },  // G#5
    { kVK_Delete,         82 },  // A#5  (macOS Delete = backspace key; above backslash)
    { 0,                  -1 },
};

static int midi_for_keycode(int kc) {
    for (int i = 0; i < 7; i++) {
        if (g_left_white[i].keyCode  == kc) return g_left_white[i].midi;
        if (g_left_black[i].midi >= 0 && g_left_black[i].keyCode == kc)
            return g_left_black[i].midi;
        if (g_right_white[i].keyCode == kc) return g_right_white[i].midi;
        if (g_right_black[i].midi >= 0 && g_right_black[i].keyCode == kc)
            return g_right_black[i].midi;
    }
    return -1;
}

static double midi_to_freq(int midi) {
    return 440.0 * pow(2.0, (midi - 69) / 12.0);
}

// ── Shared state ────────────────────────────────────────────────────────────

static Audio *g_audio = NULL;
static EventHotKeyRef g_hotkey = NULL;
static EventHandlerRef g_hotkey_handler = NULL;

// ── Piano view ──────────────────────────────────────────────────────────────

@interface PianoView : NSView
@property (nonatomic) NSRect leftRect;
@property (nonatomic) NSRect rightRect;
@property (nonatomic) CGFloat whiteW;
@property (nonatomic) CGFloat whiteH;
@property (nonatomic) CGFloat blackW;
@property (nonatomic) CGFloat blackH;

@property (strong) NSMutableSet<NSNumber *> *activeNotes;
@property (strong) NSMutableDictionary<NSNumber *, NSNumber *> *noteHandles;
@end

@implementation PianoView

- (instancetype)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        _activeNotes   = [NSMutableSet new];
        _noteHandles   = [NSMutableDictionary new];
        self.wantsLayer = YES;
        self.layer.backgroundColor = [NSColor clearColor].CGColor;
        [self recomputeLayout];
    }
    return self;
}

- (BOOL)acceptsFirstResponder     { return YES; }
- (BOOL)canBecomeKeyView          { return YES; }

- (void)setFrame:(NSRect)frame {
    [super setFrame:frame];
    [self recomputeLayout];
}

- (void)recomputeLayout {
    NSRect b = self.bounds;
    // Each piano takes ~28% of screen width, anchored to the bottom corners.
    CGFloat margin = 24;
    CGFloat panelW = MIN(b.size.width * 0.28, 420);
    CGFloat panelH = 140;
    _whiteW = panelW / 7.0;
    _whiteH = panelH;
    _blackW = _whiteW * 0.60;
    _blackH = _whiteH * 0.62;

    _leftRect  = NSMakeRect(margin, margin, panelW, panelH);
    _rightRect = NSMakeRect(b.size.width - margin - panelW, margin, panelW, panelH);
}

// Mouse clicks on empty areas (outside either piano) should pass through to
// the app beneath this overlay. NSWindow's contentView hitTest normally
// claims every click; returning nil lets Cocoa fall through.
- (NSView *)hitTest:(NSPoint)pt {
    NSPoint local = [self convertPoint:pt fromView:self.superview];
    if (NSPointInRect(local, _leftRect) || NSPointInRect(local, _rightRect)) return self;
    return nil;
}

- (void)drawOctaveInRect:(NSRect)rect
               whiteKeys:(const PianoKeyMap *)whites
               blackKeys:(const PianoKeyMap *)blacks
                  inCtx:(CGContextRef)ctx {
    CGFloat wW = _whiteW, wH = _whiteH, bW = _blackW, bH = _blackH;

    // White keys — stroke + fill with press-tint overlay.
    for (int i = 0; i < 7; i++) {
        int note = whites[i].midi;
        BOOL pressed = [self.activeNotes containsObject:@(note)];
        NSRect k = NSMakeRect(rect.origin.x + i * wW, rect.origin.y, wW, wH);
        // Drop shadow via layer — skip here, simple fill.
        CGContextSetRGBFillColor(ctx, 0.97, 0.97, 0.94, 0.92);
        CGContextFillRect(ctx, k);
        if (pressed) {
            CGContextSetRGBFillColor(ctx, 1.00, 0.70, 0.30, 0.55);
            CGContextFillRect(ctx, k);
        }
        CGContextSetRGBStrokeColor(ctx, 0.12, 0.12, 0.14, 0.85);
        CGContextSetLineWidth(ctx, 1.2);
        CGContextStrokeRect(ctx, k);
    }

    // Black keys — drawn after whites so they overlay.
    for (int i = 0; i < 7; i++) {
        int note = blacks[i].midi;
        if (note < 0) continue;
        BOOL pressed = [self.activeNotes containsObject:@(note)];
        CGFloat cx = rect.origin.x + (i + 1) * wW;
        NSRect k = NSMakeRect(cx - bW / 2, rect.origin.y + wH - bH, bW, bH);
        CGContextSetRGBFillColor(ctx, 0.08, 0.08, 0.10, 0.95);
        CGContextFillRect(ctx, k);
        if (pressed) {
            CGContextSetRGBFillColor(ctx, 1.00, 0.70, 0.30, 0.70);
            CGContextFillRect(ctx, k);
        }
        CGContextSetRGBStrokeColor(ctx, 0.02, 0.02, 0.03, 1.0);
        CGContextSetLineWidth(ctx, 0.6);
        CGContextStrokeRect(ctx, k);
    }
}

- (void)drawRect:(NSRect)dirty {
    CGContextRef ctx = [[NSGraphicsContext currentContext] CGContext];
    // Clear everything — we only fill the piano rects, rest stays transparent.
    CGContextClearRect(ctx, self.bounds);
    [self drawOctaveInRect:_leftRect  whiteKeys:g_left_white  blackKeys:g_left_black  inCtx:ctx];
    [self drawOctaveInRect:_rightRect whiteKeys:g_right_white blackKeys:g_right_black inCtx:ctx];
}

// ── Keyboard input ──────────────────────────────────────────────────────────

- (void)keyDown:(NSEvent *)ev {
    if (ev.isARepeat) return;
    int midi = midi_for_keycode(ev.keyCode);
    if (midi < 0) return;
    if ([self.activeNotes containsObject:@(midi)]) return;
    uint64_t handle = audio_synth(g_audio, WAVE_SINE,
                                  midi_to_freq(midi),
                                  INFINITY, 0.7, 0.005, 0.05, 0.0);
    if (handle) self.noteHandles[@(midi)] = @(handle);
    [self.activeNotes addObject:@(midi)];
    [self setNeedsDisplay:YES];
}

- (void)keyUp:(NSEvent *)ev {
    int midi = midi_for_keycode(ev.keyCode);
    if (midi < 0) return;
    NSNumber *h = self.noteHandles[@(midi)];
    if (h) {
        audio_kill(g_audio, h.unsignedLongLongValue, 0.04);
        [self.noteHandles removeObjectForKey:@(midi)];
    }
    [self.activeNotes removeObject:@(midi)];
    [self setNeedsDisplay:YES];
}

@end

// ── Global hotkey ───────────────────────────────────────────────────────────
static volatile int g_toggle_visible = 0;

static OSStatus hotkey_cb(EventHandlerCallRef href, EventRef ev, void *ud) {
    (void)href; (void)ev; (void)ud;
    g_toggle_visible = 1;
    return noErr;
}
static void install_global_hotkey(void) {
    EventTypeSpec spec = { kEventClassKeyboard, kEventHotKeyPressed };
    InstallEventHandler(GetEventDispatcherTarget(), hotkey_cb, 1, &spec, NULL,
                        &g_hotkey_handler);
    EventHotKeyID id = { .signature = 'ntpg', .id = 1 };
    RegisterEventHotKey(kVK_ANSI_N,
                        cmdKey | optionKey | controlKey,
                        id, GetEventDispatcherTarget(), 0, &g_hotkey);
}

// ── Status-bar item ─────────────────────────────────────────────────────────

@interface AppDelegate : NSObject <NSApplicationDelegate>
@property (strong) NSWindow *window;
@property (strong) PianoView *piano;
@property (strong) NSStatusItem *status;
@property (nonatomic) BOOL visible;
@end

@implementation AppDelegate

- (void)toggleVisible {
    if (self.visible) {
        [self.window orderOut:nil];
    } else {
        [self.window makeKeyAndOrderFront:nil];
        [NSApp activateIgnoringOtherApps:YES];
    }
    self.visible = !self.visible;
}

- (void)applicationDidFinishLaunching:(NSNotification *)note {
    g_audio = audio_init();
    install_global_hotkey();

    // Fullscreen-sized borderless transparent window anchored to the main
    // screen. collectionBehavior keeps it on top across spaces.
    NSRect screen = [NSScreen mainScreen].frame;
    self.window = [[NSWindow alloc]
        initWithContentRect:screen
                  styleMask:NSWindowStyleMaskBorderless
                    backing:NSBackingStoreBuffered
                      defer:NO];
    self.window.opaque           = NO;
    self.window.backgroundColor  = [NSColor clearColor];
    self.window.hasShadow        = NO;
    self.window.level            = NSStatusWindowLevel;   // above normal windows
    self.window.collectionBehavior = NSWindowCollectionBehaviorCanJoinAllSpaces
                                    | NSWindowCollectionBehaviorFullScreenAuxiliary
                                    | NSWindowCollectionBehaviorStationary;
    self.window.ignoresMouseEvents = NO;  // need clicks so the view can accept key focus
    self.window.movable          = NO;

    self.piano = [[PianoView alloc] initWithFrame:NSMakeRect(0, 0, screen.size.width, screen.size.height)];
    self.piano.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    self.window.contentView = self.piano;
    [self.window makeFirstResponder:self.piano];
    [self.window makeKeyAndOrderFront:nil];
    self.visible = YES;

    // Status-bar menu entry — Show/Hide + Quit. Uses a single-letter "N"
    // as a placeholder glyph; swap with an NSImage later for real polish.
    self.status = [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
    self.status.button.title = @"N";
    NSMenu *menu = [[NSMenu alloc] init];
    [menu addItemWithTitle:@"Show / Hide" action:@selector(toggleVisible) keyEquivalent:@""];
    [menu addItem:[NSMenuItem separatorItem]];
    NSMenuItem *quit = [[NSMenuItem alloc] initWithTitle:@"Quit" action:@selector(terminate:) keyEquivalent:@"q"];
    quit.target = NSApp;
    [menu addItem:quit];
    self.status.menu = menu;

    // Poll the Carbon hotkey flag on the main thread — Cocoa event loop
    // won't wake for us otherwise. 16ms period (60Hz) is plenty.
    [NSTimer scheduledTimerWithTimeInterval:0.016
                                    repeats:YES
                                      block:^(NSTimer * _Nonnull t) {
        if (g_toggle_visible) { g_toggle_visible = 0; [self toggleVisible]; }
    }];
}

- (void)applicationWillTerminate:(NSNotification *)note {
    if (g_hotkey) UnregisterEventHotKey(g_hotkey);
    if (g_hotkey_handler) RemoveEventHandler(g_hotkey_handler);
    if (g_audio) audio_destroy(g_audio);
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app { return NO; }

@end

int main(int argc, const char **argv) {
    (void)argc; (void)argv;
    @autoreleasepool {
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        AppDelegate *del = [[AppDelegate alloc] init];
        [NSApp setDelegate:del];
        [NSApp run];
    }
    return 0;
}
