// surface-audit.c — trackpad multitouch regression harness.
//
// Replays a synthetic MT-B event stream through the surface tracker in
// input.c and checks the contact set it produces. This is the regression
// backbone for playing the trackpad as a surface: the drum head needs every
// finger, since the resting ones damp the modes the striking one excites,
// so a dropped or misattributed contact is an audible bug, not a cosmetic
// one. The harness includes input.c directly to reach its statics.
//
// Build:  cc -O2 -I ../src surface-audit.c -lm -o /tmp/surface-audit
// Run:    /tmp/surface-audit
//
// PASS = every check below. Linux only (evdev headers).

#include "input.c"

static ACInput *I;

static void feed(int type, int code, int value) {
    struct input_event ev = { .type = type, .code = code, .value = value };
    if (type == EV_ABS) surface_abs(&I->surface, &ev);
    else if (type == EV_SYN && code == SYN_REPORT) surface_sync(I);
}

#define SLOT(n)  feed(EV_ABS, ABS_MT_SLOT, n)
#define ID(n)    feed(EV_ABS, ABS_MT_TRACKING_ID, n)
#define X(n)     feed(EV_ABS, ABS_MT_POSITION_X, n)
#define Y(n)     feed(EV_ABS, ABS_MT_POSITION_Y, n)
#define SYN()    feed(EV_SYN, SYN_REPORT, 0)

// Synaptics ranges from a ThinkPad pad, the first target hardware.
#define PAD_X0 1266
#define PAD_X1 5676
#define PAD_Y0 1096
#define PAD_Y1 4758
#define PAD_CX ((PAD_X0 + PAD_X1) / 2)
#define PAD_CY ((PAD_Y0 + PAD_Y1) / 2)

static int fails = 0;

static void check(const char *what, int got, int want) {
    if (got != want) { printf("  FAIL %s: got %d want %d\n", what, got, want); fails++; }
    else printf("  ok   %s = %d\n", what, got);
}

static void checkf(const char *what, float got, float want) {
    if (fabsf(got - want) > 0.002f) {
        printf("  FAIL %s: got %.4f want %.4f\n", what, got, want); fails++;
    } else printf("  ok   %s = %.3f\n", what, got);
}

static void advanced(const char *what, int before, int now) {
    if (now > before) printf("  ok   %s: %d -> %d\n", what, before, now);
    else { printf("  FAIL %s: stuck at %d\n", what, now); fails++; }
}

int main(void) {
    I = calloc(1, sizeof(ACInput));
    I->scale = 1; I->screen_w = 1920; I->screen_h = 1080;
    I->pointer_x = 500; I->pointer_y = 500;
    ACTouchSurface *s = &I->surface;
    s->fd_index = 0; s->has_slots = 1;
    s->x_min = PAD_X0; s->x_max = PAD_X1;
    s->y_min = PAD_Y0; s->y_max = PAD_Y1;
    s->x_res = 42; s->y_res = 42;
    surface_clear(s);
    s->aspect = input_touch_aspect(I);

    printf("pad aspect drives the membrane boundary:\n");
    checkf("aspect", s->aspect, (float)(PAD_X1 - PAD_X0) / (float)(PAD_Y1 - PAD_Y0));

    printf("\none finger down, dead center:\n");
    SLOT(0); ID(77); X(PAD_CX); Y(PAD_CY); SYN();
    check("contacts", s->contacts, 1);
    ACTouchSlot pts[MAX_TOUCH_SLOTS];
    int n = input_touch_contacts(I, pts, MAX_TOUCH_SLOTS);
    check("snapshot", n, 1);
    checkf("x", pts[0].x, 0.5f);
    checkf("y", pts[0].y, 0.5f);

    printf("\nsecond finger rests at a corner:\n");
    SLOT(1); ID(78); X(PAD_X0); Y(PAD_Y0); SYN();
    check("contacts", s->contacts, 2);
    check("snapshot", input_touch_contacts(I, pts, MAX_TOUCH_SLOTS), 2);
    checkf("corner x", pts[1].x, 0.0f);
    checkf("corner y", pts[1].y, 0.0f);

    printf("\na resting finger must not drag the pointer:\n");
    int px = I->pointer_x;
    SLOT(1); X(PAD_X0 + 134); SYN();
    check("pointer held", I->pointer_x, px);

    printf("\nthe primary finger does move it:\n");
    SLOT(0); X(PAD_CX + 420); SYN();
    advanced("pointer", px, I->pointer_x);

    printf("\nprimary lifts — handoff without a jump:\n");
    px = I->pointer_x;
    SLOT(0); ID(-1); SYN();
    check("contacts", s->contacts, 1);
    check("pointer held", I->pointer_x, px);
    check("primary reassigned", s->primary_id, 78);

    printf("\nthe surviving finger now drives it:\n");
    SLOT(1); X(PAD_X0 + 1134); SYN();
    advanced("pointer", px, I->pointer_x);

    printf("\nall fingers up:\n");
    SLOT(1); ID(-1); SYN();
    check("contacts", s->contacts, 0);
    check("snapshot", input_touch_contacts(I, pts, MAX_TOUCH_SLOTS), 0);
    check("primary cleared", s->primary_id, -1);

    printf("\na slot past the array is discarded, not written through:\n");
    SLOT(MAX_TOUCH_SLOTS + 3); ID(90); X(3000); Y(3000); SYN();
    check("contacts", s->contacts, 0);

    printf("\na contact is withheld until it has both coordinates:\n");
    SLOT(2); ID(91); SYN();
    check("id alone", s->contacts, 0);
    X(3000); SYN();
    check("x only", s->contacts, 0);
    Y(3000); SYN();
    check("x and y", s->contacts, 1);

    printf("\ncoordinates clamp to the pad:\n");
    SLOT(2); X(PAD_X1 + 9999); Y(PAD_Y0 - 5000); SYN();
    input_touch_contacts(I, pts, MAX_TOUCH_SLOTS);
    checkf("clamped x", pts[0].x, 1.0f);
    checkf("clamped y", pts[0].y, 0.0f);

    // A drum reads `generation` to know the hand changed. Counting contacts
    // alone could not see one finger land as another lifted inside a single
    // report — same count, different hand — and an instrument polling per
    // frame would swallow that strike entirely.
    printf("\none finger replacing another in one report is a change:\n");
    SLOT(2); ID(-1); SLOT(3); ID(92); X(PAD_CX); Y(PAD_CY); SYN();
    check("contacts", s->contacts, 1);
    int gen_swap = s->generation;
    SLOT(3); ID(-1); SLOT(4); ID(93); X(PAD_CX); Y(PAD_CY); SYN();
    check("still one contact", s->contacts, 1);
    advanced("generation", gen_swap, s->generation);

    printf("\nan unchanged hand does not bump generation:\n");
    int gen_still = s->generation;
    SYN();
    check("generation held", s->generation, gen_still);

    printf("\n%s (%d failure%s)\n", fails ? "FAILED" : "PASSED",
           fails, fails == 1 ? "" : "s");
    return fails != 0;
}
