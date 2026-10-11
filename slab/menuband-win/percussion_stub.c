// percussion_stub.c — silent stand-in for percussion.c while the real engine
// is being ported. build.ps1 compiles this only when percussion.c is absent.
#include "percussion.h"
void perc_init(double sample_rate) { (void)sample_rate; }
void perc_hit_drum(PercDrum drum, double velocity, double pan, uint64_t group) { (void)drum; (void)velocity; (void)pan; (void)group; }
PercDrum perc_strike_pad(double x, double y, double velocity, const double *anchors_xy, int anchor_count, uint64_t group) { (void)x; (void)y; (void)velocity; (void)anchors_xy; (void)anchor_count; (void)group; return PERC_KICK; }
void perc_lift_pad(double x, double y, uint64_t group) { (void)x; (void)y; (void)group; }
void perc_release(uint64_t group) { (void)group; }
void perc_flush(void) {}
void perc_set_pitch(double scale) { (void)scale; }
void perc_set_level(double level) { (void)level; }
void perc_render(float *out, int frames) { (void)out; (void)frames; }
static const char *LABELS[PERC_DRUM_COUNT] = { "BAS", "CRS", "SNR", "SPL", "CLP", "SNP", "CBL", "HHC", "BLK", "HHO", "TMB", "RDE" };
const char *perc_label(PercDrum drum) { return (drum >= 0 && drum < PERC_DRUM_COUNT) ? LABELS[drum] : "?"; }
int perc_active_voices(void) { return 0; }
