#include "usb-midi.h"
#include <alsa/asoundlib.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

extern void ac_log(const char *fmt, ...);

static pthread_mutex_t g_usb_midi_lock = PTHREAD_MUTEX_INITIALIZER;
static snd_rawmidi_t *g_usb_midi_out = NULL;
static char g_usb_midi_device[64] = {0};
static time_t g_usb_midi_next_retry = 0;

static void usb_midi_state_init(ACUsbMidiState *state) {
    if (!state) return;
    memset(state, 0, sizeof(*state));
    snprintf(state->reason, sizeof(state->reason), "unknown");
}

int usb_midi_read_state(ACUsbMidiState *state) {
    usb_midi_state_init(state);
    if (!state) return 0;

    FILE *fp = fopen("/run/usb-midi.state", "r");
    if (!fp) return 0;

    char line[256];
    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\r\n")] = 0;
        char *eq = strchr(line, '=');
        if (!eq) continue;
        *eq = 0;
        const char *key = line;
        const char *value = eq + 1;
        if (strcmp(key, "enabled") == 0) state->enabled = atoi(value) != 0;
        else if (strcmp(key, "active") == 0) state->active = atoi(value) != 0;
        else if (strcmp(key, "reason") == 0) snprintf(state->reason, sizeof(state->reason), "%s", value);
        else if (strcmp(key, "udc") == 0) snprintf(state->udc, sizeof(state->udc), "%s", value);
        else if (strcmp(key, "port") == 0) snprintf(state->port, sizeof(state->port), "%s", value);
        else if (strcmp(key, "power_role") == 0) snprintf(state->power_role, sizeof(state->power_role), "%s", value);
        else if (strcmp(key, "data_role") == 0) snprintf(state->data_role, sizeof(state->data_role), "%s", value);
        else if (strcmp(key, "alsa_device") == 0) snprintf(state->alsa_device, sizeof(state->alsa_device), "%s", value);
        else if (strcmp(key, "serial") == 0) snprintf(state->serial, sizeof(state->serial), "%s", value);
    }
    fclose(fp);
    return 1;
}

static int usb_midi_find_card_from_proc(void) {
    FILE *fp = fopen("/proc/asound/cards", "r");
    if (!fp) return -1;

    int found = -1;
    char line[256];
    while (fgets(line, sizeof(line), fp)) {
        int card = -1;
        if (sscanf(line, " %d [", &card) == 1) {
            if (strstr(line, "MIDI Gadget") || strstr(line, "f_midi")) {
                found = card;
                break;
            }
        }
    }
    fclose(fp);
    return found;
}

static int usb_midi_resolve_device(char *out, size_t out_sz) {
    if (!out || out_sz == 0) return 0;
    out[0] = 0;

    ACUsbMidiState state;
    if (usb_midi_read_state(&state) && state.alsa_device[0]) {
        snprintf(out, out_sz, "%s", state.alsa_device);
        return 1;
    }

    int card = usb_midi_find_card_from_proc();
    if (card < 0) return 0;
    snprintf(out, out_sz, "hw:%d,0,0", card);
    return 1;
}

static int usb_midi_ensure_open_locked(void) {
    if (g_usb_midi_out) return 1;

    time_t now = time(NULL);
    if (g_usb_midi_next_retry && now < g_usb_midi_next_retry) return 0;

    char device[64];
    if (!usb_midi_resolve_device(device, sizeof(device))) {
        g_usb_midi_next_retry = now + 1;
        return 0;
    }

    int rc = snd_rawmidi_open(NULL, &g_usb_midi_out, device, SND_RAWMIDI_NONBLOCK);
    if (rc < 0) {
        ac_log("[usb-midi] open %s failed: %s", device, snd_strerror(rc));
        g_usb_midi_out = NULL;
        g_usb_midi_next_retry = now + 1;
        return 0;
    }

    snd_rawmidi_nonblock(g_usb_midi_out, 1);
    snprintf(g_usb_midi_device, sizeof(g_usb_midi_device), "%s", device);
    g_usb_midi_next_retry = 0;
    ac_log("[usb-midi] opened %s", g_usb_midi_device);
    return 1;
}

static int usb_midi_send_locked(const unsigned char *msg, size_t len) {
    if (!msg || len == 0) return 0;
    if (!usb_midi_ensure_open_locked()) return 0;

    size_t sent = 0;
    int attempts = 0;
    while (sent < len && attempts < 4) {
        ssize_t rc = snd_rawmidi_write(g_usb_midi_out, msg + sent, len - sent);
        if (rc > 0) {
            sent += (size_t)rc;
            continue;
        }
        if (rc == -EAGAIN || rc == 0) {
            attempts++;
            usleep(1000);
            continue;
        }
        ac_log("[usb-midi] write %s failed: %s", g_usb_midi_device[0] ? g_usb_midi_device : "(unknown)", snd_strerror((int)rc));
        snd_rawmidi_close(g_usb_midi_out);
        g_usb_midi_out = NULL;
        g_usb_midi_device[0] = 0;
        g_usb_midi_next_retry = time(NULL) + 1;
        return 0;
    }

    return sent == len;
}

static int usb_midi_send_message(const unsigned char *msg, size_t len) {
    int ok;
    pthread_mutex_lock(&g_usb_midi_lock);
    ok = usb_midi_send_locked(msg, len);
    pthread_mutex_unlock(&g_usb_midi_lock);
    return ok;
}

void usb_midi_close(void) {
    pthread_mutex_lock(&g_usb_midi_lock);
    if (g_usb_midi_out) {
        snd_rawmidi_close(g_usb_midi_out);
        g_usb_midi_out = NULL;
    }
    g_usb_midi_device[0] = 0;
    g_usb_midi_next_retry = 0;
    pthread_mutex_unlock(&g_usb_midi_lock);
}

static int clamp_midi_u7(int value) {
    if (value < 0) return 0;
    if (value > 127) return 127;
    return value;
}

static int clamp_channel(int channel) {
    if (channel < 0) return 0;
    if (channel > 15) return 15;
    return channel;
}

int usb_midi_note_on(int note, int velocity, int channel) {
    unsigned char msg[3];
    msg[0] = (unsigned char)(0x90 | clamp_channel(channel));
    msg[1] = (unsigned char)clamp_midi_u7(note);
    msg[2] = (unsigned char)clamp_midi_u7(velocity);
    int ok = usb_midi_send_message(msg, sizeof(msg));
    if (ok) ac_log("[usb-midi] note-on ch=%d note=%d vel=%d", clamp_channel(channel), clamp_midi_u7(note), clamp_midi_u7(velocity));
    return ok;
}

int usb_midi_note_off(int note, int velocity, int channel) {
    unsigned char msg[3];
    msg[0] = (unsigned char)(0x80 | clamp_channel(channel));
    msg[1] = (unsigned char)clamp_midi_u7(note);
    msg[2] = (unsigned char)clamp_midi_u7(velocity);
    int ok = usb_midi_send_message(msg, sizeof(msg));
    if (ok) ac_log("[usb-midi] note-off ch=%d note=%d vel=%d", clamp_channel(channel), clamp_midi_u7(note), clamp_midi_u7(velocity));
    return ok;
}

int usb_midi_all_notes_off(int channel) {
    unsigned char all_notes_off[3];
    unsigned char all_sound_off[3];
    int ch = clamp_channel(channel);

    all_notes_off[0] = (unsigned char)(0xB0 | ch);
    all_notes_off[1] = 123;
    all_notes_off[2] = 0;

    all_sound_off[0] = (unsigned char)(0xB0 | ch);
    all_sound_off[1] = 120;
    all_sound_off[2] = 0;

    int ok = usb_midi_send_message(all_sound_off, sizeof(all_sound_off)) &&
             usb_midi_send_message(all_notes_off, sizeof(all_notes_off));
    if (ok) ac_log("[usb-midi] all-notes-off ch=%d", ch);
    return ok;
}
