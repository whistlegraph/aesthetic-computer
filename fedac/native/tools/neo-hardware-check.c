// SPDX-License-Identifier: GPL-2.0-or-later
// RAM-boot checks: enumerate real devices, test audio only on snd-aloop.
// A loopback pass says nothing about physical speaker support.
#define _GNU_SOURCE
#include <alsa/asoundlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <linux/input.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>

static double now(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + t.tv_nsec / 1e9;
}

static int count_entries(const char *path) {
    int count = 0;
    DIR *d = opendir(path);
    if (d) {
        struct dirent *e;
        while ((e = readdir(d))) if (e->d_name[0] != '.') count++;
        closedir(d);
    }
    return count;
}

static int keyboards(void) {
    DIR *d = opendir("/dev/input");
    int count = 0;
    if (!d) return 0;
    struct dirent *e;
    while ((e = readdir(d))) {
        if (strncmp(e->d_name, "event", 5)) continue;
        char path[512];
        snprintf(path, sizeof(path), "/dev/input/%s", e->d_name);
        int fd = open(path, O_RDONLY | O_NONBLOCK);
        if (fd < 0) continue;
        unsigned char keys[(KEY_MAX + 8) / 8] = {0};
        if (ioctl(fd, EVIOCGBIT(EV_KEY, sizeof(keys)), keys) >= 0 &&
            (keys[KEY_A / 8] & (1 << (KEY_A % 8))) &&
            (keys[KEY_ENTER / 8] & (1 << (KEY_ENTER % 8)))) count++;
        close(fd);
    }
    closedir(d);
    return count;
}

static int16_t sample(unsigned frame, unsigned channel) {
    // A nonzero deterministic pattern, two independent channels, < -24 dBFS.
    return (int16_t)(1 + ((frame * 251u + channel * 997u) % 1800u));
}

static const char *loopback_test(int *frames_verified) {
    snd_ctl_t *ctl = NULL;
    snd_pcm_t *play = NULL, *capture = NULL;
    snd_ctl_card_info_t *info;
    snd_ctl_card_info_alloca(&info);
    const char *result = "unavailable";
    // Refuse any other card. Never fall back to default or a numeric card.
    if (snd_ctl_open(&ctl, "hw:Loopback", 0) < 0) return result;
    if (snd_ctl_card_info(ctl, info) < 0 ||
        strcmp(snd_ctl_card_info_get_id(info), "Loopback") ||
        strcmp(snd_ctl_card_info_get_driver(info), "Loopback")) goto done;
    // AC OS uses substream 0; isolate this test on substream 7.
    if (snd_pcm_open(&capture, "hw:Loopback,1,7", SND_PCM_STREAM_CAPTURE, SND_PCM_NONBLOCK) < 0 ||
        snd_pcm_open(&play, "hw:Loopback,0,7", SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK) < 0) goto done;
    result = "failed";
    if (snd_pcm_set_params(capture, SND_PCM_FORMAT_S16_LE, SND_PCM_ACCESS_RW_INTERLEAVED,
                           2, 48000, 0, 500000) < 0 ||
        snd_pcm_set_params(play, SND_PCM_FORMAT_S16_LE, SND_PCM_ACCESS_RW_INTERLEAVED,
                           2, 48000, 0, 500000) < 0) goto done;
    enum { FRAMES = 4096 };
    int16_t sent[FRAMES * 2], received[1024 * 2];
    for (unsigned i = 0; i < FRAMES; i++) {
        sent[i * 2] = sample(i, 0); sent[i * 2 + 1] = sample(i, 1);
    }
    if (snd_pcm_start(capture) < 0) goto done;
    if (snd_pcm_writei(play, sent, FRAMES) != FRAMES) goto done;
    if (snd_pcm_state(play) == SND_PCM_STATE_PREPARED && snd_pcm_start(play) < 0) goto done;
    double deadline = now() + 2;
    unsigned matched = 0;
    while (now() < deadline && matched < FRAMES) {
        snd_pcm_sframes_t n = snd_pcm_readi(capture, received, 1024);
        if (n == -EAGAIN) { usleep(1000); continue; }
        if (n < 0) goto done;
        for (snd_pcm_sframes_t i = 0; i < n && matched < FRAMES; i++) {
            int16_t left = received[i * 2], right = received[i * 2 + 1];
            if (!matched && left == 0 && right == 0) continue;
            if (left != sample(matched, 0) || right != sample(matched, 1)) goto done;
            matched++;
        }
    }
    *frames_verified = (int)matched;
    if (matched == FRAMES) result = "passed";
done:
    if (play) snd_pcm_close(play);
    if (capture) snd_pcm_close(capture);
    if (ctl) snd_ctl_close(ctl);
    return result;
}

int main(void) {
    int keyboard_count = keyboards(), backlights = count_entries("/sys/class/backlight");
    int radios = count_entries("/sys/class/ieee80211");
    int physical_cards = 0, card = -1, frames = 0;
    while (snd_card_next(&card) >= 0 && card >= 0) {
        char name[32];
        snd_ctl_t *ctl;
        snd_ctl_card_info_t *info;
        snd_ctl_card_info_alloca(&info);
        snprintf(name, sizeof(name), "hw:%d", card);
        if (snd_ctl_open(&ctl, name, 0) < 0) continue;
        if (snd_ctl_card_info(ctl, info) >= 0 &&
            strcmp(snd_ctl_card_info_get_driver(info), "Loopback") &&
            strcmp(snd_ctl_card_info_get_driver(info), "Dummy")) physical_cards++;
        snd_ctl_close(ctl);
    }
    const char *audio = loopback_test(&frames);
    printf("{\n"
           "  \"keyboard\": {\"devices\": %d, \"enumeration\": \"%s\", \"physical_key_test\": \"not_run\"},\n"
           "  \"sound\": {\"nonvirtual_cards\": %d, \"physical_speaker_test\": \"not_run\", "
           "\"loopback_test\": \"%s\", \"verified_stereo_frames\": %d, \"rate\": 48000},\n"
           "  \"brightness\": {\"controllers\": %d, \"write_test\": \"not_run\"},\n"
           "  \"wifi\": {\"radios\": %d, \"connection_test\": \"not_run\"}\n}\n",
           keyboard_count, keyboard_count ? "present" : "missing", physical_cards,
           audio, frames, backlights, radios);
    return !strcmp(audio, "failed") ? 1 : 0;
}
