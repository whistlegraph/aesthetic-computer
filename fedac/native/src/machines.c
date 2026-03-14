// machines.c — System-level AC Machines monitoring daemon
// Manages a WebSocket connection to session-server for device registration,
// heartbeats, log upload, and remote command reception.

#include "machines.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

extern void ac_log(const char *fmt, ...);
extern char g_machine_id[64];

// Compile-time build info (set by Makefile)
#ifndef AC_BUILD_NAME
#define AC_BUILD_NAME "dev"
#endif
#ifndef AC_GIT_HASH
#define AC_GIT_HASH "unknown"
#endif
#ifndef AC_BUILD_TS
#define AC_BUILD_TS "unknown"
#endif

#define WS_URL "wss://session-server.aesthetic.computer/machines"

// ── Helpers ──────────────────────────────────────────────────

static int read_file(const char *path, char *buf, int bufsize) {
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    int len = (int)fread(buf, 1, bufsize - 1, f);
    fclose(f);
    if (len > 0 && buf[len - 1] == '\n') len--;
    buf[len] = 0;
    return len;
}

static void read_battery(int *percent, int *charging) {
    char buf[64];
    *percent = -1;
    *charging = 0;
    const char *bat_names[] = {"BAT0", "BAT1", NULL};
    for (int i = 0; bat_names[i]; i++) {
        char path[128];
        snprintf(path, sizeof(path), "/sys/class/power_supply/%s/capacity", bat_names[i]);
        if (read_file(path, buf, sizeof(buf)) > 0) {
            *percent = atoi(buf);
            snprintf(path, sizeof(path), "/sys/class/power_supply/%s/status", bat_names[i]);
            if (read_file(path, buf, sizeof(buf)) > 0)
                *charging = (strcmp(buf, "Charging") == 0);
            return;
        }
    }
}

// Simple JSON string extraction: find "key":"value" and copy value to out.
static int json_get_str(const char *json, const char *key, char *out, int out_sz) {
    char needle[128];
    snprintf(needle, sizeof(needle), "\"%s\":\"", key);
    const char *p = strstr(json, needle);
    if (!p) return -1;
    p += strlen(needle);
    const char *end = strchr(p, '"');
    if (!end) return -1;
    int len = (int)(end - p);
    if (len >= out_sz) len = out_sz - 1;
    memcpy(out, p, len);
    out[len] = 0;
    return len;
}

// Escape a string for JSON embedding (backslash, quotes, control chars).
// Returns number of bytes written (excluding null terminator).
static int json_escape(const char *in, int in_len, char *out, int out_sz) {
    int j = 0;
    for (int i = 0; i < in_len && j < out_sz - 2; i++) {
        unsigned char c = (unsigned char)in[i];
        if (c == '"' || c == '\\') {
            if (j + 2 >= out_sz) break;
            out[j++] = '\\';
            out[j++] = c;
        } else if (c == '\n') {
            if (j + 2 >= out_sz) break;
            out[j++] = '\\';
            out[j++] = 'n';
        } else if (c == '\r') {
            if (j + 2 >= out_sz) break;
            out[j++] = '\\';
            out[j++] = 'r';
        } else if (c == '\t') {
            if (j + 2 >= out_sz) break;
            out[j++] = '\\';
            out[j++] = 't';
        } else if (c < 0x20) {
            // Skip other control chars
        } else {
            out[j++] = c;
        }
    }
    out[j] = 0;
    return j;
}

// ── Send queue (drains one per frame when ws slot is free) ────

static void sq_push(ACMachines *m, const char *msg) {
    if (m->sq_count >= MACHINES_SEND_QUEUE_SIZE) {
        ac_log("[machines] send queue full, dropping message\n");
        return;
    }
    int slot = (m->sq_head + m->sq_count) % MACHINES_SEND_QUEUE_SIZE;
    strncpy(m->send_queue[slot], msg, MACHINES_SEND_MSG_SIZE - 1);
    m->send_queue[slot][MACHINES_SEND_MSG_SIZE - 1] = 0;
    m->sq_count++;
}

static void sq_drain_one(ACMachines *m) {
    if (m->sq_count <= 0 || !m->ws) return;
    // Only send if ws send slot is free
    if (m->ws->send_pending) return;
    ws_send(m->ws, m->send_queue[m->sq_head]);
    m->sq_head = (m->sq_head + 1) % MACHINES_SEND_QUEUE_SIZE;
    m->sq_count--;
}

// ── Connection ───────────────────────────────────────────────

static void machines_connect(ACMachines *m) {
    char url[768];
    const char *mid = g_machine_id[0] ? g_machine_id : "unknown";
    if (m->device_token[0]) {
        snprintf(url, sizeof(url), "%s?role=device&machineId=%s&token=%s",
                 WS_URL, mid, m->device_token);
    } else {
        snprintf(url, sizeof(url), "%s?role=device&machineId=%s", WS_URL, mid);
    }
    ws_connect(m->ws, url);
    m->connected = 0;
    ac_log("[machines] connecting: %s\n", mid);
}

// ── Register (sent on connect) ───────────────────────────────

static void send_register(ACMachines *m, ACWifi *wifi) {
    int bat_pct, bat_chg;
    read_battery(&bat_pct, &bat_chg);

    char hw[128] = "";
    read_file("/sys/devices/virtual/dmi/id/product_name", hw, sizeof(hw));

    char hostname[64] = "";
    read_file("/etc/hostname", hostname, sizeof(hostname));

    char msg[2048];
    snprintf(msg, sizeof(msg),
        "{\"type\":\"register\","
        "\"version\":\"%s %s-%s\","
        "\"buildName\":\"%s\","
        "\"gitHash\":\"%s\","
        "\"buildTs\":\"%s\","
        "\"currentPiece\":\"%s\","
        "\"ip\":\"%s\","
        "\"wifiSSID\":\"%s\","
        "\"battery\":%d,"
        "\"charging\":%s,"
        "\"hostname\":\"%s\","
        "\"hw\":{\"display\":\"%s\"}}",
        AC_BUILD_NAME, AC_GIT_HASH, AC_BUILD_TS,
        AC_BUILD_NAME, AC_GIT_HASH, AC_BUILD_TS,
        m->current_piece,
        wifi ? wifi->ip_address : "",
        wifi ? wifi->connected_ssid : "",
        bat_pct,
        bat_chg ? "true" : "false",
        hostname,
        hw);
    sq_push(m, msg);
    ac_log("[machines] registered\n");
}

// ── Session log upload (on connect) ──────────────────────────

static void upload_session_log(ACMachines *m) {
    char raw[3200] = "";
    int len = read_file("/mnt/ac-native.log", raw, sizeof(raw));
    if (len <= 0) return;

    // JSON-escape the log content
    char escaped[3600];
    json_escape(raw, len, escaped, sizeof(escaped));

    char msg[4000];
    snprintf(msg, sizeof(msg),
        "{\"type\":\"log\",\"logType\":\"session\","
        "\"message\":\"%s\"}", escaped);
    sq_push(m, msg);
    ac_log("[machines] session log queued (%d bytes)\n", len);
}

// ── Crash report upload ──────────────────────────────────────

static void upload_crash_report(ACMachines *m) {
    char raw[2048] = "";
    int len = read_file("/mnt/crash.json", raw, sizeof(raw));
    if (len <= 0) return;

    // crash.json is already JSON, embed it as data
    char msg[3072];
    snprintf(msg, sizeof(msg),
        "{\"type\":\"log\",\"logType\":\"crash\","
        "\"data\":%s}", raw);
    sq_push(m, msg);

    // Clear crash file
    FILE *f = fopen("/mnt/crash.json", "w");
    if (f) fclose(f);
    ac_log("[machines] crash report queued\n");
}

// ── Heartbeat ────────────────────────────────────────────────

static void send_heartbeat(ACMachines *m, int frame, int fps) {
    int bat_pct, bat_chg;
    read_battery(&bat_pct, &bat_chg);

    char msg[512];
    snprintf(msg, sizeof(msg),
        "{\"type\":\"heartbeat\","
        "\"currentPiece\":\"%s\","
        "\"battery\":%d,"
        "\"charging\":%s,"
        "\"uptime\":%d,"
        "\"fps\":%d}",
        m->current_piece,
        bat_pct,
        bat_chg ? "true" : "false",
        frame,
        fps);
    sq_push(m, msg);
}

// ── Command ack ──────────────────────────────────────────────

static void send_ack(ACMachines *m, const char *cmd_id, const char *cmd) {
    char msg[256];
    snprintf(msg, sizeof(msg),
        "{\"type\":\"command-ack\",\"commandId\":\"%s\",\"command\":\"%s\"}",
        cmd_id, cmd);
    sq_push(m, msg);
}

// ── Process incoming messages ────────────────────────────────

static void process_messages(ACMachines *m) {
    int count = ws_poll(m->ws);
    for (int i = 0; i < count; i++) {
        const char *raw = m->ws->messages[i];

        // Check if it's a command message
        if (!strstr(raw, "\"type\":\"command\"")) continue;

        char cmd[32] = "", target[128] = "", cmd_id[32] = "";
        json_get_str(raw, "command", cmd, sizeof(cmd));
        json_get_str(raw, "commandId", cmd_id, sizeof(cmd_id));
        json_get_str(raw, "target", target, sizeof(target));

        ac_log("[machines] command: %s target=%s id=%s\n", cmd, target, cmd_id);

        // Send ack immediately
        send_ack(m, cmd_id, cmd);

        if (strcmp(cmd, "reboot") == 0) {
            ac_log("[machines] rebooting by remote command\n");
            sync();
            reboot(0x01234567); // LINUX_REBOOT_CMD_RESTART
        } else if (strcmp(cmd, "jump") == 0 ||
                   strcmp(cmd, "update") == 0 ||
                   strcmp(cmd, "request-logs") == 0) {
            // Forward to main loop via cmd_pending
            if (!m->cmd_pending) {
                strncpy(m->cmd_type, cmd, sizeof(m->cmd_type) - 1);
                strncpy(m->cmd_target, target, sizeof(m->cmd_target) - 1);
                strncpy(m->cmd_id, cmd_id, sizeof(m->cmd_id) - 1);
                m->cmd_pending = 1;
            }
        }
    }
}

// ── Public API ───────────────────────────────────────────────

void machines_init(ACMachines *m) {
    memset(m, 0, sizeof(*m));
    m->ws = ws_create();
    read_file("/mnt/.device-token", m->device_token, sizeof(m->device_token));
    strncpy(m->current_piece, "notepat", sizeof(m->current_piece) - 1);
    ac_log("[machines] init: id=%s token=%s\n",
           g_machine_id, m->device_token[0] ? "yes" : "no");
}

void machines_tick(ACMachines *m, ACWifi *wifi, int frame, int fps,
                   const char *current_piece) {
    if (!m->ws) return;

    // Track current piece
    if (current_piece && current_piece[0]) {
        strncpy(m->current_piece, current_piece, sizeof(m->current_piece) - 1);
    }

    int wifi_up = (wifi && wifi->state == WIFI_STATE_CONNECTED);

    // Auto-connect when wifi is up and ws isn't active
    if (wifi_up && !m->ws->connected && !m->ws->connecting &&
        !m->connected && m->reconnect_frame == 0) {
        m->reconnect_frame = frame + 60; // ~1s delay
    }

    // Reconnect timer
    if (m->reconnect_frame > 0 && frame >= m->reconnect_frame) {
        m->reconnect_frame = 0;
        if (!m->ws->connected && !m->ws->connecting && wifi_up) {
            machines_connect(m);
        }
    }

    // Just connected → register + upload logs
    if (m->ws->connected && !m->connected) {
        m->connected = 1;
        m->last_heartbeat_frame = frame;
        send_register(m, wifi);
        upload_session_log(m);
        upload_crash_report(m);
    }

    // Disconnected → schedule reconnect
    if (!m->ws->connected && !m->ws->connecting && m->connected) {
        m->connected = 0;
        ac_log("[machines] disconnected\n");
        if (wifi_up) m->reconnect_frame = frame + MACHINES_RECONNECT_FRAMES;
    }

    // Heartbeat every ~30s
    if (m->connected && m->ws->connected &&
        (frame - m->last_heartbeat_frame) >= MACHINES_HEARTBEAT_FRAMES) {
        m->last_heartbeat_frame = frame;
        send_heartbeat(m, frame, fps);
    }

    // Process incoming messages (commands)
    if (m->connected) {
        process_messages(m);
    }

    // Drain send queue (one message per frame)
    if (m->connected && m->ws->connected) {
        sq_drain_one(m);
    }
}

void machines_destroy(ACMachines *m) {
    if (m->ws) {
        ws_close(m->ws);
        ws_destroy(m->ws);
        m->ws = NULL;
    }
}
