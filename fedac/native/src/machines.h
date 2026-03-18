#pragma once

// AC Machines — system-level remote monitoring daemon
// Runs in the main loop, independent of which piece is loaded.
// Connects to wss://session-server.aesthetic.computer/machines

#include "ws-client.h"
#include "wifi.h"

#define MACHINES_HEARTBEAT_FRAMES  1800  // ~30s at 60fps
#define MACHINES_RECONNECT_FRAMES  300   // ~5s
#define MACHINES_SEND_QUEUE_SIZE   16
#define MACHINES_SEND_MSG_SIZE     16384

typedef struct {
    ACWs *ws;
    int   connected;              // 1 after register sent
    int   reconnect_frame;        // frame to attempt reconnect (0 = none)
    int   last_heartbeat_frame;
    char  current_piece[64];
    char  device_token[512];      // from /mnt/.device-token

    // Send queue (ws_send is single-slot, we may queue multiple on connect)
    char  send_queue[MACHINES_SEND_QUEUE_SIZE][MACHINES_SEND_MSG_SIZE];
    int   sq_head;
    int   sq_tail;
    int   sq_count;

    // Pending command from server → forwarded to JS runtime
    volatile int cmd_pending;
    char  cmd_type[32];           // "jump", "reboot", "update", "request-logs"
    char  cmd_target[128];        // e.g. piece name for "jump"
    char  cmd_id[32];             // commandId for ack
} ACMachines;

// Call once at startup (after init_machine_id, before main loop)
void machines_init(ACMachines *m);

// Call once per frame from the main loop
void machines_tick(ACMachines *m, ACWifi *wifi, int frame, int fps,
                   const char *current_piece);

// Flush final logs before shutdown (blocking drain of send queue)
void machines_flush_logs(ACMachines *m);

// Call at shutdown
void machines_destroy(ACMachines *m);
