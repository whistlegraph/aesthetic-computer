#pragma once

// Minimal TLS WebSocket client for ac-native bare metal
// Connect + recv run in a background pthread; main thread only polls.

#include <pthread.h>

#define WS_MAX_MESSAGES  16
#define WS_MAX_MSG_LEN   (256 * 1024)   // 256KB — chat "connected" history can be large

typedef struct {
    // --- background thread ---
    pthread_t thread;
    int       thread_running;

    // pending connect request (written by main, read by thread)
    char  pending_host[256];
    char  pending_path[256];
    int   pending_connect;   // 1 = thread should connect

    // connection state (written by thread, read by main)
    int   connected;         // 1 = WS handshake complete
    int   connecting;        // 1 = currently in progress
    int   error;             // 1 = last connect failed

    // outgoing send queue (written by main, drained by thread)
    char  send_buf[4096];
    int   send_pending;      // 1 = send_buf has data to send

    // inbound message queue (written by thread, consumed by main each frame)
    char  messages[WS_MAX_MESSAGES][WS_MAX_MSG_LEN];
    int   msg_count;         // written by thread
    int   msg_read;          // read index advanced by main

    pthread_mutex_t mu;

    // thread-private (only touched by background thread)
    int   fd;
    void *ssl_ctx;
    void *ssl;
    unsigned char frame_buf[512 * 1024]; // 512KB — must fit largest WS frame
    int   frame_len;
} ACWs;

ACWs *ws_create(void);
void  ws_destroy(ACWs *ws);

// Non-blocking: schedules a connect on the background thread
void  ws_connect(ACWs *ws, const char *url);

// Non-blocking send (queues one message; thread drains it)
void  ws_send(ACWs *ws, const char *text);

// Call each frame from main thread.
// Swaps the inbound message buffer; returns number of new messages.
// Messages are in ws->messages[0..return_value-1].
int   ws_poll(ACWs *ws);

// Close and reset (non-blocking; signals thread to stop)
void  ws_close(ACWs *ws);
