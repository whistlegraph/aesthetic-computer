#pragma once

// Minimal TLS WebSocket client for ac-native bare metal
// Uses OpenSSL for TLS; non-blocking recv; text frames only.

#define WS_MAX_MESSAGES  16
#define WS_MAX_MSG_LEN   4096

typedef struct {
    int fd;
    void *ctx;   // SSL_CTX*
    void *ssl;   // SSL*
    int connected;
    int error;   // set on fatal error, cleared on reconnect

    // Inbound message queue (filled by ws_poll, consumed by JS each frame)
    char messages[WS_MAX_MESSAGES][WS_MAX_MSG_LEN];
    int  msg_count;

    // Partial frame reassembly buffer
    unsigned char frame_buf[65536];
    int frame_len;
} ACWs;

ACWs *ws_create(void);
void  ws_destroy(ACWs *ws);

// Connect to wss://host/path  (port 443)
int   ws_connect(ACWs *ws, const char *host, const char *path);

// Send a UTF-8 text frame (returns 0 on success)
int   ws_send(ACWs *ws, const char *text);

// Poll for incoming frames; fills ws->messages[0..msg_count-1]
// Call every frame while connected.
void  ws_poll(ACWs *ws);

// Close cleanly
void  ws_close(ACWs *ws);
