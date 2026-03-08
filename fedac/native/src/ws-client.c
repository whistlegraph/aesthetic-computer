#include "ws-client.h"

#include <stdio.h>
#include <stdarg.h>
extern void ac_log(const char *fmt, ...);
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static const char B64[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static void b64_enc(const unsigned char *in, int len, char *out) {
    int i, j = 0;
    for (i = 0; i <= len - 3; i += 3) {
        out[j++] = B64[in[i] >> 2];
        out[j++] = B64[((in[i] & 3) << 4) | (in[i+1] >> 4)];
        out[j++] = B64[((in[i+1] & 0xf) << 2) | (in[i+2] >> 6)];
        out[j++] = B64[in[i+2] & 0x3f];
    }
    if (i < len) {
        out[j++] = B64[in[i] >> 2];
        if (i+1 < len) {
            out[j++] = B64[((in[i] & 3) << 4) | (in[i+1] >> 4)];
            out[j++] = B64[(in[i+1] & 0xf) << 2];
        } else {
            out[j++] = B64[(in[i] & 3) << 4];
            out[j++] = '=';
        }
        out[j++] = '=';
    }
    out[j] = 0;
}

static int ssl_read_all(SSL *ssl, char *buf, int want) {
    int total = 0;
    while (total < want) {
        int n = SSL_read(ssl, buf + total, want - total);
        if (n <= 0) break;
        total += n;
    }
    return total;
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

ACWs *ws_create(void) {
    ACWs *ws = calloc(1, sizeof(ACWs));
    return ws;
}

void ws_destroy(ACWs *ws) {
    if (!ws) return;
    ws_close(ws);
    free(ws);
}

void ws_close(ACWs *ws) {
    if (!ws) return;
    if (ws->ssl) {
        SSL_shutdown((SSL *)ws->ssl);
        SSL_free((SSL *)ws->ssl);
        ws->ssl = NULL;
    }
    if (ws->ctx) {
        SSL_CTX_free((SSL_CTX *)ws->ctx);
        ws->ctx = NULL;
    }
    if (ws->fd >= 0) {
        close(ws->fd);
        ws->fd = -1;
    }
    ws->connected = 0;
    ws->frame_len = 0;
    ws->msg_count = 0;
}

// ---------------------------------------------------------------------------
// Connect
// ---------------------------------------------------------------------------

int ws_connect(ACWs *ws, const char *host, const char *path) {
    ws_close(ws);
    ws->error = 0;

    // --- DNS resolve ---
    struct addrinfo hints = {0}, *res = NULL;
    hints.ai_family   = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    if (getaddrinfo(host, "443", &hints, &res) != 0 || !res) {
        ac_log("[ws] DNS failed for %s", host);
        return -1;
    }

    // --- TCP connect ---
    ws->fd = socket(AF_INET, SOCK_STREAM, 0);
    if (ws->fd < 0) { freeaddrinfo(res); return -1; }

    // 5-second connect timeout via O_NONBLOCK + select
    fcntl(ws->fd, F_SETFL, O_NONBLOCK);
    connect(ws->fd, res->ai_addr, res->ai_addrlen);
    freeaddrinfo(res);

    fd_set wfds; struct timeval tv = {5, 0};
    FD_ZERO(&wfds); FD_SET(ws->fd, &wfds);
    if (select(ws->fd + 1, NULL, &wfds, NULL, &tv) <= 0) {
        ac_log("[ws] TCP connect timeout to %s", host);
        close(ws->fd); ws->fd = -1;
        return -1;
    }
    // Restore blocking
    fcntl(ws->fd, F_SETFL, 0);

    // --- TLS ---
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) { close(ws->fd); ws->fd = -1; return -1; }
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);  // bare metal, no CA bundle
    ws->ctx = ctx;

    SSL *ssl = SSL_new(ctx);
    SSL_set_fd(ssl, ws->fd);
    SSL_set_tlsext_host_name(ssl, host);
    ws->ssl = ssl;

    if (SSL_connect(ssl) != 1) {
        ac_log("[ws] TLS handshake failed for %s", host);
        ws_close(ws);
        return -1;
    }

    // --- WebSocket HTTP upgrade ---
    unsigned char key_raw[16];
    RAND_bytes(key_raw, sizeof(key_raw));
    char key_b64[32];
    b64_enc(key_raw, 16, key_b64);

    char req[512];
    int rlen = snprintf(req, sizeof(req),
        "GET %s HTTP/1.1\r\n"
        "Host: %s\r\n"
        "Upgrade: websocket\r\n"
        "Connection: Upgrade\r\n"
        "Sec-WebSocket-Key: %s\r\n"
        "Sec-WebSocket-Version: 13\r\n"
        "\r\n",
        path, host, key_b64);
    SSL_write(ssl, req, rlen);

    // Read HTTP response (look for "\r\n\r\n")
    char resp[1024] = {0};
    int rpos = 0;
    while (rpos < (int)sizeof(resp) - 1) {
        char c;
        int n = SSL_read(ssl, &c, 1);
        if (n <= 0) break;
        resp[rpos++] = c;
        if (rpos >= 4 && memcmp(resp + rpos - 4, "\r\n\r\n", 4) == 0) break;
    }

    if (!strstr(resp, "101")) {
        ac_log("[ws] WebSocket upgrade failed: %.80s", resp);
        ws_close(ws);
        return -1;
    }

    // Set non-blocking for poll loop
    fcntl(ws->fd, F_SETFL, O_NONBLOCK);
    SSL_set_mode(ssl, SSL_MODE_ENABLE_PARTIAL_WRITE | SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER);

    ws->connected = 1;
    ac_log("[ws] Connected to wss://%s%s", host, path);
    return 0;
}

// ---------------------------------------------------------------------------
// Send text frame (client → server, masked)
// ---------------------------------------------------------------------------

int ws_send(ACWs *ws, const char *text) {
    if (!ws || !ws->connected || !ws->ssl) return -1;
    size_t plen = strlen(text);

    unsigned char mask[4];
    RAND_bytes(mask, 4);

    unsigned char hdr[10];
    int hlen = 0;
    hdr[hlen++] = 0x81;  // FIN + text opcode
    if (plen < 126) {
        hdr[hlen++] = 0x80 | (unsigned char)plen;
    } else if (plen < 65536) {
        hdr[hlen++] = 0x80 | 126;
        hdr[hlen++] = (plen >> 8) & 0xff;
        hdr[hlen++] = plen & 0xff;
    } else {
        return -1;  // too large
    }
    memcpy(hdr + hlen, mask, 4); hlen += 4;

    // Build masked payload
    unsigned char *masked = malloc(plen);
    if (!masked) return -1;
    for (size_t i = 0; i < plen; i++)
        masked[i] = (unsigned char)text[i] ^ mask[i & 3];

    SSL_write((SSL *)ws->ssl, hdr, hlen);
    SSL_write((SSL *)ws->ssl, masked, (int)plen);
    free(masked);
    return 0;
}

// ---------------------------------------------------------------------------
// Poll — non-blocking; appends decoded text frames to ws->messages
// ---------------------------------------------------------------------------

void ws_poll(ACWs *ws) {
    if (!ws || !ws->connected || !ws->ssl) return;
    ws->msg_count = 0;

    SSL *ssl = (SSL *)ws->ssl;

    // Read as many bytes as available into frame_buf
    while (ws->frame_len < (int)sizeof(ws->frame_buf) - 1) {
        int n = SSL_read(ssl, ws->frame_buf + ws->frame_len,
                         sizeof(ws->frame_buf) - 1 - ws->frame_len);
        if (n > 0) {
            ws->frame_len += n;
        } else {
            int err = SSL_get_error(ssl, n);
            if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) break;
            // Connection closed or error
            ac_log("[ws] recv error %d — disconnecting", err);
            ws_close(ws);
            return;
        }
    }

    // Parse complete frames from frame_buf
    unsigned char *buf = ws->frame_buf;
    int avail = ws->frame_len;

    while (avail >= 2) {
        int fin  = (buf[0] >> 7) & 1;
        int op   = buf[0] & 0x0f;
        int mask = (buf[1] >> 7) & 1;
        uint64_t plen = buf[1] & 0x7f;
        int hlen = 2;

        if (plen == 126) {
            if (avail < 4) break;
            plen = ((uint64_t)buf[2] << 8) | buf[3];
            hlen = 4;
        } else if (plen == 127) {
            if (avail < 10) break;
            plen = 0;
            for (int i = 0; i < 8; i++) plen = (plen << 8) | buf[2 + i];
            hlen = 10;
        }
        if (mask) hlen += 4;

        int frame_total = hlen + (int)plen;
        if (avail < frame_total) break;  // wait for more data

        if (op == 0x1 /* text */ || op == 0x0 /* continuation */) {
            unsigned char *payload = buf + hlen;
            if (mask) {
                unsigned char *mk = buf + hlen - 4;
                for (uint64_t i = 0; i < plen; i++)
                    payload[i] ^= mk[i & 3];
            }
            if (fin && ws->msg_count < WS_MAX_MESSAGES) {
                int copy = (int)plen < WS_MAX_MSG_LEN - 1 ? (int)plen : WS_MAX_MSG_LEN - 1;
                memcpy(ws->messages[ws->msg_count], payload, copy);
                ws->messages[ws->msg_count][copy] = 0;
                ws->msg_count++;
            }
        } else if (op == 0x9 /* ping */) {
            // Send pong
            unsigned char pong[2] = { 0x8a, 0x00 };
            SSL_write(ssl, pong, 2);
        } else if (op == 0x8 /* close */) {
            ac_log("[ws] Server sent close frame");
            ws_close(ws);
            return;
        }

        // Consume this frame
        memmove(buf, buf + frame_total, avail - frame_total);
        avail -= frame_total;
    }
    ws->frame_len = avail;
}
