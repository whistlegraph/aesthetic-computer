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
#include <sys/select.h>
#include <arpa/inet.h>
#include <stdint.h>

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

// ---------------------------------------------------------------------------
// Base64 (for WS handshake key)
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

// ---------------------------------------------------------------------------
// Thread-private helpers (called only from background thread)
// ---------------------------------------------------------------------------

static void thread_close_ssl(ACWs *ws) {
    if (ws->ssl) {
        SSL_shutdown((SSL *)ws->ssl);
        SSL_free((SSL *)ws->ssl);
        ws->ssl = NULL;
    }
    if (ws->ssl_ctx) {
        SSL_CTX_free((SSL_CTX *)ws->ssl_ctx);
        ws->ssl_ctx = NULL;
    }
    if (ws->fd >= 0) {
        close(ws->fd);
        ws->fd = -1;
    }
}

// Returns 0 on success, -1 on failure.
static int thread_connect(ACWs *ws, const char *host, const char *path) {
    thread_close_ssl(ws);
    ws->frame_len = 0;

    pthread_mutex_lock(&ws->mu);
    ws->connected = 0;
    ws->connecting = 1;
    ws->error = 0;
    pthread_mutex_unlock(&ws->mu);

    // DNS
    struct addrinfo hints = {0}, *res = NULL;
    hints.ai_family   = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    if (getaddrinfo(host, "443", &hints, &res) != 0 || !res) {
        ac_log("[ws] DNS failed for %s", host);
        goto fail;
    }

    // TCP connect (with 8s timeout via non-blocking + select)
    ws->fd = socket(AF_INET, SOCK_STREAM, 0);
    if (ws->fd < 0) { freeaddrinfo(res); goto fail; }
    fcntl(ws->fd, F_SETFL, O_NONBLOCK);
    connect(ws->fd, res->ai_addr, res->ai_addrlen);
    freeaddrinfo(res);
    {
        fd_set wfds; struct timeval tv = {8, 0};
        FD_ZERO(&wfds); FD_SET(ws->fd, &wfds);
        if (select(ws->fd + 1, NULL, &wfds, NULL, &tv) <= 0) {
            ac_log("[ws] TCP connect timeout to %s", host);
            goto fail;
        }
    }
    fcntl(ws->fd, F_SETFL, 0);  // restore blocking for TLS

    // TLS
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) goto fail;
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
    ws->ssl_ctx = ctx;

    SSL *ssl = SSL_new(ctx);
    SSL_set_fd(ssl, ws->fd);
    SSL_set_tlsext_host_name(ssl, host);
    ws->ssl = ssl;

    if (SSL_connect(ssl) != 1) {
        ac_log("[ws] TLS handshake failed for %s", host);
        goto fail;
    }

    // WebSocket HTTP upgrade
    unsigned char key_raw[16];
    RAND_bytes(key_raw, 16);
    char key_b64[32];
    b64_enc(key_raw, 16, key_b64);

    char req[512];
    int rlen = snprintf(req, sizeof(req),
        "GET %s HTTP/1.1\r\nHost: %s\r\nUpgrade: websocket\r\n"
        "Connection: Upgrade\r\nSec-WebSocket-Key: %s\r\n"
        "Sec-WebSocket-Version: 13\r\n\r\n",
        path, host, key_b64);
    SSL_write(ssl, req, rlen);

    // Read until "\r\n\r\n"
    char resp[1024] = {0};
    int rpos = 0;
    while (rpos < (int)sizeof(resp) - 1) {
        char c;
        if (SSL_read(ssl, &c, 1) <= 0) break;
        resp[rpos++] = c;
        if (rpos >= 4 && memcmp(resp + rpos - 4, "\r\n\r\n", 4) == 0) break;
    }
    if (!strstr(resp, "101")) {
        ac_log("[ws] Upgrade failed: %.80s", resp);
        goto fail;
    }

    // Keep socket blocking but set a 50ms receive timeout so the background
    // thread doesn't block forever — switching to O_NONBLOCK after TLS 1.3
    // handshake confuses OpenSSL's internal state machine (SSL_ERROR_SSL).
    struct timeval so_tv = {0, 50000};
    setsockopt(ws->fd, SOL_SOCKET, SO_RCVTIMEO, &so_tv, sizeof(so_tv));

    pthread_mutex_lock(&ws->mu);
    ws->connected = 1;
    ws->connecting = 0;
    pthread_mutex_unlock(&ws->mu);
    ac_log("[ws] Connected to wss://%s%s", host, path);
    return 0;

fail:
    thread_close_ssl(ws);
    pthread_mutex_lock(&ws->mu);
    ws->connecting = 0;
    ws->error = 1;
    pthread_mutex_unlock(&ws->mu);
    return -1;
}

// Send a masked text frame (called from background thread only)
static void thread_send(ACWs *ws, const char *text) {
    SSL *ssl = (SSL *)ws->ssl;
    if (!ssl) return;
    size_t plen = strlen(text);
    unsigned char mask[4]; RAND_bytes(mask, 4);
    unsigned char hdr[10]; int hlen = 0;
    hdr[hlen++] = 0x81;
    if (plen < 126)       { hdr[hlen++] = 0x80 | (uint8_t)plen; }
    else                  { hdr[hlen++] = 0x80 | 126;
                            hdr[hlen++] = (plen>>8)&0xff;
                            hdr[hlen++] = plen&0xff; }
    memcpy(hdr+hlen, mask, 4); hlen += 4;
    unsigned char *masked = malloc(plen);
    if (!masked) return;
    for (size_t i = 0; i < plen; i++) masked[i] = (uint8_t)text[i] ^ mask[i&3];
    SSL_write(ssl, hdr, hlen);
    SSL_write(ssl, masked, (int)plen);
    free(masked);
}

// Recv one iteration — reads available bytes, decodes frames into ws->messages
static void thread_recv(ACWs *ws) {
    SSL *ssl = (SSL *)ws->ssl;
    if (!ssl) return;

    // Read whatever is available; on fatal error fall through to parse buffered data
    int ssl_fatal = 0;
    while (ws->frame_len < (int)sizeof(ws->frame_buf) - 1) {
        int n = SSL_read(ssl, ws->frame_buf + ws->frame_len,
                         (int)sizeof(ws->frame_buf) - 1 - ws->frame_len);
        if (n > 0) { ws->frame_len += n; }
        else {
            int e = SSL_get_error(ssl, n);
            if (e == SSL_ERROR_WANT_READ || e == SSL_ERROR_WANT_WRITE) break;
            // SO_RCVTIMEO timeout: EAGAIN/EWOULDBLOCK via SSL_ERROR_SYSCALL — not fatal
            if (e == SSL_ERROR_SYSCALL && (errno == EAGAIN || errno == EWOULDBLOCK || errno == 0)) break;
            ac_log("[ws] recv error %d errno=%d (buffered %d bytes)", e, errno, ws->frame_len);
            ssl_fatal = 1;
            break;
        }
    }

    // Parse frames
    unsigned char *buf = ws->frame_buf;
    int avail = ws->frame_len;
    while (avail >= 2) {
        int fin  = (buf[0]>>7)&1;
        int op   = buf[0]&0x0f;
        int has_mask = (buf[1]>>7)&1;
        uint64_t plen = buf[1]&0x7f;
        int hlen = 2;
        if (plen == 126) { if (avail<4) break; plen=((uint64_t)buf[2]<<8)|buf[3]; hlen=4; }
        else if (plen == 127) { if (avail<10) break; plen=0; for(int i=0;i<8;i++) plen=(plen<<8)|buf[2+i]; hlen=10; }
        if (has_mask) hlen += 4;
        int total = hlen + (int)plen;
        if (avail < total) break;

        unsigned char *payload = buf + hlen;
        if (has_mask) {
            unsigned char *mk = buf + hlen - 4;
            for (uint64_t i = 0; i < plen; i++) payload[i] ^= mk[i&3];
        }

        if ((op == 0x1 || op == 0x0) && fin) {
            int copy = (int)plen < WS_MAX_MSG_LEN-1 ? (int)plen : WS_MAX_MSG_LEN-1;
            ac_log("[ws] frame op=%d plen=%d first64=%.64s", op, (int)plen, (char*)payload);
            pthread_mutex_lock(&ws->mu);
            int slot = ws->msg_count % WS_MAX_MESSAGES;
            memcpy(ws->messages[slot], payload, copy);
            ws->messages[slot][copy] = 0;
            if (ws->msg_count < WS_MAX_MESSAGES) ws->msg_count++;
            pthread_mutex_unlock(&ws->mu);
        } else if (op == 0x9) {
            // Ping → pong
            unsigned char pong[2] = {0x8a, 0x00};
            SSL_write(ssl, pong, 2);
        } else if (op == 0x8) {
            ac_log("[ws] server close");
            thread_close_ssl(ws);
            pthread_mutex_lock(&ws->mu);
            ws->connected = 0;
            pthread_mutex_unlock(&ws->mu);
            return;
        }
        memmove(buf, buf+total, avail-total);
        avail -= total;
    }
    ws->frame_len = avail;

    // Now close if the SSL layer had a fatal error (after processing buffered frames)
    if (ssl_fatal) {
        thread_close_ssl(ws);
        pthread_mutex_lock(&ws->mu);
        ws->connected = 0;
        ws->error = 1;
        pthread_mutex_unlock(&ws->mu);
    }
}

// ---------------------------------------------------------------------------
// Background thread
// ---------------------------------------------------------------------------

static void *ws_thread(void *arg) {
    ACWs *ws = (ACWs *)arg;
    while (ws->thread_running) {
        // Check for pending connect request
        pthread_mutex_lock(&ws->mu);
        int do_connect = ws->pending_connect;
        char host[256], path[256];
        if (do_connect) {
            strncpy(host, ws->pending_host, 255); host[255]=0;
            strncpy(path, ws->pending_path, 255); path[255]=0;
            ws->pending_connect = 0;
        }
        pthread_mutex_unlock(&ws->mu);

        if (do_connect) thread_connect(ws, host, path);

        // Check for pending send
        pthread_mutex_lock(&ws->mu);
        int do_send = ws->send_pending && ws->connected;
        char sbuf[4096];
        if (do_send) { strncpy(sbuf, ws->send_buf, 4095); sbuf[4095]=0; ws->send_pending=0; }
        pthread_mutex_unlock(&ws->mu);
        if (do_send) thread_send(ws, sbuf);

        // Recv if connected
        pthread_mutex_lock(&ws->mu);
        int is_connected = ws->connected;
        pthread_mutex_unlock(&ws->mu);
        if (is_connected) thread_recv(ws);

        // When not connected, sleep 16ms to avoid spinning.
        // When connected, thread_recv() blocks up to 50ms in SSL_read (SO_RCVTIMEO).
        if (!is_connected) {
            struct timespec ts = {0, 16000000};
            nanosleep(&ts, NULL);
        }
    }
    thread_close_ssl(ws);
    return NULL;
}

// ---------------------------------------------------------------------------
// Public API (called from main thread)
// ---------------------------------------------------------------------------

ACWs *ws_create(void) {
    ACWs *ws = calloc(1, sizeof(ACWs));
    if (!ws) return NULL;
    ws->fd = -1;
    pthread_mutex_init(&ws->mu, NULL);
    ws->thread_running = 1;
    pthread_create(&ws->thread, NULL, ws_thread, ws);
    return ws;
}

void ws_destroy(ACWs *ws) {
    if (!ws) return;
    ws->thread_running = 0;
    pthread_join(ws->thread, NULL);
    pthread_mutex_destroy(&ws->mu);
    free(ws);
}

void ws_connect(ACWs *ws, const char *url) {
    if (!ws) return;
    const char *host_start = url;
    if (strncmp(url, "wss://", 6) == 0) host_start = url + 6;
    else if (strncmp(url, "ws://", 5) == 0) host_start = url + 5;

    char host[256] = {0};
    const char *slash = strchr(host_start, '/');
    const char *path = slash ? slash : "/";
    int hlen = slash ? (int)(slash - host_start) : (int)strlen(host_start);
    if (hlen >= 255) hlen = 255;
    memcpy(host, host_start, hlen);

    pthread_mutex_lock(&ws->mu);
    strncpy(ws->pending_host, host, 255);
    strncpy(ws->pending_path, path, 255);
    ws->pending_connect = 1;
    ws->connected = 0;
    ws->error = 0;
    pthread_mutex_unlock(&ws->mu);
}

void ws_send(ACWs *ws, const char *text) {
    if (!ws) return;
    pthread_mutex_lock(&ws->mu);
    if (ws->connected) {
        strncpy(ws->send_buf, text, 4095);
        ws->send_buf[4095] = 0;
        ws->send_pending = 1;
    }
    pthread_mutex_unlock(&ws->mu);
}

int ws_poll(ACWs *ws) {
    if (!ws) return 0;
    pthread_mutex_lock(&ws->mu);
    int count = ws->msg_count;
    ws->msg_count = 0;  // consumed
    pthread_mutex_unlock(&ws->mu);
    return count;
}

void ws_close(ACWs *ws) {
    if (!ws) return;
    pthread_mutex_lock(&ws->mu);
    ws->connected = 0;
    ws->pending_connect = 0;
    ws->send_pending = 0;
    pthread_mutex_unlock(&ws->mu);
    // thread will see connected=0 and stop recv; ssl closed on next iteration
}
