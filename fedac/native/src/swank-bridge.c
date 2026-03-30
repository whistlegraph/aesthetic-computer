// swank-bridge.c — Evaluate CL expressions via local Swank server
//
// Swank wire protocol:
//   Send: 6-digit hex length + S-expression
//   Recv: 6-digit hex length + S-expression
//
// We send:  (:emacs-rex (swank:interactive-eval "EXPR") "CL-USER" :repl-thread 1)
// We get:   (:return (:ok "RESULT") 1)

#include "swank-bridge.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/select.h>

#define SWANK_PORT 4005
#define SWANK_TIMEOUT_MS 10000

static int swank_connect(void) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(SWANK_PORT),
        .sin_addr.s_addr = htonl(INADDR_LOOPBACK),
    };

    // Non-blocking connect with timeout
    fcntl(fd, F_SETFL, O_NONBLOCK);
    connect(fd, (struct sockaddr *)&addr, sizeof(addr));

    fd_set wfds;
    struct timeval tv = { 2, 0 }; // 2s connect timeout
    FD_ZERO(&wfds);
    FD_SET(fd, &wfds);
    if (select(fd + 1, NULL, &wfds, NULL, &tv) <= 0) {
        close(fd);
        return -1;
    }

    // Check for connect error
    int err = 0;
    socklen_t len = sizeof(err);
    getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &len);
    if (err) { close(fd); return -1; }

    fcntl(fd, F_SETFL, 0); // restore blocking
    return fd;
}

static int swank_send(int fd, const char *sexp) {
    int slen = strlen(sexp);
    char header[7];
    snprintf(header, sizeof(header), "%06x", slen);
    if (write(fd, header, 6) != 6) return -1;
    if (write(fd, sexp, slen) != slen) return -1;
    return 0;
}

static int swank_recv(int fd, char *buf, int buf_len, int timeout_ms) {
    // Read 6-byte hex length header
    char header[7] = {0};
    int total = 0;
    fd_set rfds;
    struct timeval tv;

    while (total < 6) {
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        tv.tv_sec = timeout_ms / 1000;
        tv.tv_usec = (timeout_ms % 1000) * 1000;
        if (select(fd + 1, &rfds, NULL, NULL, &tv) <= 0) return -1;
        int n = read(fd, header + total, 6 - total);
        if (n <= 0) return -1;
        total += n;
    }

    int plen = (int)strtol(header, NULL, 16);
    if (plen <= 0 || plen >= buf_len) return -1;

    // Read payload
    total = 0;
    while (total < plen) {
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        tv.tv_sec = timeout_ms / 1000;
        tv.tv_usec = (timeout_ms % 1000) * 1000;
        if (select(fd + 1, &rfds, NULL, NULL, &tv) <= 0) return -1;
        int n = read(fd, buf + total, plen - total);
        if (n <= 0) return -1;
        total += n;
    }
    buf[plen] = 0;
    return plen;
}

// Extract the result string from (:return (:ok "RESULT") N)
static void extract_result(const char *response, char *result, int result_len) {
    // Look for (:ok ".....")
    const char *ok = strstr(response, "(:ok ");
    if (!ok) {
        // Look for (:abort ".....")
        const char *ab = strstr(response, "(:abort ");
        if (ab) {
            const char *q1 = strchr(ab + 8, '"');
            if (q1) {
                const char *q2 = strrchr(q1 + 1, '"');
                if (q2) {
                    int len = (int)(q2 - q1 - 1);
                    if (len >= result_len) len = result_len - 1;
                    memcpy(result, q1 + 1, len);
                    result[len] = 0;
                    return;
                }
            }
        }
        snprintf(result, result_len, "error: %s", response);
        return;
    }

    const char *q1 = strchr(ok + 5, '"');
    if (!q1) {
        // No quoted result — might be NIL or a non-string value
        snprintf(result, result_len, "%.200s", ok + 5);
        // Trim trailing )
        char *rp = result + strlen(result) - 1;
        while (rp > result && (*rp == ')' || *rp == ' ')) *rp-- = 0;
        return;
    }

    // Find matching closing quote (handle escaped quotes)
    const char *p = q1 + 1;
    char *dst = result;
    int remaining = result_len - 1;
    while (*p && *p != '"' && remaining > 0) {
        if (*p == '\\' && *(p + 1)) {
            p++; // skip escape
            *dst++ = *p++;
            remaining--;
        } else {
            *dst++ = *p++;
            remaining--;
        }
    }
    *dst = 0;
}

int swank_eval(const char *expr, char *result, int result_len) {
    result[0] = 0;

    int fd = swank_connect();
    if (fd < 0) {
        snprintf(result, result_len, "error: cannot connect to Swank (port %d)", SWANK_PORT);
        return -1;
    }

    // Consume the initial Swank greeting/info messages
    char greeting[4096];
    for (int i = 0; i < 5; i++) {
        int n = swank_recv(fd, greeting, sizeof(greeting), 500);
        if (n <= 0) break;
        // Look for :indentation-update which signals ready
        if (strstr(greeting, ":indentation-update")) break;
    }

    // Build eval request — escape quotes in expr
    char escaped[2048];
    int ei = 0;
    for (int i = 0; expr[i] && ei < 2040; i++) {
        if (expr[i] == '"' || expr[i] == '\\') escaped[ei++] = '\\';
        escaped[ei++] = expr[i];
    }
    escaped[ei] = 0;

    char sexp[4096];
    snprintf(sexp, sizeof(sexp),
        "(:emacs-rex (swank:interactive-eval \"%s\") \"CL-USER\" :repl-thread 1)",
        escaped);

    if (swank_send(fd, sexp) < 0) {
        close(fd);
        snprintf(result, result_len, "error: send failed");
        return -1;
    }

    // Read response(s) — skip :write-string, wait for :return
    char response[8192];
    for (int attempt = 0; attempt < 10; attempt++) {
        int n = swank_recv(fd, response, sizeof(response), SWANK_TIMEOUT_MS);
        if (n <= 0) break;
        if (strstr(response, ":return")) {
            extract_result(response, result, result_len);
            close(fd);
            return 0;
        }
        // :write-string contains stdout output — append to result
        if (strstr(response, ":write-string")) {
            const char *q1 = strchr(response + 14, '"');
            if (q1) {
                const char *q2 = strrchr(q1 + 1, '"');
                if (q2) {
                    int cur = strlen(result);
                    int len = (int)(q2 - q1 - 1);
                    if (cur + len < result_len - 1) {
                        memcpy(result + cur, q1 + 1, len);
                        result[cur + len] = 0;
                    }
                }
            }
        }
    }

    close(fd);
    if (!result[0]) snprintf(result, result_len, "error: no response from Swank");
    return -1;
}

int swank_available(void) {
    int fd = swank_connect();
    if (fd < 0) return 0;
    close(fd);
    return 1;
}
