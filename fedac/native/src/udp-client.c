#include "udp-client.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>

extern void ac_log(const char *fmt, ...);

// Packet format (binary, little-endian):
//   [1 byte type] [payload...]
// Type 0x01 = fairy point:
//   [1] [4 float x] [4 float y] [1 handle_len] [N handle_bytes]
// Type 0x02 = fairy broadcast (from server):
//   [1] [4 float x] [4 float y] [1 handle_len] [N handle_bytes]

#define PKT_FAIRY_SEND  0x01
#define PKT_FAIRY_RECV  0x02

static uint64_t udp_now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (uint64_t)ts.tv_sec * 1000ULL + (uint64_t)ts.tv_nsec / 1000000ULL;
}

static void json_escape_copy(char *dst, size_t dst_size, const char *src) {
    size_t out = 0;

    if (!dst || dst_size == 0) return;
    if (!src) src = "";

    while (*src && out + 1 < dst_size) {
        unsigned char c = (unsigned char)*src++;
        if (c == '"' || c == '\\') {
            if (out + 2 >= dst_size) break;
            dst[out++] = '\\';
            dst[out++] = (char)c;
        } else if (c >= 32 && c < 127) {
            dst[out++] = (char)c;
        }
    }

    dst[out] = '\0';
}

static int udp_snapshot_identity(
    ACUdp *udp,
    int *sock_out,
    struct sockaddr_in *addr_out,
    char *handle_out,
    size_t handle_out_size,
    char *machine_out,
    size_t machine_out_size
) {
    int sock = -1;
    int connected = 0;

    if (!udp) return 0;

    pthread_mutex_lock(&udp->mu);
    sock = udp->sock;
    connected = udp->connected;
    if (addr_out) *addr_out = udp->server_addr;
    if (handle_out && handle_out_size > 0) {
        strncpy(handle_out, udp->handle, handle_out_size - 1);
        handle_out[handle_out_size - 1] = 0;
    }
    if (machine_out && machine_out_size > 0) {
        strncpy(machine_out, udp->machine_id, machine_out_size - 1);
        machine_out[machine_out_size - 1] = 0;
    }
    pthread_mutex_unlock(&udp->mu);

    if (sock_out) *sock_out = sock;
    return connected && sock >= 0;
}

static void udp_send_json(ACUdp *udp, const char *json) {
    int sock = -1;
    struct sockaddr_in server_addr;

    if (!udp || !json || !json[0]) return;
    if (!udp_snapshot_identity(udp, &sock, &server_addr, NULL, 0, NULL, 0)) return;

    ssize_t sent = sendto(
        sock,
        json,
        strlen(json),
        0,
        (struct sockaddr *)&server_addr,
        sizeof(server_addr)
    );

    if (sent < 0 && errno != EAGAIN && errno != EWOULDBLOCK) {
        ac_log("[udp] json send failed: %s\n", strerror(errno));
    }
}

static void *udp_thread(void *arg) {
    ACUdp *udp = (ACUdp *)arg;
    unsigned char buf[256];

    while (udp->thread_running) {
        if (udp->sock < 0) {
            usleep(100000); // 100ms
            continue;
        }

        // Send pending fairy point
        pthread_mutex_lock(&udp->mu);
        int do_send = udp->send_pending;
        float sx = udp->send_x, sy = udp->send_y;
        char handle[64];
        strncpy(handle, udp->handle, sizeof(handle) - 1);
        handle[sizeof(handle) - 1] = 0;
        udp->send_pending = 0;
        pthread_mutex_unlock(&udp->mu);

        if (do_send && udp->connected) {
            int hlen = (int)strlen(handle);
            unsigned char pkt[128];
            pkt[0] = PKT_FAIRY_SEND;
            memcpy(pkt + 1, &sx, 4);
            memcpy(pkt + 5, &sy, 4);
            pkt[9] = (unsigned char)hlen;
            memcpy(pkt + 10, handle, hlen);
            int pkt_len = 10 + hlen;
            sendto(udp->sock, pkt, pkt_len, 0,
                   (struct sockaddr *)&udp->server_addr,
                   sizeof(udp->server_addr));
        }

        // Recv with short timeout
        struct pollfd pfd = { .fd = udp->sock, .events = POLLIN };
        int ret = poll(&pfd, 1, 16); // 16ms = ~60Hz
        if (ret > 0 && (pfd.revents & POLLIN)) {
            ssize_t n = recvfrom(udp->sock, buf, sizeof(buf), 0, NULL, NULL);
            if (n > 0 && buf[0] == PKT_FAIRY_RECV && n >= 10) {
                float fx, fy;
                memcpy(&fx, buf + 1, 4);
                memcpy(&fy, buf + 5, 4);

                pthread_mutex_lock(&udp->mu);
                if (udp->fairy_count < UDP_MAX_FAIRIES) {
                    udp->fairies[udp->fairy_count].x = fx;
                    udp->fairies[udp->fairy_count].y = fy;
                    udp->fairy_count++;
                }
                pthread_mutex_unlock(&udp->mu);
            }
        }
    }
    return NULL;
}

ACUdp *udp_create(void) {
    ACUdp *udp = calloc(1, sizeof(ACUdp));
    udp->sock = -1;
    pthread_mutex_init(&udp->mu, NULL);
    udp->thread_running = 1;
    pthread_create(&udp->thread, NULL, udp_thread, udp);
    return udp;
}

void udp_destroy(ACUdp *udp) {
    if (!udp) return;
    udp->thread_running = 0;
    pthread_join(udp->thread, NULL);
    if (udp->sock >= 0) close(udp->sock);
    pthread_mutex_destroy(&udp->mu);
    free(udp);
}

void udp_connect(ACUdp *udp, const char *host, int port) {
    if (!udp) return;

    // Resolve hostname
    struct hostent *he = gethostbyname(host);
    if (!he) {
        ac_log("[udp] DNS resolve failed for %s\n", host);
        return;
    }

    memset(&udp->server_addr, 0, sizeof(udp->server_addr));
    udp->server_addr.sin_family = AF_INET;
    udp->server_addr.sin_port = htons(port);
    memcpy(&udp->server_addr.sin_addr, he->h_addr_list[0], he->h_length);

    // Create socket
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        ac_log("[udp] socket() failed: %s\n", strerror(errno));
        return;
    }

    // Non-blocking
    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);

    pthread_mutex_lock(&udp->mu);
    udp->sock = fd;
    udp->connected = 1;
    pthread_mutex_unlock(&udp->mu);

    ac_log("[udp] connected to %s:%d\n", host, port);
}

void udp_send_fairy(ACUdp *udp, float x, float y) {
    if (!udp) return;
    pthread_mutex_lock(&udp->mu);
    udp->send_x = x;
    udp->send_y = y;
    udp->send_pending = 1;
    pthread_mutex_unlock(&udp->mu);
}

void udp_set_identity(ACUdp *udp, const char *handle, const char *machine_id) {
    if (!udp) return;
    pthread_mutex_lock(&udp->mu);
    if (handle) {
        strncpy(udp->handle, handle, sizeof(udp->handle) - 1);
        udp->handle[sizeof(udp->handle) - 1] = 0;
    }
    if (machine_id) {
        strncpy(udp->machine_id, machine_id, sizeof(udp->machine_id) - 1);
        udp->machine_id[sizeof(udp->machine_id) - 1] = 0;
    }
    pthread_mutex_unlock(&udp->mu);
}

void udp_send_midi(ACUdp *udp, const char *event, int note, int velocity, int channel, const char *piece) {
    char handle[64] = "";
    char machine_id[64] = "";
    char handle_json[128];
    char machine_json[128];
    char piece_json[64];
    char event_json[32];
    char json[512];

    if (!udp || !event || !piece) return;
    if (!udp_snapshot_identity(udp, NULL, NULL, handle, sizeof(handle), machine_id, sizeof(machine_id))) return;

    json_escape_copy(handle_json, sizeof(handle_json), handle);
    json_escape_copy(machine_json, sizeof(machine_json), machine_id[0] ? machine_id : "unknown");
    json_escape_copy(piece_json, sizeof(piece_json), piece);
    json_escape_copy(event_json, sizeof(event_json), event);

    snprintf(
        json,
        sizeof(json),
        "{\"type\":\"notepat:midi\",\"event\":\"%s\",\"note\":%d,\"velocity\":%d,"
        "\"channel\":%d,\"handle\":\"%s\",\"machineId\":\"%s\",\"piece\":\"%s\",\"ts\":%llu}",
        event_json,
        note,
        velocity,
        channel,
        handle_json,
        machine_json,
        piece_json,
        (unsigned long long)udp_now_ms()
    );

    udp_send_json(udp, json);
}

void udp_send_midi_heartbeat(ACUdp *udp, const char *piece) {
    char handle[64] = "";
    char machine_id[64] = "";
    char handle_json[128];
    char machine_json[128];
    char piece_json[64];
    char json[512];

    if (!udp || !piece) return;
    if (!udp_snapshot_identity(udp, NULL, NULL, handle, sizeof(handle), machine_id, sizeof(machine_id))) return;

    json_escape_copy(handle_json, sizeof(handle_json), handle);
    json_escape_copy(machine_json, sizeof(machine_json), machine_id[0] ? machine_id : "unknown");
    json_escape_copy(piece_json, sizeof(piece_json), piece);

    snprintf(
        json,
        sizeof(json),
        "{\"type\":\"notepat:midi:heartbeat\",\"handle\":\"%s\",\"machineId\":\"%s\","
        "\"piece\":\"%s\",\"broadcast\":true,\"ts\":%llu}",
        handle_json,
        machine_json,
        piece_json,
        (unsigned long long)udp_now_ms()
    );

    udp_send_json(udp, json);
}

int udp_poll_fairies(ACUdp *udp, UDPFairy *out, int max) {
    if (!udp) return 0;
    pthread_mutex_lock(&udp->mu);
    int count = udp->fairy_count;
    if (count > max) count = max;
    if (count > 0) {
        memcpy(out, udp->fairies, count * sizeof(UDPFairy));
        udp->fairy_count = 0;
    }
    pthread_mutex_unlock(&udp->mu);
    return count;
}
