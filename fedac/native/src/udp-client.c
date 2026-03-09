#include "udp-client.h"

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

extern void ac_log(const char *fmt, ...);

// Packet format (binary, little-endian):
//   [1 byte type] [payload...]
// Type 0x01 = fairy point:
//   [1] [4 float x] [4 float y] [1 handle_len] [N handle_bytes]
// Type 0x02 = fairy broadcast (from server):
//   [1] [4 float x] [4 float y] [1 handle_len] [N handle_bytes]

#define PKT_FAIRY_SEND  0x01
#define PKT_FAIRY_RECV  0x02

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
