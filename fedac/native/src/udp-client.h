#pragma once

// Raw UDP client for fairy point co-presence (ac-native bare metal)
// Background thread sends/recvs small packets to session server.

#include <pthread.h>
#include <netinet/in.h>

#define UDP_MAX_FAIRIES   16
#define UDP_FAIRY_PORT    10010

typedef struct {
    float x;   // 0.0-1.0 normalized
    float y;
} UDPFairy;

typedef struct {
    // Background recv thread
    pthread_t      thread;
    int            thread_running;
    int            sock;  // UDP socket fd (-1 = not connected)

    // Server address
    struct sockaddr_in server_addr;
    int            connected;  // 1 = resolved + socket open

    // Outgoing: main thread writes, thread sends
    float          send_x, send_y;
    int            send_pending;

    // Incoming: thread writes, main thread reads during paint
    UDPFairy       fairies[UDP_MAX_FAIRIES];
    int            fairy_count;

    // Identity
    char           handle[64];

    pthread_mutex_t mu;
} ACUdp;

ACUdp *udp_create(void);
void   udp_destroy(ACUdp *udp);

// Connect to session server's raw UDP relay
void   udp_connect(ACUdp *udp, const char *host, int port);

// Queue a fairy point to send (non-blocking, main thread)
void   udp_send_fairy(ACUdp *udp, float x, float y);

// Poll received fairies (returns count, fills out[] up to max)
// Clears the buffer after reading.
int    udp_poll_fairies(ACUdp *udp, UDPFairy *out, int max);
