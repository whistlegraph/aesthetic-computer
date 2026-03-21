#ifndef AC_WIFI_H
#define AC_WIFI_H

#include <stdint.h>
#include <sys/types.h>
#include <pthread.h>

#define WIFI_MAX_NETWORKS 32
#define WIFI_SSID_MAX 33      // 32 chars + null
#define WIFI_IFACE_MAX 32
#define WIFI_PASS_MAX 128

typedef enum {
    WIFI_STATE_OFF = 0,
    WIFI_STATE_SCANNING,
    WIFI_STATE_SCAN_DONE,
    WIFI_STATE_CONNECTING,
    WIFI_STATE_CONNECTED,
    WIFI_STATE_FAILED,
} WiFiState;

typedef enum {
    WIFI_CMD_NONE = 0,
    WIFI_CMD_SCAN,
    WIFI_CMD_CONNECT,
    WIFI_CMD_DISCONNECT,
    WIFI_CMD_AUTOCONNECT,
} WiFiCommand;

typedef struct {
    char ssid[WIFI_SSID_MAX];
    int signal;               // dBm (e.g. -45)
    int encrypted;            // 1 if WPA/WPA2, 0 if open
    char bssid[18];           // "AA:BB:CC:DD:EE:FF"
} WiFiNetwork;

typedef struct {
    // --- Main-thread readable state (protected by lock) ---
    WiFiState state;
    WiFiNetwork networks[WIFI_MAX_NETWORKS];
    int network_count;
    char connected_ssid[WIFI_SSID_MAX];
    char ip_address[16];      // "192.168.1.X"
    int signal_strength;      // current signal dBm
    char status_msg[64];      // human-readable status
    char iface[WIFI_IFACE_MAX]; // detected wireless interface name

    // --- Internal (thread-owned) ---
    pid_t wpa_pid;            // wpa_supplicant process
    pid_t dhcp_pid;           // dhclient process

    // Auto-reconnect state
    char last_ssid[WIFI_SSID_MAX];   // last successfully connected SSID
    char last_pass[WIFI_PASS_MAX];   // last password used
    int reconnect_failures;          // consecutive reconnect failures

    // --- Threading ---
    pthread_t thread;
    pthread_mutex_t lock;
    pthread_cond_t cond;      // signaled when a new command arrives
    volatile int thread_running;

    // Command queue (single slot — latest command wins)
    volatile WiFiCommand pending_cmd;
    char cmd_ssid[WIFI_SSID_MAX];
    char cmd_pass[WIFI_PASS_MAX];

    // Log ring buffer — readable from JS for diagnostics
    char log[32][128];           // last 32 log lines
    volatile int log_count;      // total lines written (modulo 32 for ring index)
} ACWifi;

// Initialize WiFi subsystem (bring up interface, start worker thread)
ACWifi *wifi_init(void);

// Post async scan command (non-blocking, runs on wifi thread)
void wifi_scan(ACWifi *wifi);

// Post async connect command (non-blocking, runs on wifi thread)
void wifi_connect(ACWifi *wifi, const char *ssid, const char *password);

// Post async disconnect command (non-blocking, runs on wifi thread)
void wifi_disconnect(ACWifi *wifi);

// These are now no-ops — polling happens on the wifi thread internally.
// Kept for API compatibility; main thread just reads wifi->state.
int wifi_scan_poll(ACWifi *wifi);
int wifi_connect_poll(ACWifi *wifi);

// Auto-connect on boot: scan, match against saved creds, connect to best.
// Non-blocking — posts scan+connect commands to the wifi thread.
void wifi_autoconnect(ACWifi *wifi);

// Stop thread and free
void wifi_destroy(ACWifi *wifi);

#endif
