#ifndef AC_WIFI_H
#define AC_WIFI_H

#include <stdint.h>
#include <sys/types.h>

#define WIFI_MAX_NETWORKS 32
#define WIFI_SSID_MAX 33      // 32 chars + null
#define WIFI_IFACE "wlan0"

typedef enum {
    WIFI_STATE_OFF = 0,
    WIFI_STATE_SCANNING,
    WIFI_STATE_SCAN_DONE,
    WIFI_STATE_CONNECTING,
    WIFI_STATE_CONNECTED,
    WIFI_STATE_FAILED,
} WiFiState;

typedef struct {
    char ssid[WIFI_SSID_MAX];
    int signal;               // dBm (e.g. -45)
    int encrypted;            // 1 if WPA/WPA2, 0 if open
    char bssid[18];           // "AA:BB:CC:DD:EE:FF"
} WiFiNetwork;

typedef struct {
    WiFiState state;
    WiFiNetwork networks[WIFI_MAX_NETWORKS];
    int network_count;
    char connected_ssid[WIFI_SSID_MAX];
    char ip_address[16];      // "192.168.1.X"
    int signal_strength;      // current signal dBm
    char status_msg[64];      // human-readable status
    pid_t wpa_pid;            // wpa_supplicant process
    pid_t dhcp_pid;           // dhclient process
} ACWifi;

// Initialize WiFi subsystem (bring up interface)
ACWifi *wifi_init(void);

// Start async network scan
void wifi_scan(ACWifi *wifi);

// Check if scan is complete (non-blocking)
// Returns 1 if results are ready
int wifi_scan_poll(ACWifi *wifi);

// Connect to a network (starts wpa_supplicant + dhclient)
void wifi_connect(ACWifi *wifi, const char *ssid, const char *password);

// Check connection status (non-blocking)
int wifi_connect_poll(ACWifi *wifi);

// Disconnect and cleanup
void wifi_disconnect(ACWifi *wifi);

// Destroy WiFi subsystem
void wifi_destroy(ACWifi *wifi);

#endif
