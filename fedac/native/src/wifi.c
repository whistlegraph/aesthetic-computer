#include "wifi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>

// Defined in ac-native.c
extern void ac_log(const char *fmt, ...);

static int run_cmd(const char *cmd) {
    ac_log("[wifi] exec: %s", cmd);
    int r = system(cmd);
    if (r != 0) ac_log("[wifi] cmd failed (%d): %s", r, cmd);
    return r;
}

static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

// Detect the first wireless interface name
static int detect_iface(char *out, int out_len) {
    // Method 1: iw dev
    FILE *fp = popen("iw dev 2>/dev/null | grep Interface | head -1 | awk '{print $2}'", "r");
    if (fp) {
        char buf[32] = "";
        if (fgets(buf, sizeof(buf), fp)) {
            buf[strcspn(buf, "\n")] = 0;
            if (buf[0]) {
                snprintf(out, out_len, "%s", buf);
                pclose(fp);
                return 1;
            }
        }
        pclose(fp);
    }
    // Method 2: scan /sys/class/net/*/wireless
    fp = popen("ls -d /sys/class/net/*/wireless 2>/dev/null | head -1", "r");
    if (fp) {
        char buf[128] = "";
        if (fgets(buf, sizeof(buf), fp)) {
            buf[strcspn(buf, "\n")] = 0;
            // Extract interface name from /sys/class/net/IFACE/wireless
            char *start = strstr(buf, "/net/");
            if (start) {
                start += 5;
                char *end = strchr(start, '/');
                if (end) {
                    *end = 0;
                    snprintf(out, out_len, "%s", start);
                    pclose(fp);
                    return 1;
                }
            }
        }
        pclose(fp);
    }
    return 0;
}

ACWifi *wifi_init(void) {
    ACWifi *wifi = calloc(1, sizeof(ACWifi));
    if (!wifi) return NULL;

    wifi->state = WIFI_STATE_OFF;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "initializing...");
    wifi->iface[0] = 0;

    // Check if iw exists
    if (!file_exists("/usr/sbin/iw") && !file_exists("/sbin/iw")) {
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "iw not found");
        ac_log("[wifi] iw binary not found");
        return wifi;
    }

    // Detect wireless interface
    if (!detect_iface(wifi->iface, sizeof(wifi->iface))) {
        // Try wlan0 as fallback
        snprintf(wifi->iface, sizeof(wifi->iface), "wlan0");
    }
    ac_log("[wifi] Using interface: %s", wifi->iface);

    // Bring up the interface
    char cmd[128];
    snprintf(cmd, sizeof(cmd), "ip link set %s up 2>/dev/null", wifi->iface);
    run_cmd(cmd);

    // Verify interface exists
    snprintf(cmd, sizeof(cmd), "ip link show %s >/dev/null 2>&1", wifi->iface);
    if (system(cmd) != 0) {
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "no wifi interface");
        ac_log("[wifi] Interface %s not found", wifi->iface);
        return wifi;
    }

    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "ready");
    ac_log("[wifi] Interface %s is up", wifi->iface);
    return wifi;
}

void wifi_scan(ACWifi *wifi) {
    if (!wifi || !wifi->iface[0]) return;
    wifi->state = WIFI_STATE_SCANNING;
    wifi->network_count = 0;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "scanning...");

    char cmd[256];
    snprintf(cmd, sizeof(cmd), "iw dev %s scan 2>/dev/null > /tmp/wifi_scan.txt &", wifi->iface);
    run_cmd(cmd);
}

int wifi_scan_poll(ACWifi *wifi) {
    if (!wifi || wifi->state != WIFI_STATE_SCANNING) return 0;

    // Check if scan file exists and iw scan process is done
    if (!file_exists("/tmp/wifi_scan.txt")) return 0;

    // Check if iw is still running
    if (system("pgrep -x iw >/dev/null 2>&1") == 0) return 0;

    // Parse scan results
    FILE *fp = fopen("/tmp/wifi_scan.txt", "r");
    if (!fp) {
        wifi->state = WIFI_STATE_SCAN_DONE;
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "scan failed");
        return 1;
    }

    wifi->network_count = 0;
    char line[512];
    int cur = -1;

    while (fgets(line, sizeof(line), fp) && wifi->network_count < WIFI_MAX_NETWORKS) {
        // New BSS entry
        char bssid[18];
        if (sscanf(line, "BSS %17s", bssid) == 1) {
            cur = wifi->network_count++;
            memset(&wifi->networks[cur], 0, sizeof(WiFiNetwork));
            strncpy(wifi->networks[cur].bssid, bssid, 17);
            wifi->networks[cur].signal = -100;
        }
        if (cur < 0) continue;

        // Signal strength
        int sig;
        if (sscanf(line, "\tsignal: %d", &sig) == 1) {
            wifi->networks[cur].signal = sig;
        }

        // SSID
        char ssid[64];
        if (sscanf(line, "\tSSID: %63[^\n]", ssid) == 1) {
            strncpy(wifi->networks[cur].ssid, ssid, WIFI_SSID_MAX - 1);
        }

        // Encryption
        if (strstr(line, "WPA") || strstr(line, "RSN")) {
            wifi->networks[cur].encrypted = 1;
        }
    }
    fclose(fp);
    unlink("/tmp/wifi_scan.txt");

    // Sort by signal strength (strongest first)
    for (int i = 0; i < wifi->network_count - 1; i++) {
        for (int j = i + 1; j < wifi->network_count; j++) {
            if (wifi->networks[j].signal > wifi->networks[i].signal) {
                WiFiNetwork tmp = wifi->networks[i];
                wifi->networks[i] = wifi->networks[j];
                wifi->networks[j] = tmp;
            }
        }
    }

    // Remove entries with empty SSID
    int w = 0;
    for (int r = 0; r < wifi->network_count; r++) {
        if (wifi->networks[r].ssid[0]) {
            if (w != r) wifi->networks[w] = wifi->networks[r];
            w++;
        }
    }
    wifi->network_count = w;

    wifi->state = WIFI_STATE_SCAN_DONE;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg),
             "%d networks", wifi->network_count);
    ac_log("[wifi] Scan complete: %d networks", wifi->network_count);
    return 1;
}

void wifi_connect(ACWifi *wifi, const char *ssid, const char *password) {
    if (!wifi || !ssid || !wifi->iface[0]) return;

    wifi->state = WIFI_STATE_CONNECTING;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "connecting...");
    ac_log("[wifi] Connecting to '%s'", ssid);

    // Kill any existing wpa_supplicant
    if (wifi->wpa_pid > 0) {
        kill(wifi->wpa_pid, SIGTERM);
        waitpid(wifi->wpa_pid, NULL, WNOHANG);
        wifi->wpa_pid = 0;
    }
    if (wifi->dhcp_pid > 0) {
        kill(wifi->dhcp_pid, SIGTERM);
        waitpid(wifi->dhcp_pid, NULL, WNOHANG);
        wifi->dhcp_pid = 0;
    }
    run_cmd("killall wpa_supplicant 2>/dev/null; killall dhclient 2>/dev/null");

    // Write wpa_supplicant config
    FILE *fp = fopen("/tmp/wpa.conf", "w");
    if (!fp) {
        wifi->state = WIFI_STATE_FAILED;
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "config error");
        return;
    }

    fprintf(fp, "ctrl_interface=/var/run/wpa_supplicant\n");
    fprintf(fp, "update_config=1\n\n");
    fprintf(fp, "network={\n");
    fprintf(fp, "    ssid=\"%s\"\n", ssid);
    if (password && password[0]) {
        fprintf(fp, "    psk=\"%s\"\n", password);
    } else {
        fprintf(fp, "    key_mgmt=NONE\n");
    }
    fprintf(fp, "}\n");
    fclose(fp);

    // Create required directories
    mkdir("/var", 0755);
    mkdir("/var/run", 0755);
    mkdir("/var/run/wpa_supplicant", 0755);

    // Start wpa_supplicant
    pid_t pid = fork();
    if (pid == 0) {
        execl("/usr/sbin/wpa_supplicant", "wpa_supplicant",
              "-i", wifi->iface, "-c", "/tmp/wpa.conf",
              "-B", "-P", "/tmp/wpa.pid", NULL);
        _exit(1);
    }
    wifi->wpa_pid = pid;
    waitpid(pid, NULL, 0); // Wait for wpa_supplicant to daemonize (-B)

    strncpy(wifi->connected_ssid, ssid, WIFI_SSID_MAX - 1);
}

int wifi_connect_poll(ACWifi *wifi) {
    if (!wifi || wifi->state != WIFI_STATE_CONNECTING) return 0;

    // Check wpa_supplicant status
    char cmd[256];
    snprintf(cmd, sizeof(cmd),
             "wpa_cli -i %s status 2>/dev/null | grep wpa_state", wifi->iface);
    FILE *fp = popen(cmd, "r");
    if (!fp) return 0;

    char line[128] = "";
    fgets(line, sizeof(line), fp);
    pclose(fp);

    if (strstr(line, "COMPLETED")) {
        // WPA connected! Start DHCP if not already running
        if (wifi->dhcp_pid <= 0) {
            ac_log("[wifi] WPA connected, starting DHCP");
            snprintf(wifi->status_msg, sizeof(wifi->status_msg), "getting IP...");

            pid_t pid = fork();
            if (pid == 0) {
                execl("/usr/sbin/dhclient", "dhclient",
                      "-1",  // try once
                      "-pf", "/tmp/dhclient.pid",
                      "-lf", "/tmp/dhclient.leases",
                      wifi->iface, NULL);
                _exit(1);
            }
            wifi->dhcp_pid = pid;
        }

        // Check if we have an IP
        snprintf(cmd, sizeof(cmd),
                 "ip addr show %s 2>/dev/null | grep 'inet ' | awk '{print $2}' | cut -d/ -f1",
                 wifi->iface);
        fp = popen(cmd, "r");
        if (fp) {
            char ip[32] = "";
            if (fgets(ip, sizeof(ip), fp)) {
                ip[strcspn(ip, "\n")] = 0;
                if (ip[0] && strcmp(ip, "0.0.0.0") != 0) {
                    strncpy(wifi->ip_address, ip, sizeof(wifi->ip_address) - 1);
                    wifi->state = WIFI_STATE_CONNECTED;
                    snprintf(wifi->status_msg, sizeof(wifi->status_msg),
                             "%s", wifi->ip_address);
                    ac_log("[wifi] Connected! IP: %s", wifi->ip_address);
                    pclose(fp);
                    return 1;
                }
            }
            pclose(fp);
        }

        // Check if dhclient has exited (failed)
        if (wifi->dhcp_pid > 0) {
            int status;
            pid_t r = waitpid(wifi->dhcp_pid, &status, WNOHANG);
            if (r > 0) {
                if (WIFEXITED(status) && WEXITSTATUS(status) != 0) {
                    wifi->state = WIFI_STATE_FAILED;
                    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "DHCP failed");
                    ac_log("[wifi] DHCP failed");
                    return 1;
                }
                wifi->dhcp_pid = 0;
            }
        }
    } else if (strstr(line, "DISCONNECTED") || strstr(line, "INTERFACE_DISABLED")) {
        static int connect_ticks = 0;
        connect_ticks++;
        if (connect_ticks > 600) { // ~10 seconds at 60fps
            wifi->state = WIFI_STATE_FAILED;
            snprintf(wifi->status_msg, sizeof(wifi->status_msg), "auth failed");
            ac_log("[wifi] Connection timeout");
            connect_ticks = 0;
            return 1;
        }
    }

    return 0;
}

void wifi_disconnect(ACWifi *wifi) {
    if (!wifi) return;

    run_cmd("killall wpa_supplicant 2>/dev/null");
    run_cmd("killall dhclient 2>/dev/null");

    char cmd[128];
    snprintf(cmd, sizeof(cmd), "ip addr flush dev %s 2>/dev/null", wifi->iface);
    run_cmd(cmd);

    if (wifi->wpa_pid > 0) {
        kill(wifi->wpa_pid, SIGTERM);
        waitpid(wifi->wpa_pid, NULL, WNOHANG);
    }
    if (wifi->dhcp_pid > 0) {
        kill(wifi->dhcp_pid, SIGTERM);
        waitpid(wifi->dhcp_pid, NULL, WNOHANG);
    }

    wifi->wpa_pid = 0;
    wifi->dhcp_pid = 0;
    wifi->state = WIFI_STATE_OFF;
    wifi->connected_ssid[0] = 0;
    wifi->ip_address[0] = 0;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "disconnected");
    ac_log("[wifi] Disconnected");
}

void wifi_destroy(ACWifi *wifi) {
    if (!wifi) return;
    wifi_disconnect(wifi);
    free(wifi);
}
