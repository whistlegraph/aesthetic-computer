#include "wifi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <stdarg.h>

// Defined in ac-native.c
extern void ac_log(const char *fmt, ...);

// Write to both ac_log and the wifi ring buffer (readable from JS)
static void wifi_log(ACWifi *wifi, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
static void wifi_log(ACWifi *wifi, const char *fmt, ...) {
    char buf[128];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    ac_log("[wifi] %s", buf);
    if (wifi) {
        int idx = wifi->log_count % 32;
        strncpy(wifi->log[idx], buf, 127);
        wifi->log[idx][127] = 0;
        wifi->log_count++;
    }
}

// ============================================================
// Helpers (called from wifi thread only — blocking is fine)
// ============================================================

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

static int detect_iface(char *out, int out_len) {
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
    fp = popen("ls -d /sys/class/net/*/wireless 2>/dev/null | head -1", "r");
    if (fp) {
        char buf[128] = "";
        if (fgets(buf, sizeof(buf), fp)) {
            buf[strcspn(buf, "\n")] = 0;
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

// Thread-safe state update helpers
static void wifi_set_state(ACWifi *wifi, WiFiState st) {
    pthread_mutex_lock(&wifi->lock);
    wifi->state = st;
    pthread_mutex_unlock(&wifi->lock);
}

static void wifi_set_status(ACWifi *wifi, const char *msg) {
    pthread_mutex_lock(&wifi->lock);
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "%s", msg);
    pthread_mutex_unlock(&wifi->lock);
}

static void wifi_set_state_and_status(ACWifi *wifi, WiFiState st, const char *msg) {
    pthread_mutex_lock(&wifi->lock);
    wifi->state = st;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "%s", msg);
    pthread_mutex_unlock(&wifi->lock);
}

// ============================================================
// Scan (runs on wifi thread)
// ============================================================

static void wifi_do_scan(ACWifi *wifi) {
    if (!wifi->iface[0]) return;

    wifi_set_state_and_status(wifi, WIFI_STATE_SCANNING, "scanning...");

    // Bring interface up + set regulatory domain (blocking — that's fine here)
    char cmd[256];
    snprintf(cmd, sizeof(cmd), "ip link set %s up 2>/dev/null", wifi->iface);
    run_cmd(cmd);
    run_cmd("iw reg set US 2>/dev/null");

    // Remove stale scan files
    unlink("/tmp/wifi_scan.txt");
    unlink("/tmp/wifi_scan_err.txt");

    // Run scan synchronously on this thread (no need for background shell)
    snprintf(cmd, sizeof(cmd),
             "iw dev %s scan > /tmp/wifi_scan.txt 2>/tmp/wifi_scan_err.txt",
             wifi->iface);
    int rc = run_cmd(cmd);
    if (rc != 0) {
        ac_log("[wifi] scan returned code %d", rc);
        FILE *err = fopen("/tmp/wifi_scan_err.txt", "r");
        if (err) {
            char errbuf[256] = "";
            while (fgets(errbuf, sizeof(errbuf), err)) {
                errbuf[strcspn(errbuf, "\n")] = 0;
                if (errbuf[0]) ac_log("[wifi] scan stderr: %s", errbuf);
            }
            fclose(err);
        }
    }

    // Parse scan results
    FILE *fp = fopen("/tmp/wifi_scan.txt", "r");
    if (!fp) {
        wifi_set_state_and_status(wifi, WIFI_STATE_SCAN_DONE, "scan failed");
        return;
    }

    WiFiNetwork nets[WIFI_MAX_NETWORKS];
    int count = 0;
    char line[512];
    int cur = -1;

    while (fgets(line, sizeof(line), fp) && count < WIFI_MAX_NETWORKS) {
        char bssid[18];
        if (sscanf(line, "BSS %17s", bssid) == 1) {
            cur = count++;
            memset(&nets[cur], 0, sizeof(WiFiNetwork));
            strncpy(nets[cur].bssid, bssid, 17);
            nets[cur].signal = -100;
        }
        if (cur < 0) continue;

        int sig;
        if (sscanf(line, "\tsignal: %d", &sig) == 1)
            nets[cur].signal = sig;

        char ssid[64];
        if (sscanf(line, "\tSSID: %63[^\n]", ssid) == 1)
            strncpy(nets[cur].ssid, ssid, WIFI_SSID_MAX - 1);

        if (strstr(line, "WPA") || strstr(line, "RSN"))
            nets[cur].encrypted = 1;
    }
    fclose(fp);
    unlink("/tmp/wifi_scan.txt");

    // Sort by signal (strongest first)
    for (int i = 0; i < count - 1; i++)
        for (int j = i + 1; j < count; j++)
            if (nets[j].signal > nets[i].signal) {
                WiFiNetwork tmp = nets[i]; nets[i] = nets[j]; nets[j] = tmp;
            }

    // Remove empty SSIDs
    int w = 0;
    for (int r = 0; r < count; r++)
        if (nets[r].ssid[0]) { if (w != r) nets[w] = nets[r]; w++; }

    // Commit results under lock
    pthread_mutex_lock(&wifi->lock);
    memcpy(wifi->networks, nets, sizeof(WiFiNetwork) * w);
    wifi->network_count = w;
    wifi->state = WIFI_STATE_SCAN_DONE;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "%d networks", w);
    pthread_mutex_unlock(&wifi->lock);

    wifi_log(wifi, "Scan complete: %d networks", w);
}

// ============================================================
// Connect (runs on wifi thread)
// ============================================================

static void wifi_do_connect(ACWifi *wifi, const char *ssid, const char *password) {
    if (!ssid || !wifi->iface[0]) return;

    wifi_set_state_and_status(wifi, WIFI_STATE_CONNECTING, "connecting...");
    wifi_log(wifi, "Connecting to '%s'", ssid);

    // Kill any existing wpa_supplicant / dhclient
    if (wifi->wpa_pid > 0) {
        kill(wifi->wpa_pid, SIGTERM);
        waitpid(wifi->wpa_pid, NULL, 0);
        wifi->wpa_pid = 0;
    }
    if (wifi->dhcp_pid > 0) {
        kill(wifi->dhcp_pid, SIGTERM);
        waitpid(wifi->dhcp_pid, NULL, 0);
        wifi->dhcp_pid = 0;
    }
    run_cmd("killall wpa_supplicant 2>/dev/null; killall dhclient 2>/dev/null");

    // Write wpa_supplicant config
    FILE *fp = fopen("/tmp/wpa.conf", "w");
    if (!fp) {
        wifi_set_state_and_status(wifi, WIFI_STATE_FAILED, "config error");
        return;
    }
    fprintf(fp, "ctrl_interface=/var/run/wpa_supplicant\n");
    fprintf(fp, "update_config=1\n\n");
    fprintf(fp, "network={\n");
    fprintf(fp, "    ssid=\"%s\"\n", ssid);
    if (password && password[0])
        fprintf(fp, "    psk=\"%s\"\n", password);
    else
        fprintf(fp, "    key_mgmt=NONE\n");
    fprintf(fp, "}\n");
    fclose(fp);

    // Create required directories
    mkdir("/var", 0755);
    mkdir("/var/run", 0755);
    mkdir("/var/run/wpa_supplicant", 0755);

    // Remove stale ctrl_interface socket — wpa_supplicant refuses to start
    // if /var/run/wpa_supplicant/<iface> already exists from a prior run
    {
        char sock_path[128];
        snprintf(sock_path, sizeof(sock_path),
                 "/var/run/wpa_supplicant/%s", wifi->iface);
        if (unlink(sock_path) == 0)
            wifi_log(wifi, "Removed stale socket: %s", sock_path);
    }

    // Start wpa_supplicant
    pid_t pid = fork();
    if (pid == 0) {
        const char *wpa_paths[] = {
            "/bin/wpa_supplicant", "/usr/bin/wpa_supplicant",
            "/usr/sbin/wpa_supplicant", "/sbin/wpa_supplicant", NULL
        };
        for (int i = 0; wpa_paths[i]; i++) {
            if (file_exists(wpa_paths[i])) {
                execl(wpa_paths[i], "wpa_supplicant",
                      "-i", wifi->iface, "-c", "/tmp/wpa.conf",
                      "-B", "-P", "/tmp/wpa.pid", NULL);
            }
        }
        _exit(1);
    }
    wifi->wpa_pid = pid;
    waitpid(pid, NULL, 0); // Wait for wpa_supplicant to daemonize (-B)

    pthread_mutex_lock(&wifi->lock);
    strncpy(wifi->connected_ssid, ssid, WIFI_SSID_MAX - 1);
    pthread_mutex_unlock(&wifi->lock);

    // Poll for WPA completion + DHCP (blocking loop on this thread)
    int connect_ticks = 0;
    int dhcp_started = 0;
    char last_wpa_state[64] = "";

    while (connect_ticks < 1200 && wifi->thread_running) { // ~60 seconds max (50ms polls)
        // Check if a new command interrupted us
        if (wifi->pending_cmd != WIFI_CMD_NONE) {
            wifi_log(wifi, "Connect interrupted by new command");
            return;
        }

        usleep(50000); // 50ms between polls
        connect_ticks++;

        // Check wpa_supplicant status
        char cmd[256];
        snprintf(cmd, sizeof(cmd),
                 "wpa_cli -i %s status 2>/dev/null | grep wpa_state", wifi->iface);
        FILE *wfp = popen(cmd, "r");
        if (!wfp) continue;

        char line[128] = "";
        fgets(line, sizeof(line), wfp);
        pclose(wfp);

        // Extract WPA state for logging/status
        {
            char *eq = strchr(line, '=');
            const char *st = eq ? eq + 1 : line;
            char state_str[64] = "";
            strncpy(state_str, st, sizeof(state_str) - 1);
            state_str[strcspn(state_str, "\n\r")] = 0;
            if (state_str[0] && strcmp(state_str, last_wpa_state) != 0) {
                wifi_log(wifi, "WPA: %s (tick %d)", state_str, connect_ticks);
                strncpy(last_wpa_state, state_str, sizeof(last_wpa_state) - 1);
                // Update user-visible status with WPA state detail
                if (strstr(state_str, "SCANNING"))
                    wifi_set_status(wifi, "scanning...");
                else if (strstr(state_str, "ASSOCIATING"))
                    wifi_set_status(wifi, "associating...");
                else if (strstr(state_str, "4WAY_HANDSHAKE"))
                    wifi_set_status(wifi, "authenticating...");
                else if (strstr(state_str, "GROUP_HANDSHAKE"))
                    wifi_set_status(wifi, "group handshake...");
            }
        }

        if (strstr(line, "COMPLETED")) {
            // WPA connected — start DHCP if not already running
            if (!dhcp_started) {
                wifi_log(wifi, "WPA connected, starting DHCP");
                wifi_set_status(wifi, "getting IP...");

                pid_t dpid = fork();
                if (dpid == 0) {
                    int fd = open("/tmp/dhcp.log", O_WRONLY|O_CREAT|O_TRUNC, 0644);
                    if (fd >= 0) { dup2(fd, 2); dup2(fd, 1); close(fd); }
                    // Prefer udhcpc (busybox) — fast, no script needed
                    const char *udhcpc = NULL;
                    if (file_exists("/sbin/udhcpc")) udhcpc = "/sbin/udhcpc";
                    else if (file_exists("/bin/udhcpc")) udhcpc = "/bin/udhcpc";
                    else if (file_exists("/usr/bin/udhcpc")) udhcpc = "/usr/bin/udhcpc";
                    if (udhcpc) {
                        fprintf(stderr, "[wifi] using udhcpc: %s\n", udhcpc);
                        execl(udhcpc, "udhcpc", "-i", wifi->iface,
                              "-n",  // exit if no lease (don't background)
                              "-q",  // quit after obtaining lease
                              "-s", "/usr/share/udhcpc/default.script",
                              "-t", "5",  // 5 retries
                              "-T", "3",  // 3 second timeout
                              NULL);
                        // execl failed
                        fprintf(stderr, "[wifi] udhcpc execl failed: %s\n", strerror(errno));
                    }
                    // Fallback to dhclient
                    const char *dhc_paths[] = {
                        "/sbin/dhclient", "/usr/sbin/dhclient", NULL
                    };
                    const char *script = "/sbin/dhclient-script";
                    if (file_exists("/bin/dhclient-script")) script = "/bin/dhclient-script";
                    for (int i = 0; dhc_paths[i]; i++) {
                        if (file_exists(dhc_paths[i])) {
                            execl(dhc_paths[i], "dhclient",
                                  "-v", "-1", "-sf", script,
                                  "-pf", "/tmp/dhclient.pid",
                                  "-lf", "/tmp/dhclient.leases",
                                  wifi->iface, NULL);
                        }
                    }
                    _exit(1);
                }
                wifi->dhcp_pid = dpid;
                dhcp_started = 1;
            }

            // Check for IP address
            snprintf(cmd, sizeof(cmd),
                     "ip addr show %s 2>/dev/null | grep 'inet ' | awk '{print $2}' | cut -d/ -f1",
                     wifi->iface);
            FILE *ifp = popen(cmd, "r");
            if (ifp) {
                char ip[32] = "";
                if (fgets(ip, sizeof(ip), ifp)) {
                    ip[strcspn(ip, "\n")] = 0;
                    if (ip[0] && strcmp(ip, "0.0.0.0") != 0) {
                        pthread_mutex_lock(&wifi->lock);
                        strncpy(wifi->ip_address, ip, sizeof(wifi->ip_address) - 1);
                        wifi->state = WIFI_STATE_CONNECTED;
                        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "%s", ip);
                        pthread_mutex_unlock(&wifi->lock);
                        wifi_log(wifi, "Connected! IP: %s", ip);

                        // Captive portal detection + auto-accept
                        {
                            char portal_cmd[256];
                            snprintf(portal_cmd, sizeof(portal_cmd),
                                "curl -sL -o /dev/null -w '%%{http_code}' "
                                "--max-time 5 --connect-timeout 3 "
                                "http://connectivitycheck.gstatic.com/generate_204 "
                                "2>/dev/null");
                            FILE *pf = popen(portal_cmd, "r");
                            if (pf) {
                                char code[8] = "";
                                if (fgets(code, sizeof(code), pf))
                                    code[strcspn(code, "\n")] = 0;
                                pclose(pf);
                                if (strcmp(code, "204") == 0) {
                                    wifi_log(wifi, "Internet: OK (no captive portal)");
                                } else if (code[0]) {
                                    wifi_log(wifi, "Captive portal detected (HTTP %s), trying auto-accept...", code);
                                    // Follow redirect and accept — many portals just need a GET
                                    snprintf(portal_cmd, sizeof(portal_cmd),
                                        "curl -sL --max-time 10 --connect-timeout 5 "
                                        "-o /dev/null -w '%%{http_code}' "
                                        "http://connectivitycheck.gstatic.com/generate_204 "
                                        "2>/dev/null");
                                    FILE *af = popen(portal_cmd, "r");
                                    if (af) {
                                        char acode[8] = "";
                                        if (fgets(acode, sizeof(acode), af))
                                            acode[strcspn(acode, "\n")] = 0;
                                        pclose(af);
                                        if (strcmp(acode, "204") == 0)
                                            wifi_log(wifi, "Captive portal cleared!");
                                        else
                                            wifi_log(wifi, "Captive portal may need manual login (HTTP %s)", acode);
                                    }
                                } else {
                                    wifi_log(wifi, "Connectivity check failed (no response)");
                                }
                            }
                        }

                        // Save credentials for auto-reconnect
                        strncpy(wifi->last_ssid, ssid, WIFI_SSID_MAX - 1);
                        strncpy(wifi->last_pass, password ? password : "", WIFI_PASS_MAX - 1);
                        wifi->reconnect_failures = 0;

                        // Ensure resolv.conf has DNS fallbacks
                        mkdir("/etc", 0755);
                        FILE *rf = fopen("/etc/resolv.conf", "a");
                        if (rf) {
                            fseek(rf, 0, SEEK_END);
                            if (ftell(rf) == 0)
                                fprintf(rf, "nameserver 8.8.8.8\nnameserver 1.1.1.1\n");
                            fclose(rf);
                        }
                        pclose(ifp);
                        return; // Success!
                    }
                }
                pclose(ifp);
            }

            // Check if DHCP client died
            if (wifi->dhcp_pid > 0) {
                int status;
                pid_t r = waitpid(wifi->dhcp_pid, &status, WNOHANG);
                if (r > 0) {
                    if (WIFEXITED(status) && WEXITSTATUS(status) != 0) {
                        wifi_log(wifi, "DHCP exit %d", WEXITSTATUS(status));
                        FILE *dlog = fopen("/tmp/dhcp.log", "r");
                        if (dlog) {
                            char dline[256];
                            while (fgets(dline, sizeof(dline), dlog))
                                wifi_log(wifi, "dhclient: %s", dline);
                            fclose(dlog);
                        }
                        // Retry DHCP on next iteration
                        wifi->dhcp_pid = 0;
                        dhcp_started = 0;
                    }
                    if (WIFSIGNALED(status)) {
                        wifi_log(wifi, "dhclient killed by signal %d", WTERMSIG(status));
                        wifi->dhcp_pid = 0;
                        dhcp_started = 0;
                    }
                }
            }
        } else if (strstr(line, "DISCONNECTED") || strstr(line, "INTERFACE_DISABLED")) {
            // Still waiting for WPA auth
            if (connect_ticks > 200) { // ~10 seconds (50ms polls)
                wifi_set_state_and_status(wifi, WIFI_STATE_FAILED, "auth failed");
                wifi_log(wifi, "Auth failed after %d ticks", connect_ticks);
                return;
            }
        }
    }

    // Timed out
    if (wifi->state == WIFI_STATE_CONNECTING) {
        wifi_set_state_and_status(wifi, WIFI_STATE_FAILED, "timeout");
        wifi_log(wifi, "Connect timed out after 60s (last WPA: %s)", last_wpa_state);
    }
}

// ============================================================
// Disconnect (runs on wifi thread)
// ============================================================

static void wifi_do_disconnect(ACWifi *wifi) {
    run_cmd("killall wpa_supplicant 2>/dev/null");
    run_cmd("killall dhclient 2>/dev/null");

    char cmd[128];
    snprintf(cmd, sizeof(cmd), "ip addr flush dev %s 2>/dev/null", wifi->iface);
    run_cmd(cmd);

    if (wifi->wpa_pid > 0) {
        kill(wifi->wpa_pid, SIGTERM);
        waitpid(wifi->wpa_pid, NULL, 0);
    }
    if (wifi->dhcp_pid > 0) {
        kill(wifi->dhcp_pid, SIGTERM);
        waitpid(wifi->dhcp_pid, NULL, 0);
    }

    pthread_mutex_lock(&wifi->lock);
    wifi->wpa_pid = 0;
    wifi->dhcp_pid = 0;
    wifi->state = WIFI_STATE_OFF;
    wifi->connected_ssid[0] = 0;
    wifi->ip_address[0] = 0;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "disconnected");
    pthread_mutex_unlock(&wifi->lock);

    wifi_log(wifi, "Disconnected");
}

// ============================================================
// Auto-connect (runs on wifi thread)
// ============================================================

// Hardcoded fallback SSID/pass (matches wifi.mjs AC_SSID/AC_PASS)
#define AC_SSID "aesthetic.computer"
#define AC_PASS "aesthetic.computer"

static void wifi_do_autoconnect(ACWifi *wifi) {
    wifi_log(wifi, "Auto-connect: scanning...");

    // Step 1: Scan
    wifi_do_scan(wifi);
    if (wifi->network_count == 0) {
        wifi_log(wifi, "Auto-connect: no networks found");
        wifi_set_state_and_status(wifi, WIFI_STATE_SCAN_DONE, "no networks");
        return;
    }

    // Step 2: Read saved credentials from /mnt/wifi_creds.json
    // Format: [{"ssid":"MyNet","pass":"secret"}, ...]
    typedef struct { char ssid[WIFI_SSID_MAX]; char pass[WIFI_PASS_MAX]; } SavedCred;
    SavedCred creds[16];
    int cred_count = 0;

    // Always include preset networks
    strncpy(creds[0].ssid, AC_SSID, WIFI_SSID_MAX - 1);
    strncpy(creds[0].pass, AC_PASS, WIFI_PASS_MAX - 1);
    cred_count = 1;
    // GettyLink (open network, no password)
    strncpy(creds[cred_count].ssid, "GettyLink", WIFI_SSID_MAX - 1);
    creds[cred_count].pass[0] = '\0';
    cred_count++;

    FILE *fp = fopen("/mnt/wifi_creds.json", "r");
    if (fp) {
        char buf[2048] = "";
        size_t n = fread(buf, 1, sizeof(buf) - 1, fp);
        buf[n] = 0;
        fclose(fp);

        // Minimal JSON array parse: find each {"ssid":"...","pass":"..."}
        char *p = buf;
        while ((p = strstr(p, "\"ssid\"")) && cred_count < 16) {
            char *sq = strchr(p + 6, '"'); // opening quote of ssid value
            if (!sq) break;
            sq++;
            char *eq = strchr(sq, '"'); // closing quote
            if (!eq) break;

            int len = (int)(eq - sq);
            if (len > 0 && len < WIFI_SSID_MAX) {
                strncpy(creds[cred_count].ssid, sq, len);
                creds[cred_count].ssid[len] = 0;

                // Find corresponding "pass" value
                creds[cred_count].pass[0] = 0;
                char *pp = strstr(eq, "\"pass\"");
                if (pp) {
                    char *pq = strchr(pp + 6, '"');
                    if (pq) {
                        pq++;
                        char *pe = strchr(pq, '"');
                        if (pe) {
                            int plen = (int)(pe - pq);
                            if (plen >= 0 && plen < WIFI_PASS_MAX) {
                                strncpy(creds[cred_count].pass, pq, plen);
                                creds[cred_count].pass[plen] = 0;
                            }
                        }
                    }
                }

                // Skip duplicates of AC preset
                if (strcmp(creds[cred_count].ssid, AC_SSID) != 0)
                    cred_count++;
            }
            p = eq + 1;
        }
        wifi_log(wifi, "Auto-connect: %d saved creds", cred_count);
    } else {
        wifi_log(wifi, "Auto-connect: no saved creds, preset only");
    }

    // Step 3: Match scanned networks against saved creds (by signal strength)
    // Networks are already sorted by signal (strongest first) from wifi_do_scan
    pthread_mutex_lock(&wifi->lock);
    for (int i = 0; i < wifi->network_count; i++) {
        for (int j = 0; j < cred_count; j++) {
            if (strcmp(wifi->networks[i].ssid, creds[j].ssid) == 0) {
                char ssid[WIFI_SSID_MAX], pass[WIFI_PASS_MAX];
                strncpy(ssid, creds[j].ssid, WIFI_SSID_MAX - 1);
                ssid[WIFI_SSID_MAX - 1] = 0;
                strncpy(pass, creds[j].pass, WIFI_PASS_MAX - 1);
                pass[WIFI_PASS_MAX - 1] = 0;
                pthread_mutex_unlock(&wifi->lock);

                wifi_log(wifi, "Trying '%s' (%d dBm)",
                       ssid, wifi->networks[i].signal);
                wifi_do_connect(wifi, ssid, pass);

                if (wifi->state == WIFI_STATE_CONNECTED) {
                    wifi_log(wifi, "Auto-connect: success!");
                    return;
                }
                wifi_log(wifi, "'%s' failed, trying next...", ssid);
                pthread_mutex_lock(&wifi->lock);
                break; // Move to next scanned network
            }
        }
    }
    pthread_mutex_unlock(&wifi->lock);

    wifi_log(wifi, "Auto-connect: no match");
    wifi_set_state_and_status(wifi, WIFI_STATE_SCAN_DONE, "no saved network");
}

// ============================================================
// Worker thread
// ============================================================

// Check if interface still has an IP (connectivity watchdog)
// Also updates signal_strength while connected.
static int wifi_check_link(ACWifi *wifi) {
    if (!wifi->iface[0]) return 0;
    char cmd[256];
    snprintf(cmd, sizeof(cmd),
             "ip -4 addr show %s 2>/dev/null | grep -q 'inet '", wifi->iface);
    int has_ip = system(cmd) == 0;

    // Update signal strength
    if (has_ip) {
        snprintf(cmd, sizeof(cmd),
                 "iw dev %s link 2>/dev/null | grep signal | awk '{print $2}'",
                 wifi->iface);
        FILE *fp = popen(cmd, "r");
        if (fp) {
            char buf[32] = "";
            if (fgets(buf, sizeof(buf), fp)) {
                int sig = atoi(buf);
                if (sig < 0) {
                    pthread_mutex_lock(&wifi->lock);
                    wifi->signal_strength = sig;
                    pthread_mutex_unlock(&wifi->lock);
                }
            }
            pclose(fp);
        }
    }

    return has_ip;
}

static void *wifi_thread_fn(void *arg) {
    ACWifi *wifi = (ACWifi *)arg;
    int watchdog_counter = 0;

    while (wifi->thread_running) {
        // Wait for a command (with 2s timeout for watchdog checks)
        pthread_mutex_lock(&wifi->lock);
        if (wifi->pending_cmd == WIFI_CMD_NONE) {
            struct timespec ts;
            clock_gettime(CLOCK_REALTIME, &ts);
            ts.tv_sec += 2; // 2-second watchdog interval
            pthread_cond_timedwait(&wifi->cond, &wifi->lock, &ts);
        }

        WiFiCommand cmd = wifi->pending_cmd;
        wifi->pending_cmd = WIFI_CMD_NONE;

        // Copy command args before releasing lock
        char ssid[WIFI_SSID_MAX] = "";
        char pass[WIFI_PASS_MAX] = "";
        if (cmd == WIFI_CMD_CONNECT) {
            strncpy(ssid, wifi->cmd_ssid, WIFI_SSID_MAX - 1);
            strncpy(pass, wifi->cmd_pass, WIFI_PASS_MAX - 1);
        }
        WiFiState cur_state = wifi->state;
        pthread_mutex_unlock(&wifi->lock);

        // Execute command (blocking calls are fine — we're on the wifi thread)
        switch (cmd) {
            case WIFI_CMD_SCAN:          wifi_do_scan(wifi); break;
            case WIFI_CMD_CONNECT:       wifi_do_connect(wifi, ssid, pass); break;
            case WIFI_CMD_DISCONNECT:    wifi_do_disconnect(wifi); break;
            case WIFI_CMD_AUTOCONNECT:   wifi_do_autoconnect(wifi); break;
            default: break;
        }

        // Watchdog: check connectivity every ~10s (5 iterations × 2s)
        if (cmd == WIFI_CMD_NONE && cur_state == WIFI_STATE_CONNECTED) {
            watchdog_counter++;
            if (watchdog_counter >= 5) {
                watchdog_counter = 0;
                if (!wifi_check_link(wifi)) {
                    wifi_log(wifi, "Connection lost — reconnecting '%s'",
                           wifi->last_ssid);
                    wifi_set_state_and_status(wifi, WIFI_STATE_CONNECTING,
                                              "reconnecting...");
                    wifi_do_connect(wifi, wifi->last_ssid, wifi->last_pass);
                    if (wifi->state != WIFI_STATE_CONNECTED) {
                        wifi->reconnect_failures++;
                        wifi_log(wifi, "Reconnect failed (%d)",
                               wifi->reconnect_failures);
                        // Back off: wait longer between retries
                        if (wifi->reconnect_failures > 3)
                            sleep(wifi->reconnect_failures * 2);
                    }
                }
            }
        } else {
            watchdog_counter = 0;
        }
    }

    return NULL;
}

// ============================================================
// Public API (called from main thread — all non-blocking)
// ============================================================

ACWifi *wifi_init(void) {
    ACWifi *wifi = calloc(1, sizeof(ACWifi));
    if (!wifi) return NULL;

    wifi->state = WIFI_STATE_OFF;
    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "initializing...");
    wifi->iface[0] = 0;
    pthread_mutex_init(&wifi->lock, NULL);
    pthread_cond_init(&wifi->cond, NULL);

    // Check if iw exists
    int has_iw = file_exists("/usr/sbin/iw") || file_exists("/sbin/iw") ||
                 file_exists("/usr/bin/iw") || file_exists("/bin/iw");
    if (!has_iw)
        has_iw = (system("which iw >/dev/null 2>&1") == 0);
    if (!has_iw) {
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "iw not found");
        ac_log("[wifi] iw binary not found");
        return wifi;
    }
    ac_log("[wifi] iw binary found");

    // Log diagnostic info
    {
        FILE *dbg;
        dbg = popen("ls /sys/class/net/ 2>/dev/null", "r");
        if (dbg) {
            char buf[256] = "";
            if (fgets(buf, sizeof(buf), dbg)) {
                buf[strcspn(buf, "\n")] = 0;
                ac_log("[wifi] net interfaces at start: %s", buf);
            }
            pclose(dbg);
        }
        dbg = popen("ls /lib/firmware/iwlwifi-*.ucode 2>/dev/null | head -3", "r");
        if (dbg) {
            char buf[256] = "";
            while (fgets(buf, sizeof(buf), dbg)) {
                buf[strcspn(buf, "\n")] = 0;
                ac_log("[wifi] firmware: %s", buf);
            }
            pclose(dbg);
        }
        // Read kernel log for wireless-related messages
        int kmsg_fd = open("/dev/kmsg", O_RDONLY | O_NONBLOCK);
        if (kmsg_fd >= 0) {
            lseek(kmsg_fd, 0, SEEK_SET);
            char kbuf[512];
            int kmsg_count = 0, matched = 0;
            ssize_t rr;
            while ((rr = read(kmsg_fd, kbuf, sizeof(kbuf) - 1)) > 0 && kmsg_count < 2000) {
                kbuf[rr] = 0;
                if (strcasestr(kbuf, "iwl") || strcasestr(kbuf, "wifi") ||
                    strcasestr(kbuf, "wlan") || strcasestr(kbuf, "80211") ||
                    strcasestr(kbuf, "firmware") ||
                    (strcasestr(kbuf, "pci") && kmsg_count < 100)) {
                    char *msg = strchr(kbuf, ';');
                    if (msg) msg = strchr(msg + 1, ';');
                    if (msg) msg = strchr(msg + 1, ';');
                    if (msg) msg++; else msg = kbuf;
                    msg[strcspn(msg, "\n")] = 0;
                    ac_log("[wifi] kmsg: %s", msg);
                    matched++;
                }
                kmsg_count++;
            }
            ac_log("[wifi] kmsg: %d total messages, %d matched", kmsg_count, matched);
            close(kmsg_fd);
        }
        // PCI device enumeration
        dbg = popen("for d in /sys/bus/pci/devices/*; do echo \"$(basename $d) $(cat $d/vendor 2>/dev/null):$(cat $d/device 2>/dev/null)\"; done 2>/dev/null", "r");
        if (dbg) {
            char buf[256] = "";
            while (fgets(buf, sizeof(buf), dbg)) {
                buf[strcspn(buf, "\n")] = 0;
                ac_log("[wifi] PCI id: %s", buf);
            }
            pclose(dbg);
        }
    }

    // Wait for wireless interface (up to 3 seconds)
    int found = 0;
    for (int attempt = 0; attempt < 30; attempt++) {
        if (detect_iface(wifi->iface, sizeof(wifi->iface))) {
            found = 1;
            break;
        }
        ac_log("[wifi] Waiting for wireless interface (attempt %d/30)...", attempt + 1);
        usleep(100000);
    }

    if (!found) {
        ac_log("[wifi] No wireless interface detected");
        FILE *fp = popen("ls /sys/class/net/ 2>/dev/null", "r");
        if (fp) {
            char buf[256] = "";
            if (fgets(buf, sizeof(buf), fp)) {
                buf[strcspn(buf, "\n")] = 0;
                ac_log("[wifi]   interfaces: %s", buf);
            }
            pclose(fp);
        }
        snprintf(wifi->status_msg, sizeof(wifi->status_msg), "no wifi hw");
        return wifi;
    }
    ac_log("[wifi] Detected interface: %s", wifi->iface);

    // Unblock rfkill if needed
    {
        char cmd[256];
        snprintf(cmd, sizeof(cmd),
                 "cat /sys/class/net/%s/phy80211/rfkill*/soft 2>/dev/null | head -1",
                 wifi->iface);
        FILE *fp = popen(cmd, "r");
        if (fp) {
            char buf[8] = "";
            if (fgets(buf, sizeof(buf), fp) && buf[0] == '1') {
                ac_log("[wifi] Soft-blocked, unblocking...");
                run_cmd("rfkill unblock wifi 2>/dev/null");
                usleep(200000);
            }
            pclose(fp);
        }
    }

    // Bring up the interface
    {
        char cmd[128];
        snprintf(cmd, sizeof(cmd), "ip link set %s up 2>/dev/null", wifi->iface);
        run_cmd(cmd);

        snprintf(cmd, sizeof(cmd), "ip link show %s 2>/dev/null", wifi->iface);
        FILE *fp = popen(cmd, "r");
        if (fp) {
            char buf[256] = "";
            if (fgets(buf, sizeof(buf), fp)) {
                buf[strcspn(buf, "\n")] = 0;
                ac_log("[wifi] link status: %s", buf);
            }
            pclose(fp);
        }
    }

    snprintf(wifi->status_msg, sizeof(wifi->status_msg), "ready");
    ac_log("[wifi] Interface %s is up", wifi->iface);

    // Start worker thread
    wifi->thread_running = 1;
    if (pthread_create(&wifi->thread, NULL, wifi_thread_fn, wifi) != 0) {
        ac_log("[wifi] Failed to create wifi thread");
        wifi->thread_running = 0;
    } else {
        ac_log("[wifi] Worker thread started");
    }

    return wifi;
}

void wifi_scan(ACWifi *wifi) {
    if (!wifi || !wifi->iface[0] || !wifi->thread_running) return;

    pthread_mutex_lock(&wifi->lock);
    // Don't interrupt scanning, connecting, or DHCP in progress
    if (wifi->state == WIFI_STATE_SCANNING ||
        wifi->state == WIFI_STATE_CONNECTING ||
        wifi->state == WIFI_STATE_CONNECTED) {
        pthread_mutex_unlock(&wifi->lock);
        return;
    }
    wifi->pending_cmd = WIFI_CMD_SCAN;
    pthread_cond_signal(&wifi->cond);
    pthread_mutex_unlock(&wifi->lock);
}

void wifi_connect(ACWifi *wifi, const char *ssid, const char *password) {
    if (!wifi || !ssid || !wifi->iface[0] || !wifi->thread_running) return;

    pthread_mutex_lock(&wifi->lock);
    strncpy(wifi->cmd_ssid, ssid, WIFI_SSID_MAX - 1);
    wifi->cmd_ssid[WIFI_SSID_MAX - 1] = 0;
    if (password) {
        strncpy(wifi->cmd_pass, password, WIFI_PASS_MAX - 1);
        wifi->cmd_pass[WIFI_PASS_MAX - 1] = 0;
    } else {
        wifi->cmd_pass[0] = 0;
    }
    wifi->pending_cmd = WIFI_CMD_CONNECT;
    pthread_cond_signal(&wifi->cond);
    pthread_mutex_unlock(&wifi->lock);
}

void wifi_disconnect(ACWifi *wifi) {
    if (!wifi || !wifi->thread_running) return;

    pthread_mutex_lock(&wifi->lock);
    wifi->pending_cmd = WIFI_CMD_DISCONNECT;
    pthread_cond_signal(&wifi->cond);
    pthread_mutex_unlock(&wifi->lock);
}

// No-ops — polling now happens inside the wifi thread.
// Main thread just reads wifi->state / wifi->networks directly.
int wifi_scan_poll(ACWifi *wifi) {
    (void)wifi;
    return 0;
}

int wifi_connect_poll(ACWifi *wifi) {
    (void)wifi;
    return 0;
}

void wifi_autoconnect(ACWifi *wifi) {
    if (!wifi || !wifi->iface[0] || !wifi->thread_running) return;

    pthread_mutex_lock(&wifi->lock);
    wifi->pending_cmd = WIFI_CMD_AUTOCONNECT;
    pthread_cond_signal(&wifi->cond);
    pthread_mutex_unlock(&wifi->lock);
}

void wifi_destroy(ACWifi *wifi) {
    if (!wifi) return;

    // Signal thread to stop
    wifi->thread_running = 0;
    pthread_mutex_lock(&wifi->lock);
    pthread_cond_signal(&wifi->cond);
    pthread_mutex_unlock(&wifi->lock);

    // Wait for thread to finish
    if (wifi->thread)
        pthread_join(wifi->thread, NULL);

    wifi_do_disconnect(wifi);

    pthread_mutex_destroy(&wifi->lock);
    pthread_cond_destroy(&wifi->cond);
    free(wifi);
}
