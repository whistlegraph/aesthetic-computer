// lanserv — LAN dev server: HTTP control endpoint + mDNS/Bonjour responder.
// One pthread, select()-driven across the HTTP listener and the mDNS socket.
// Reload requests funnel through /tmp/ac-jump, the same file the ssh path
// uses, so the main loop has a single watcher for every trigger.

#define _GNU_SOURCE // strcasestr

#include "lanserv.h"

#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

extern void ac_log(const char *fmt, ...);

#ifndef MSG_NOSIGNAL // absent on darwin (host-side syntax checks)
#define MSG_NOSIGNAL 0
#endif

#define LANSERV_HTTP_PORT 80
#define LANSERV_HTTP_PORT_FALLBACK 8080
#define LANSERV_MAX_BODY (8 * 1024 * 1024)
#define MDNS_PORT 5353
#define MDNS_GROUP "224.0.0.251"
#define MDNS_TTL 120

typedef struct {
    pthread_t thread;
    pthread_mutex_t lock;
    volatile int running;
    char piece[64];
    char ip[16];
    char build[64];
    volatile int ip_changed; // thread should rejoin multicast + announce
    time_t started;
    int http_port;
    char fp[24];             // hardware fingerprint (16 hex) for the name
} LanServ;

static LanServ g_lan = {0};

// ── small helpers ──────────────────────────────────────────────────────

// Stable, memorable per-machine name derived from the hardware
// fingerprint: "<adjective>-<animal>". Same machine → same name across
// reflashes (the fp is hardware-derived). 32×32 = 1024 combinations —
// ample for the fleet, and collisions still differ by the curated slot.
static const char *LAN_ADJ[32] = {
    "amber","azure","cobalt","coral","crimson","dusk","ember","frost",
    "golden","hazel","indigo","ivory","jade","lunar","mauve","neon",
    "olive","onyx","opal","pearl","quartz","rust","sable","scarlet",
    "silver","slate","solar","teal","umber","velvet","violet","zinc",
};
static const char *LAN_ANIMAL[32] = {
    "otter","fox","wolf","lynx","puma","hare","mink","stoat",
    "heron","raven","finch","wren","swift","crane","ibis","lark",
    "moth","newt","toad","carp","perch","tetra","koi","eel",
    "gecko","skink","viper","adder","beetle","mantis","cicada","drake",
};

static void lan_name_from_fp(const char *fp, char *out, size_t outlen) {
    // Two 8-hex-digit halves of the fp index the word lists.
    char a[9] = {0}, b[9] = {0};
    snprintf(a, sizeof(a), "%.8s", fp);
    snprintf(b, sizeof(b), "%.8s", fp + 8);
    unsigned long ai = strtoul(a, NULL, 16);
    unsigned long bi = strtoul(b, NULL, 16);
    snprintf(out, outlen, "%s-%s", LAN_ADJ[ai % 32], LAN_ANIMAL[bi % 32]);
}

// mDNS hostname. Prefers the curated slot (ac0, ac1, …) once the backend
// has assigned one — re-read each call so it goes live without a restart.
// Until then, every machine still gets a unique name from its fingerprint.
static void lan_hostname(char *out, size_t outlen) {
    out[0] = 0;
    FILE *f = fopen("/mnt/.ac-device-slot", "r");
    if (f) {
        if (fgets(out, (int)outlen, f)) out[strcspn(out, " \t\r\n")] = 0;
        fclose(f);
    }
    if (out[0]) return;
    pthread_mutex_lock(&g_lan.lock);
    int have_fp = g_lan.fp[0] != 0;
    char fp[24];
    snprintf(fp, sizeof(fp), "%s", g_lan.fp);
    pthread_mutex_unlock(&g_lan.lock);
    if (have_fp) lan_name_from_fp(fp, out, outlen);
    else snprintf(out, outlen, "ac-device");
}

static void lan_snapshot(char *piece, size_t plen, char *ip, size_t iplen) {
    pthread_mutex_lock(&g_lan.lock);
    snprintf(piece, plen, "%s", g_lan.piece);
    snprintf(ip, iplen, "%s", g_lan.ip);
    pthread_mutex_unlock(&g_lan.lock);
}

// Piece/lib filenames: [A-Za-z0-9._-], no leading dot, no "..".
static int lan_name_ok(const char *n) {
    if (!n[0] || n[0] == '.') return 0;
    if (strstr(n, "..")) return 0;
    for (const char *p = n; *p; p++)
        if (!isalnum((unsigned char)*p) && *p != '.' && *p != '_' && *p != '-')
            return 0;
    return 1;
}

static void lan_trigger_jump(const char *piece) {
    FILE *f = fopen("/tmp/ac-jump", "w");
    if (f) {
        fprintf(f, "%s\n", piece);
        fclose(f);
    }
}

// ── HTTP ───────────────────────────────────────────────────────────────

static int http_listen(int *port_out) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    struct sockaddr_in a = {0};
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_ANY);
    int ports[2] = {LANSERV_HTTP_PORT, LANSERV_HTTP_PORT_FALLBACK};
    for (int i = 0; i < 2; i++) {
        a.sin_port = htons((uint16_t)ports[i]);
        if (bind(fd, (struct sockaddr *)&a, sizeof(a)) == 0) {
            if (listen(fd, 8) == 0) {
                *port_out = ports[i];
                return fd;
            }
            break;
        }
    }
    close(fd);
    return -1;
}

static void http_send(int fd, int code, const char *status,
                      const char *ctype, const char *body, size_t blen) {
    char head[256];
    int hl = snprintf(head, sizeof(head),
                      "HTTP/1.1 %d %s\r\nContent-Type: %s\r\n"
                      "Content-Length: %zu\r\nConnection: close\r\n"
                      "Access-Control-Allow-Origin: *\r\n\r\n",
                      code, status, ctype, blen);
    send(fd, head, (size_t)hl, MSG_NOSIGNAL);
    if (body && blen) send(fd, body, blen, MSG_NOSIGNAL);
}

static void http_send_text(int fd, int code, const char *status, const char *msg) {
    http_send(fd, code, status, "text/plain", msg, strlen(msg));
}

static void http_serve_file(int fd, const char *path, const char *ctype) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        http_send_text(fd, 404, "Not Found", "no such file\n");
        return;
    }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char head[256];
    int hl = snprintf(head, sizeof(head),
                      "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n"
                      "Content-Length: %ld\r\nConnection: close\r\n\r\n",
                      ctype, sz);
    send(fd, head, (size_t)hl, MSG_NOSIGNAL);
    char buf[16384];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), f)) > 0)
        if (send(fd, buf, n, MSG_NOSIGNAL) < 0) break;
    fclose(f);
}

// Tail the newest runtime log (last 64KB).
static void http_serve_logs(int fd) {
    const char *cands[] = {"/mnt/cage-child.log", "/mnt/ac-native.log",
                           "/tmp/ac-native-cage.log"};
    FILE *f = NULL;
    for (int i = 0; i < 3 && !f; i++) f = fopen(cands[i], "rb");
    if (!f) {
        http_send_text(fd, 404, "Not Found", "no log file\n");
        return;
    }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    long off = sz > 65536 ? sz - 65536 : 0;
    fseek(f, off, SEEK_SET);
    char *buf = malloc((size_t)(sz - off) + 1);
    size_t n = buf ? fread(buf, 1, (size_t)(sz - off), f) : 0;
    fclose(f);
    http_send(fd, 200, "OK", "text/plain", buf ? buf : "", n);
    free(buf);
}

// Receive `remain` body bytes (after `pre`/`prelen` already read) into a
// temp file next to `dest`, then rename into place so loads never see a
// half-written piece.
static int http_save_body(int fd, const char *dest, const char *pre,
                          size_t prelen, long remain) {
    char tmp[300];
    snprintf(tmp, sizeof(tmp), "%s.tmp", dest);
    FILE *f = fopen(tmp, "wb");
    if (!f) return -1;
    if (prelen) fwrite(pre, 1, prelen, f);
    char buf[16384];
    while (remain > 0) {
        ssize_t n = recv(fd, buf, remain < (long)sizeof(buf) ? (size_t)remain : sizeof(buf), 0);
        if (n <= 0) {
            fclose(f);
            unlink(tmp);
            return -1;
        }
        fwrite(buf, 1, (size_t)n, f);
        remain -= n;
    }
    fclose(f);
    if (rename(tmp, dest) != 0) {
        unlink(tmp);
        return -1;
    }
    return 0;
}

static void http_handle(int fd) {
    struct timeval tv = {5, 0};
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

    char req[8192];
    size_t got = 0;
    char *hdr_end = NULL;
    while (got < sizeof(req) - 1) {
        ssize_t n = recv(fd, req + got, sizeof(req) - 1 - got, 0);
        if (n <= 0) return;
        got += (size_t)n;
        req[got] = 0;
        if ((hdr_end = strstr(req, "\r\n\r\n"))) break;
    }
    if (!hdr_end) {
        http_send_text(fd, 400, "Bad Request", "headers too large\n");
        return;
    }
    hdr_end += 4;

    char method[8] = "", target[512] = "";
    if (sscanf(req, "%7s %511s", method, target) != 2) return;

    // Split query string.
    char *query = strchr(target, '?');
    if (query) *query++ = 0;
    int want_jump = query && strstr(query, "jump=1") != NULL;

    long clen = 0;
    char *cl = strcasestr(req, "Content-Length:");
    if (cl) clen = strtol(cl + 15, NULL, 10);
    if (clen < 0 || clen > LANSERV_MAX_BODY) {
        http_send_text(fd, 413, "Payload Too Large", "body too large\n");
        return;
    }

    int is_get = strcmp(method, "GET") == 0;
    int is_put = strcmp(method, "PUT") == 0 || strcmp(method, "POST") == 0;

    if (is_get && (strcmp(target, "/") == 0 || strcmp(target, "/status") == 0)) {
        char piece[64], ip[16], host[64], body[512];
        lan_snapshot(piece, sizeof(piece), ip, sizeof(ip));
        lan_hostname(host, sizeof(host));
        int blen = snprintf(body, sizeof(body),
                            "{\"name\":\"%s\",\"build\":\"%s\",\"piece\":\"%s\","
                            "\"ip\":\"%s\",\"uptime\":%ld,\"port\":%d}\n",
                            host, g_lan.build, piece, ip,
                            (long)(time(NULL) - g_lan.started), g_lan.http_port);
        http_send(fd, 200, "OK", "application/json", body, (size_t)blen);
        return;
    }

    if (is_get && strcmp(target, "/logs") == 0) {
        http_serve_logs(fd);
        return;
    }

    // /pieces/<n> and /lib/<n>
    const char *dir = NULL, *name = NULL;
    if (strncmp(target, "/pieces/", 8) == 0) { dir = "/pieces"; name = target + 8; }
    else if (strncmp(target, "/lib/", 5) == 0) { dir = "/lib"; name = target + 5; }

    if (dir) {
        if (!lan_name_ok(name)) {
            http_send_text(fd, 400, "Bad Request", "bad name\n");
            return;
        }
        char path[300];
        snprintf(path, sizeof(path), "%s/%s", dir, name);
        if (is_get) {
            http_serve_file(fd, path, "text/javascript");
            return;
        }
        if (is_put) {
            size_t prelen = got - (size_t)(hdr_end - req);
            if ((long)prelen > clen) prelen = (size_t)clen;
            if (http_save_body(fd, path, hdr_end, prelen, clen - (long)prelen) != 0) {
                http_send_text(fd, 500, "Internal Server Error", "write failed\n");
                return;
            }
            ac_log("[lanserv] wrote %s (%ld bytes)%s\n", path, clen,
                   want_jump ? " + jump" : "");
            if (want_jump && dir[1] == 'p') {
                char base[128];
                snprintf(base, sizeof(base), "%s", name);
                char *dot = strrchr(base, '.');
                if (dot) *dot = 0;
                lan_trigger_jump(base);
            }
            http_send_text(fd, 200, "OK", "saved\n");
            return;
        }
    }

    if (strncmp(target, "/jump/", 6) == 0 && !is_get) {
        const char *p = target + 6;
        if (!lan_name_ok(p)) {
            http_send_text(fd, 400, "Bad Request", "bad name\n");
            return;
        }
        lan_trigger_jump(p);
        ac_log("[lanserv] jump requested → %s\n", p);
        http_send_text(fd, 200, "OK", "jumping\n");
        return;
    }

    http_send_text(fd, 404, "Not Found", "unknown route\n");
}

// ── mDNS / DNS-SD ──────────────────────────────────────────────────────
// Minimal RFC 6762 responder: answers A for <host>.local, PTR/SRV/TXT for
// <host>._http._tcp.local, and the service-enumeration meta-query — enough
// for `ping host.local` and printer-style discovery in Bonjour browsers.

static int mdns_open(void) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) return -1;
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
#ifdef SO_REUSEPORT
    setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &one, sizeof(one));
#endif
    struct sockaddr_in a = {0};
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_ANY);
    a.sin_port = htons(MDNS_PORT);
    if (bind(fd, (struct sockaddr *)&a, sizeof(a)) < 0) {
        close(fd);
        return -1;
    }
    // loop=1 so a same-host mDNSResponder sees our answers (lets the dev
    // harness verify discovery locally); the device ignores its own
    // packets — responses are dropped by the QR-bit check in mdns_handle.
    unsigned char ttl = 255, loop = 1;
    setsockopt(fd, IPPROTO_IP, IP_MULTICAST_TTL, &ttl, sizeof(ttl));
    setsockopt(fd, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop));
    return fd;
}

static void mdns_join(int fd) {
    struct ip_mreq m = {0};
    m.imr_multiaddr.s_addr = inet_addr(MDNS_GROUP);
    m.imr_interface.s_addr = htonl(INADDR_ANY);
    // Drop first so a rejoin after an IP change doesn't EADDRINUSE.
    setsockopt(fd, IPPROTO_IP, IP_DROP_MEMBERSHIP, &m, sizeof(m));
    setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &m, sizeof(m));
}

// Append dot-separated `name` as DNS labels. Returns new offset, -1 on overflow.
static int dn_put_name(uint8_t *buf, int off, int cap, const char *name) {
    const char *p = name;
    while (*p) {
        const char *dot = strchr(p, '.');
        int len = dot ? (int)(dot - p) : (int)strlen(p);
        if (len < 1 || len > 63 || off + len + 1 >= cap) return -1;
        buf[off++] = (uint8_t)len;
        memcpy(buf + off, p, (size_t)len);
        off += len;
        p += len;
        if (*p == '.') p++;
    }
    if (off + 1 >= cap) return -1;
    buf[off++] = 0;
    return off;
}

static int dn_put_u16(uint8_t *buf, int off, int cap, uint16_t v) {
    if (off + 2 > cap) return -1;
    buf[off] = (uint8_t)(v >> 8);
    buf[off + 1] = (uint8_t)v;
    return off + 2;
}

static int dn_put_u32(uint8_t *buf, int off, int cap, uint32_t v) {
    if (off + 4 > cap) return -1;
    buf[off] = (uint8_t)(v >> 24);
    buf[off + 1] = (uint8_t)(v >> 16);
    buf[off + 2] = (uint8_t)(v >> 8);
    buf[off + 3] = (uint8_t)v;
    return off + 4;
}

// Record header: name, type, class, ttl, then caller writes RDLENGTH+RDATA.
// `flush` sets the cache-flush bit (multicast responses only).
static int dn_put_rr_head(uint8_t *buf, int off, int cap, const char *name,
                          uint16_t type, int flush, uint32_t ttl) {
    off = dn_put_name(buf, off, cap, name);
    if (off < 0) return -1;
    off = dn_put_u16(buf, off, cap, type);
    if (off < 0) return -1;
    off = dn_put_u16(buf, off, cap, flush ? 0x8001 : 0x0001);
    if (off < 0) return -1;
    return dn_put_u32(buf, off, cap, ttl);
}

static int dn_put_a(uint8_t *buf, int off, int cap, const char *name,
                    uint32_t ip_be, int flush) {
    off = dn_put_rr_head(buf, off, cap, name, 1 /*A*/, flush, MDNS_TTL);
    if (off < 0) return -1;
    off = dn_put_u16(buf, off, cap, 4);
    if (off < 0 || off + 4 > cap) return -1;
    memcpy(buf + off, &ip_be, 4);
    return off + 4;
}

static int dn_put_ptr(uint8_t *buf, int off, int cap, const char *name,
                      const char *target) {
    off = dn_put_rr_head(buf, off, cap, name, 12 /*PTR*/, 0, MDNS_TTL);
    if (off < 0) return -1;
    int rdlen_at = off;
    off = dn_put_u16(buf, off, cap, 0);
    if (off < 0) return -1;
    int start = off;
    off = dn_put_name(buf, off, cap, target);
    if (off < 0) return -1;
    buf[rdlen_at] = (uint8_t)((off - start) >> 8);
    buf[rdlen_at + 1] = (uint8_t)(off - start);
    return off;
}

static int dn_put_srv(uint8_t *buf, int off, int cap, const char *name,
                      uint16_t port, const char *target, int flush) {
    off = dn_put_rr_head(buf, off, cap, name, 33 /*SRV*/, flush, MDNS_TTL);
    if (off < 0) return -1;
    int rdlen_at = off;
    off = dn_put_u16(buf, off, cap, 0);
    if (off < 0) return -1;
    int start = off;
    off = dn_put_u16(buf, off, cap, 0); // priority
    off = off < 0 ? -1 : dn_put_u16(buf, off, cap, 0); // weight
    off = off < 0 ? -1 : dn_put_u16(buf, off, cap, port);
    off = off < 0 ? -1 : dn_put_name(buf, off, cap, target);
    if (off < 0) return -1;
    buf[rdlen_at] = (uint8_t)((off - start) >> 8);
    buf[rdlen_at + 1] = (uint8_t)(off - start);
    return off;
}

static int dn_put_txt(uint8_t *buf, int off, int cap, const char *name,
                      const char *kv, int flush) {
    off = dn_put_rr_head(buf, off, cap, name, 16 /*TXT*/, flush, MDNS_TTL);
    if (off < 0) return -1;
    int len = (int)strlen(kv);
    if (len > 255) len = 255;
    off = dn_put_u16(buf, off, cap, (uint16_t)(len + 1));
    if (off < 0 || off + len + 1 > cap) return -1;
    buf[off++] = (uint8_t)len;
    memcpy(buf + off, kv, (size_t)len);
    return off + len;
}

// Read a (possibly compressed) name from a query packet into dot form.
static int dn_read_name(const uint8_t *pkt, int plen, int off, char *out,
                        int outlen) {
    int o = 0, hops = 0;
    while (off < plen) {
        uint8_t l = pkt[off];
        if (l == 0) {
            out[o] = 0;
            return off + 1; // only meaningful pre-pointer; callers track separately
        }
        if ((l & 0xC0) == 0xC0) {
            if (off + 1 >= plen || ++hops > 4) return -1;
            off = ((l & 0x3F) << 8) | pkt[off + 1];
            continue;
        }
        if (off + 1 + l > plen || o + l + 2 > outlen) return -1;
        if (o) out[o++] = '.';
        for (int i = 0; i < l; i++)
            out[o++] = (char)tolower(pkt[off + 1 + i]);
        off += 1 + l;
    }
    return -1;
}

// Skip past a name in the packet (without following pointers).
static int dn_skip_name(const uint8_t *pkt, int plen, int off) {
    while (off < plen) {
        uint8_t l = pkt[off];
        if (l == 0) return off + 1;
        if ((l & 0xC0) == 0xC0) return off + 2;
        off += 1 + l;
    }
    return -1;
}

typedef struct {
    char host[80];     // "ac0.local"
    char svc[96];      // "_http._tcp.local"
    char inst[160];    // "ac0._http._tcp.local"
    uint32_t ip_be;
    uint16_t port;
    char txt[80];      // "build=..."
} MdnsIdentity;

static void mdns_identity(MdnsIdentity *id) {
    char host[64], piece[64], ip[16];
    lan_hostname(host, sizeof(host));
    lan_snapshot(piece, sizeof(piece), ip, sizeof(ip));
    (void)piece;
    snprintf(id->host, sizeof(id->host), "%s.local", host);
    snprintf(id->svc, sizeof(id->svc), "_http._tcp.local");
    snprintf(id->inst, sizeof(id->inst), "%s._http._tcp.local", host);
    id->ip_be = ip[0] ? inet_addr(ip) : 0;
    id->port = (uint16_t)g_lan.http_port;
    snprintf(id->txt, sizeof(id->txt), "build=%s", g_lan.build);
}

// Unsolicited announcement: A + service PTR + SRV + TXT, multicast.
static void mdns_announce(int fd) {
    MdnsIdentity id;
    mdns_identity(&id);
    if (!id.ip_be) return;
    uint8_t pkt[768];
    memset(pkt, 0, 12);
    pkt[2] = 0x84; // QR=1 AA=1
    pkt[7] = 4;    // ANCOUNT
    int off = 12;
    off = dn_put_a(pkt, off, (int)sizeof(pkt), id.host, id.ip_be, 1);
    if (off > 0) off = dn_put_ptr(pkt, off, (int)sizeof(pkt), id.svc, id.inst);
    if (off > 0) off = dn_put_srv(pkt, off, (int)sizeof(pkt), id.inst, id.port, id.host, 1);
    if (off > 0) off = dn_put_txt(pkt, off, (int)sizeof(pkt), id.inst, id.txt, 1);
    if (off < 0) return;
    struct sockaddr_in dst = {0};
    dst.sin_family = AF_INET;
    dst.sin_addr.s_addr = inet_addr(MDNS_GROUP);
    dst.sin_port = htons(MDNS_PORT);
    sendto(fd, pkt, (size_t)off, 0, (struct sockaddr *)&dst, sizeof(dst));
}

static void mdns_handle(int fd) {
    uint8_t pkt[1500];
    struct sockaddr_in src;
    socklen_t slen = sizeof(src);
    ssize_t n = recvfrom(fd, pkt, sizeof(pkt), 0, (struct sockaddr *)&src, &slen);
    if (n < 12) return;
    if (pkt[2] & 0x80) return; // response, not query
    int qd = (pkt[4] << 8) | pkt[5];
    if (qd < 1 || qd > 16) return;

    MdnsIdentity id;
    mdns_identity(&id);
    if (!id.ip_be) return;

    // Legacy unicast queries (source port != 5353) get a unicast reply
    // echoing the query ID, without cache-flush bits (RFC 6762 §6.7).
    int legacy = ntohs(src.sin_port) != MDNS_PORT;

    uint8_t out[1024];
    memset(out, 0, 12);
    if (legacy) {
        out[0] = pkt[0];
        out[1] = pkt[1];
    }
    out[2] = 0x84;
    int off = 12, answers = 0, extras = 0;

    int qoff = 12;
    char qname[256];
    for (int i = 0; i < qd && qoff > 0 && qoff < (int)n; i++) {
        if (dn_read_name(pkt, (int)n, qoff, qname, sizeof(qname)) < 0) return;
        qoff = dn_skip_name(pkt, (int)n, qoff);
        if (qoff < 0 || qoff + 4 > (int)n) return;
        uint16_t qtype = (uint16_t)((pkt[qoff] << 8) | pkt[qoff + 1]);
        qoff += 4;
        int flush = legacy ? 0 : 1;

        if (strcmp(qname, id.host) == 0 && (qtype == 1 || qtype == 255)) {
            off = dn_put_a(out, off, (int)sizeof(out), id.host, id.ip_be, flush);
            answers++;
        } else if (strcmp(qname, id.svc) == 0 && (qtype == 12 || qtype == 255)) {
            off = dn_put_ptr(out, off, (int)sizeof(out), id.svc, id.inst);
            answers++;
            if (off > 0) off = dn_put_srv(out, off, (int)sizeof(out), id.inst, id.port, id.host, flush);
            if (off > 0) off = dn_put_txt(out, off, (int)sizeof(out), id.inst, id.txt, flush);
            if (off > 0) off = dn_put_a(out, off, (int)sizeof(out), id.host, id.ip_be, flush);
            extras += 3;
        } else if (strcmp(qname, id.inst) == 0 &&
                   (qtype == 33 || qtype == 16 || qtype == 255)) {
            off = dn_put_srv(out, off, (int)sizeof(out), id.inst, id.port, id.host, flush);
            answers++;
            if (off > 0) off = dn_put_txt(out, off, (int)sizeof(out), id.inst, id.txt, flush);
            answers++;
            if (off > 0) off = dn_put_a(out, off, (int)sizeof(out), id.host, id.ip_be, flush);
            extras++;
        } else if (strcmp(qname, "_services._dns-sd._udp.local") == 0 && qtype == 12) {
            off = dn_put_ptr(out, off, (int)sizeof(out), qname, id.svc);
            answers++;
        }
        if (off < 0) return;
    }
    if (!answers) return;
    out[6] = (uint8_t)(answers >> 8);
    out[7] = (uint8_t)answers;
    out[10] = (uint8_t)(extras >> 8);
    out[11] = (uint8_t)extras;

    struct sockaddr_in dst = src;
    if (!legacy) {
        dst.sin_addr.s_addr = inet_addr(MDNS_GROUP);
        dst.sin_port = htons(MDNS_PORT);
    }
    sendto(fd, out, (size_t)off, 0, (struct sockaddr *)&dst, sizeof(dst));
}

// ── thread ─────────────────────────────────────────────────────────────

static void *lanserv_thread(void *arg) {
    (void)arg;
    int http_fd = http_listen(&g_lan.http_port);
    int mdns_fd = mdns_open();
    if (http_fd < 0) ac_log("[lanserv] http bind failed: %s\n", strerror(errno));
    else ac_log("[lanserv] http listening on :%d\n", g_lan.http_port);
    if (mdns_fd < 0) ac_log("[lanserv] mdns bind failed: %s\n", strerror(errno));

    int announced = 0;
    char announced_name[64] = "";
    int name_check = 0;
    while (g_lan.running) {
        if (g_lan.ip_changed && mdns_fd >= 0) {
            g_lan.ip_changed = 0;
            mdns_join(mdns_fd);
            // Announce twice ~1s apart per RFC 6762 probing/announcing guidance.
            mdns_announce(mdns_fd);
            announced = 1;
            lan_hostname(announced_name, sizeof(announced_name));
            ac_log("[lanserv] mdns announcing %s.local\n", announced_name);
        }

        fd_set rd;
        FD_ZERO(&rd);
        int maxfd = -1;
        if (http_fd >= 0) { FD_SET(http_fd, &rd); maxfd = http_fd; }
        if (mdns_fd >= 0) { FD_SET(mdns_fd, &rd); if (mdns_fd > maxfd) maxfd = mdns_fd; }
        if (maxfd < 0) break;
        struct timeval tv = {1, 0};
        int r = select(maxfd + 1, &rd, NULL, NULL, &tv);
        if (r < 0) {
            if (errno == EINTR) continue;
            break;
        }
        if (r == 0) {
            // Second announcement one tick after the first.
            if (announced == 1 && mdns_fd >= 0) {
                mdns_announce(mdns_fd);
                announced = 2;
            }
            // Re-announce when the resolved name changes — the first announce
            // can fire before the fingerprint is set or the curated slot
            // (ac0) is fetched, leaving us on the "ac-device" fallback. Check
            // every ~3s and re-announce under the new name once it resolves.
            if (mdns_fd >= 0 && ++name_check >= 3) {
                name_check = 0;
                char now_name[64], ip[16], piece[64];
                lan_hostname(now_name, sizeof(now_name));
                lan_snapshot(piece, sizeof(piece), ip, sizeof(ip));
                if (ip[0] && strcmp(now_name, announced_name) != 0) {
                    snprintf(announced_name, sizeof(announced_name), "%s", now_name);
                    mdns_announce(mdns_fd);
                    ac_log("[lanserv] mdns re-announcing %s.local (name resolved)\n",
                           now_name);
                }
            }
            continue;
        }
        if (http_fd >= 0 && FD_ISSET(http_fd, &rd)) {
            int cfd = accept(http_fd, NULL, NULL);
            if (cfd >= 0) {
                http_handle(cfd);
                close(cfd);
            }
        }
        if (mdns_fd >= 0 && FD_ISSET(mdns_fd, &rd)) mdns_handle(mdns_fd);
    }
    if (http_fd >= 0) close(http_fd);
    if (mdns_fd >= 0) close(mdns_fd);
    return NULL;
}

// ── public api ─────────────────────────────────────────────────────────

void lanserv_start(const char *build_name) {
    if (g_lan.running) return;
    pthread_mutex_init(&g_lan.lock, NULL);
    snprintf(g_lan.build, sizeof(g_lan.build), "%s",
             build_name && build_name[0] ? build_name : "dev");
    g_lan.started = time(NULL);
    g_lan.running = 1;
    if (pthread_create(&g_lan.thread, NULL, lanserv_thread, NULL) != 0) {
        g_lan.running = 0;
        ac_log("[lanserv] thread create failed\n");
    }
}

void lanserv_set_fingerprint(const char *fp) {
    if (!g_lan.running || !fp) return;
    pthread_mutex_lock(&g_lan.lock);
    snprintf(g_lan.fp, sizeof(g_lan.fp), "%s", fp);
    pthread_mutex_unlock(&g_lan.lock);
}

void lanserv_update(const char *piece, const char *ip) {
    if (!g_lan.running) return;
    pthread_mutex_lock(&g_lan.lock);
    if (piece && strcmp(g_lan.piece, piece) != 0)
        snprintf(g_lan.piece, sizeof(g_lan.piece), "%s", piece);
    const char *want = ip ? ip : "";
    if (strcmp(g_lan.ip, want) != 0) {
        snprintf(g_lan.ip, sizeof(g_lan.ip), "%s", want);
        if (want[0]) g_lan.ip_changed = 1;
    }
    pthread_mutex_unlock(&g_lan.lock);
}

void lanserv_stop(void) {
    if (!g_lan.running) return;
    g_lan.running = 0;
    pthread_join(g_lan.thread, NULL);
}
