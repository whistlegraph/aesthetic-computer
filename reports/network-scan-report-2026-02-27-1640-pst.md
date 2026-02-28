# Network Scan Report

**Date:** 2026-02-27 4:40 PM PST
**Scanner:** nmap 7.92 (container) + nmap 7.98 (macOS host)
**Container:** `aesthetic` devcontainer (Fedora 43, Linux 6.12.69-linuxkit)
**Host:** jas-mbp.local (macOS, OpenSSH 10.2)

---

## Network Topology

```
┌───────────────────────────────────────────────────────────────────┐
│                    WiFi / LAN (10.204.0.0/16)                     │
│         Enterprise/managed network with client isolation          │
│                                                                   │
│  ┌──────────────┐                      ┌───────────────────────┐  │
│  │ Gateway      │                      │ MacBook (jas-mbp)     │  │
│  │ 10.204.0.1   │◄────── en0 ─────────►│ 10.204.122.246        │  │
│  │ DNS:53 HTTP:80│                      │ WiFi, active          │  │
│  └──────────────┘                      └───────────┬───────────┘  │
│                                                    │              │
│        (No other LAN hosts visible —               │              │
│         client isolation enabled)                   │              │
└────────────────────────────────────────────────────│──────────────┘
                                                     │
                              ┌───────────────────────┘
                              │  Docker Desktop VM
                              │  (LinuxKit hypervisor)
                              │
              ┌───────────────┴───────────────────────────────┐
              │        192.168.65.0/24 (Docker Desktop)        │
              │                                                │
              │  .254  host.docker.internal (Mac host)         │
              │        SSH:22  Kerberos:88  SMB:445  VNC:5900  │
              │        AC:8888  Session:8889                   │
              │                                                │
              │  .7    DNS resolver (port 53)                  │
              │  .129  HTTP service (port 80)                  │
              │  .3    Docker internal (no ports)              │
              │  .6    Docker internal (no ports)              │
              └───────────────┬───────────────────────────────┘
                              │
              ┌───────────────┴───────────────────────────────┐
              │        172.17.0.0/16 (Docker bridge)           │
              │                                                │
              │  .0.1  VM gateway (rpcbind:111)                │
              │  .0.2  Unknown container (no ports)            │
              │  .0.3  THIS CONTAINER (aesthetic)              │
              │        14 open ports (see below)               │
              └───────────────────────────────────────────────┘
```

---

## 1. MacBook Host — `host.docker.internal` / `192.168.65.254`

**OS:** macOS (Apple Mac OS X)
**Hostname:** jas-mbp.local
**WiFi IP:** 10.204.122.246/16 on en0
**Network:** Enterprise/managed (client isolation, /16 subnet, no SSID reported)

### External-Facing Ports (from container via 192.168.65.254)

| Port | State | Service | Version |
|------|-------|---------|---------|
| 22 | **open** | SSH | OpenSSH 10.2 |
| 88 | **open** | Kerberos | Heimdal Kerberos |
| 445 | **open** | SMB | microsoft-ds |
| 5900 | **open** | VNC | Apple Remote Desktop |
| 8888 | **open** | HTTPS | AC dev server (Netlify) |
| 8889 | **open** | HTTPS | AC session server |

### Localhost Services (from Mac self-scan, 127.0.0.1)

| Port | Service | Process |
|------|---------|---------|
| 22 | SSH | sshd |
| 88 | Kerberos | macOS directory services |
| 445 | SMB | macOS file sharing |
| 3003 | Silo | VS Code port forward |
| 3478 | STUN/TURN | VS Code port forward |
| 4040 | ngrok UI | VS Code port forward |
| 4177 | AT User Pages | VS Code port forward |
| 5000 | AirPlay | ControlCenter |
| 5555 | Extension Views | VS Code port forward |
| 5900 | VNC | Screen Sharing |
| 6463 | Discord RPC | Discord |
| 7000 | AirPlay | ControlCenter |
| 7890 | Status dashboard | VS Code port forward |
| 8021 | FTP proxy | Unknown |
| 8083 | Chat System | VS Code port forward |
| 8084 | Chat SOTCE | VS Code port forward |
| 8085 | Chat Clock | VS Code port forward |
| 8111 | Caddy | VS Code port forward |
| 8112 | Caddy alt | VS Code port forward |
| 8888 | AC dev server | VS Code port forward |
| 8889 | Session server | VS Code port forward |
| 9333 | Electron | VS Code (litecoin port) |
| 10000-10007 | WebRTC media | VS Code port forward |
| 43585 | VS Code helper | Code Helper |
| 61463 | Rapport | rapportd (Nearby sharing) |
| 50818-50819 | Rapport | rapportd (Nearby sharing) |

**23 open ports on localhost**, mostly VS Code forwarding container ports.

---

## 2. WiFi LAN — `10.204.0.0/16`

| Host | Status | Notes |
|------|--------|-------|
| 10.204.0.1 (gateway) | up | DNS:53 open, HTTP:80 open, SSH/HTTPS/8080/8443 filtered |
| 10.204.122.246 (self) | up | MacBook |
| Everything else | down | **Client isolation is active** |

The network uses a `/16` subnet (65,534 possible hosts) but only the gateway and the Mac itself are visible. This is characteristic of enterprise WiFi with AP isolation / private VLAN — each client can only see the gateway, not other clients.

---

## 3. Docker Desktop Internal — `192.168.65.0/24`

| IP | Role | Open Ports |
|----|------|------------|
| 192.168.65.254 | Mac host (host.docker.internal) | SSH:22, Kerberos:88, SMB:445, VNC:5900, AC:8888, Session:8889 |
| 192.168.65.7 | DNS resolver | DNS:53 |
| 192.168.65.129 | Internal HTTP service | HTTP:80 |
| 192.168.65.3 | Docker internal | none |
| 192.168.65.6 | Docker internal | none |

---

## 4. Docker Bridge — `172.17.0.0/16`

| IP | Role | Open Ports |
|----|------|------------|
| 172.17.0.1 | VM gateway | rpcbind:111 |
| 172.17.0.2 | Unknown container | none |
| 172.17.0.3 | **aesthetic** (this container) | 14 ports (see below) |

---

## 5. This Container — `172.17.0.3` (aesthetic)

Full 65535-port scan results:

| Port | Service | Purpose |
|------|---------|---------|
| 3002 | Unknown | Dev tool / process |
| 5555 | Extension Views | VS Code extension dev server |
| 6379 | Redis | Session state / cache |
| 7890 | Dashboard | Devcontainer status |
| 8083 | Chat System | AC chat endpoint |
| 8084 | Chat SOTCE | AC chat endpoint |
| 8085 | Chat Clock | AC chat endpoint |
| 8111 | Caddy | SSL reverse proxy |
| 8888 | Netlify Dev | Main AC dev server |
| 8889 | Session Server | Multiplayer state sync |
| 34291 | Ephemeral | Node.js dev process |
| 37417 | Ephemeral | Node.js dev process |
| 42815 | Ephemeral | Likely Vite HMR / module loader |
| 45603 | Ephemeral | Node.js dev process |

All expected services for AC development.

---

## Security Observations

### Exposure from Container
- **SSH to Mac is trivial**: `ssh jas@host.docker.internal` works with no password prompt (key-based auth)
- VNC (Screen Sharing) is accessible from the container
- SMB (File Sharing) is accessible from the container
- All forwarded ports are bound to `127.0.0.1` on the Mac (good — not exposed to LAN)

### WiFi Network
- Client isolation prevents lateral movement on the LAN — only the gateway is reachable
- The gateway has HTTP:80 open (likely a captive portal or management interface)
- Not a home network — `machines.json` has `192.168.1.127` for the MacBook but current IP is `10.204.122.246`

### Container
- Redis (6379) bound to all interfaces, not just localhost — acceptable for dev but would be a concern in production
- 4 ephemeral high ports open (Node.js dev processes)

---

## machines.json vs Reality

| Machine | Listed IP | Actual | Reachable? |
|---------|-----------|--------|------------|
| jeffrey-macbook | 192.168.1.127 | 10.204.122.246 | Yes (via host.docker.internal) |
| jas-fedora | 192.168.1.191 | ? | Not on this network |
| x1-nano-g2 | 192.168.1.178 | ? | Not on this network |
| ff1-dvveklza | 192.168.1.82 | ? | Not on this network |
| mac-mini | 192.168.12.27 | ? | Not on this network |

The `192.168.1.x` IPs in `machines.json` are for a home network. You're currently on an enterprise/public `10.204.0.0/16` network with client isolation.

---

## SSH Punch-Through Summary

The container-to-Mac SSH tunnel is working perfectly:

```
Container (172.17.0.3)
  └── ssh jas@host.docker.internal ──► MacBook (jas-mbp.local)
                                          └── 10.204.122.246 (WiFi, isolated)
```

This is the cleanest path. No need for `--network=host`.
