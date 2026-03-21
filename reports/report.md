# Network Scan Report

**Date:** 2026-02-28 00:23 UTC (2026-02-27 4:23 PM PST)
**Scanner:** nmap 7.92 from inside `aesthetic` devcontainer
**Host OS:** Linux 6.12.69-linuxkit (Docker Desktop for Mac)

---

## Network Topology

```
                        ┌──────────────────────────┐
                        │   MacBook (macOS host)    │
                        │   192.168.65.254          │
                        │   (host.docker.internal)  │
                        │                           │
                        │   OS: Mac OS X            │
                        │   SSH: OpenSSH 10.2       │
                        └─────────┬────────────────┘
                                  │
              ┌───────────────────┼───────────────────┐
              │                   │                   │
    ┌─────────┴──────┐  ┌────────┴───────┐  ┌───────┴────────┐
    │ Docker Desktop │  │ DNS Resolver   │  │ HTTP Service   │
    │ VM Gateway     │  │ 192.168.65.7   │  │ 192.168.65.129 │
    │ 172.17.0.1     │  │ port 53/tcp    │  │ port 80/tcp    │
    │ port 111/tcp   │  └────────────────┘  └────────────────┘
    │ (rpcbind)      │
    └────────┬───────┘
             │
    ┌────────┴───────────────────────┐
    │        Docker Bridge           │
    │        172.17.0.0/16           │
    │                                │
    │  ┌──────────┐  ┌────────────┐  │
    │  │ .0.2     │  │ .0.3       │  │
    │  │ (empty)  │  │ aesthetic  │  │
    │  │ no ports │  │ container  │  │
    │  └──────────┘  │ (this box) │  │
    │                └────────────┘  │
    └────────────────────────────────┘
```

---

## Hosts Discovered

### 1. MacBook Host — `192.168.65.254` (host.docker.internal)

The main host machine. Identified as **Mac OS X** by nmap.

| Port | State | Service | Version |
|------|-------|---------|---------|
| 22 | **open** | SSH | OpenSSH 10.2 (protocol 2.0) |
| 88 | **open** | Kerberos | Heimdal Kerberos |
| 445 | **open** | SMB | microsoft-ds |
| 5900 | **open** | VNC | Apple Remote Desktop |
| 8888 | **open** | HTTPS | Netlify dev server (AC site) |
| 8889 | **open** | HTTPS | Session server |

**Key findings:**
- SSH is reachable from the container via `ssh 192.168.65.254` (or `ssh host.docker.internal`)
- VNC/Screen Sharing is enabled (port 5900)
- Kerberos and SMB are running (standard macOS directory services)
- Ports 8888 and 8889 are the AC dev server and session server, forwarded through Docker Desktop

### 2. Docker VM Gateway — `172.17.0.1`

| Port | State | Service |
|------|-------|---------|
| 111 | **open** | rpcbind |

Minimal exposure. This is the LinuxKit VM that Docker Desktop runs containers in.

### 3. DNS Resolver — `192.168.65.7`

| Port | State | Service |
|------|-------|---------|
| 53 | **open** | DNS |

Docker Desktop's internal DNS resolver. Listed in `/etc/resolv.conf`.

### 4. HTTP Service — `192.168.65.129`

| Port | State | Service |
|------|-------|---------|
| 80 | **open** | HTTP |

Unknown service — possibly a Docker Desktop internal API or dashboard.

### 5. Unknown Host — `192.168.65.3`

No open ports on common scanned range. Possibly Docker Desktop control plane.

### 6. Unknown Host — `192.168.65.6`

No open ports on common scanned range. Another Docker Desktop internal component.

### 7. Container Peer — `172.17.0.2`

No open ports detected. May be a sidecar or stopped container.

---

## This Container — `172.17.0.3` (aesthetic)

Full 65535-port scan of ourselves:

| Port | State | Service (likely) |
|------|-------|-----------------|
| 3002 | open | Unknown (possibly a dev tool) |
| 5555 | open | VS Code Extension Views dev server |
| 6379 | open | Redis |
| 7890 | open | Devcontainer status dashboard |
| 8083 | open | Chat System |
| 8084 | open | Chat SOTCE |
| 8085 | open | Chat Clock |
| 8111 | open | Caddy reverse proxy |
| 8888 | open | AC dev server (Netlify) |
| 8889 | open | Session server |
| 34291 | open | Ephemeral (likely node/dev process) |
| 37417 | open | Ephemeral (likely node/dev process) |
| 42815 | open | Ephemeral (likely Vite HMR or module loader) |
| 45603 | open | Ephemeral (likely node/dev process) |

**14 ports open** — all expected AC development services.

---

## Can We Reach the Actual WiFi LAN?

**Short answer: Not directly from inside the container.**

The container sits behind Docker Desktop's NAT:
```
Container (172.17.0.3) → Docker Bridge (172.17.0.1) → LinuxKit VM → Docker Desktop NAT → macOS host → WiFi LAN
```

- Scanning `192.168.1.0/24` (common home LAN) shows hosts respond but all ports show as **filtered** — the packets go through Docker Desktop's NAT but responses are unreliable/slow
- `HOST_IP` env var is **not set** in the container, so we don't know the Mac's WiFi LAN IP
- The LAN scan was too slow to complete through the NAT layers

### How to Punch Through

**Option A: SSH tunnel to the Mac (easiest)**
```bash
# From inside the container, SSH to the Mac host:
ssh user@host.docker.internal

# Then scan from there:
nmap -sn 192.168.1.0/24
```

**Option B: Set HOST_IP in devcontainer**
Add to `containerEnv` in devcontainer.json:
```json
"HOST_IP": "192.168.1.XX"  // your Mac's WiFi IP
```
Then you can scan through the Mac as a jump host.

**Option C: Add `--network=host` to runArgs**
```json
"runArgs": [..., "--network=host"]
```
This gives the container the host's full network stack — no NAT. You'd see the WiFi LAN directly. Trade-off: loses Docker's port isolation.

**Option D: macvlan network**
Create a Docker macvlan network that bridges to the host's WiFi interface. Gives the container its own IP on the LAN. Most complex but cleanest separation.

---

## Security Notes

- **SSH (22) is open** on the Mac via `host.docker.internal` — accessible from the container without any extra config
- **VNC (5900) is open** — Screen Sharing is enabled on the Mac
- **SMB (445) is open** — File sharing active
- **Redis (6379)** in the container is bound to all interfaces (not just localhost) — fine for dev, but worth noting
- All AC dev services (8888, 8889, 8083-8085) are running and accessible

---

## Summary

| Network | Hosts Found | Open Services |
|---------|------------|---------------|
| 172.17.0.0/16 (Docker bridge) | 3 | rpcbind, AC stack |
| 192.168.65.0/24 (Docker Desktop) | 5 | SSH, VNC, SMB, Kerberos, DNS, AC forwarded ports |
| WiFi LAN | unreachable | need SSH tunnel or network mode change |

The container can already SSH to the Mac at `host.docker.internal:22`. To scan the actual WiFi LAN, the simplest path is SSH'ing into the Mac first, or switching to `--network=host` mode.
