#pragma once

// LAN dev server — HTTP control endpoint + mDNS/Bonjour responder.
// Runs on its own thread (like wifi). The device answers at
// http://<slot>.local/ (slot from /mnt/.ac-device-slot, else "notepat")
// and advertises _http._tcp via DNS-SD so it shows up in Bonjour
// browsers like a printer.
//
// HTTP routes (port 80):
//   GET  /                      → status JSON (name, build, piece, ip, uptime)
//   GET  /logs                  → tail of the runtime log
//   GET  /pieces/<n>.mjs        → read a piece back
//   PUT  /pieces/<n>.mjs[?jump=1] → write piece (atomic), optionally reload
//   PUT  /lib/<n>.mjs           → write a lib module
//   POST /jump/<n>              → hot-reload piece <n> (writes /tmp/ac-jump)

void lanserv_start(const char *build_name);

// Per-frame from the main loop: current piece + wifi ip ("" when down).
void lanserv_update(const char *piece, const char *ip);

void lanserv_stop(void);
