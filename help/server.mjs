#!/usr/bin/env node
// help - reverse proxy to NanoClaw running on macbook
//
// Proxies all requests to a local port where an SSH reverse tunnel
// from the macbook exposes NanoClaw's HTTP Help channel.
//
// SSH tunnel (run on macbook):
//   ssh -R 3005:localhost:3004 root@help.aesthetic.computer -N -o ServerAliveInterval=30
//
// This proxy: :3004 (Caddy) → :3005 (SSH tunnel → macbook NanoClaw)

import "dotenv/config";
import http from "http";

const PORT = parseInt(process.env.PORT || "3004", 10);
const BACKEND_PORT = parseInt(process.env.BACKEND_PORT || "3005", 10);
const BACKEND_HOST = process.env.BACKEND_HOST || "127.0.0.1";

const server = http.createServer((req, res) => {
  const options = {
    hostname: BACKEND_HOST,
    port: BACKEND_PORT,
    path: req.url,
    method: req.method,
    headers: req.headers,
  };

  const proxy = http.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res, { end: true });
  });

  proxy.on("error", (err) => {
    console.error(`proxy error: ${err.message}`);
    if (!res.headersSent) {
      res.writeHead(502, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "NanoClaw backend unreachable. Is the SSH tunnel running?" }));
    }
  });

  req.pipe(proxy, { end: true });
});

server.listen(PORT, () => {
  console.log(`help proxy listening on :${PORT} → ${BACKEND_HOST}:${BACKEND_PORT}`);
});
