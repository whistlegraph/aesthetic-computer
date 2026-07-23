#!/usr/bin/env node
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { Mediorgan } from "./mediorgan.mjs";
import { StateRepository } from "./repository.mjs";
import { sleepCommit } from "./sleep.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const defaultWebRoot = resolve(here, "../web");
const LOOPBACK = new Set(["127.0.0.1", "::1"]);

function json(response, status, value) {
  const data = Buffer.from(JSON.stringify(value));
  response.writeHead(status, {
    "content-type": "application/json; charset=utf-8",
    "content-length": data.length,
    "cache-control": "no-store",
    "x-content-type-options": "nosniff",
  });
  response.end(data);
}

async function requestBody(request, limit = 8192) {
  const chunks = [];
  let size = 0;
  for await (const chunk of request) {
    size += chunk.length;
    if (size > limit) throw new Error("request body too large");
    chunks.push(chunk);
  }
  return JSON.parse(Buffer.concat(chunks).toString("utf8") || "{}");
}

function publicState(repository) {
  const state = repository.terrarium.state;
  return {
    tick: state.tick,
    lastSeq: state.lastSeq,
    stateHash: repository.stateHash(),
    entities: state.entities.map(({ id, species, x, y, z, energy }) => ({ id, species, x, y, z, energy })),
    visitors: Object.values(state.visitors).map(({ handle, x, y, z }) => ({ handle, x, y, z })),
    organs: ["sensory", "spatial", "drive", "memory", "action", "voice"],
    membrane: "mediorgan",
  };
}

export async function createTerrariumServer({
  root,
  host = "127.0.0.1",
  port = 0,
  capabilities = {},
  tickMs = 100,
  webRoot = defaultWebRoot,
  onTickError = (error) => console.error("terrarium tick failed:", error.message),
} = {}) {
  if (!LOOPBACK.has(host)) throw new Error("terrarium server refuses non-loopback binding");
  let repository;
  try {
    repository = await StateRepository.open(root, { segmentId: "visit" });
  } catch {
    repository = await StateRepository.create(root, { seed: "intelligent-terrarium-visit-v1", profile: "1gb" });
    repository.segmentPath = join(root, "journal", "segments", "visit.ndjson");
  }
  const mediorgan = new Mediorgan(repository, { capabilities });
  const streams = new Set();
  let writeTail = Promise.resolve();

  function exclusive(operation) {
    const result = writeTail.then(operation);
    writeTail = result.catch(() => {});
    return result;
  }

  function broadcast(message) {
    const line = `${JSON.stringify(message)}\n`;
    for (const response of streams) {
      if (!response.write(line)) {
        response.end();
        streams.delete(response);
      }
    }
  }

  async function authenticate(request, response) {
    const handle = mediorgan.authenticate(request.headers.authorization);
    if (!handle) {
      json(response, 401, { error: "valid development capability required" });
      return null;
    }
    await exclusive(() => mediorgan.ensurePresent(handle));
    return handle;
  }

  const server = createServer(async (request, response) => {
    try {
      const url = new URL(request.url, `http://${request.headers.host || "127.0.0.1"}`);
      if (request.method === "GET" && (url.pathname === "/" || url.pathname === "/client.mjs" || url.pathname === "/spatial-audio.mjs")) {
        const filename = url.pathname === "/" ? "index.html" : url.pathname.slice(1);
        const content = await readFile(join(webRoot, filename));
        const contentType = extname(filename) === ".html" ? "text/html; charset=utf-8" : "text/javascript; charset=utf-8";
        response.writeHead(200, {
          "content-type": contentType,
          "content-length": content.length,
          "cache-control": "no-store",
          "content-security-policy": "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; connect-src 'self'",
          "x-content-type-options": "nosniff",
          "x-frame-options": "DENY",
        });
        response.end(content);
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/state") {
        const handle = await authenticate(request, response);
        if (!handle) return;
        json(response, 200, { handle, state: publicState(repository) });
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/stream") {
        const handle = await authenticate(request, response);
        if (!handle) return;
        response.writeHead(200, {
          "content-type": "application/x-ndjson; charset=utf-8",
          "cache-control": "no-store",
          connection: "keep-alive",
          "x-content-type-options": "nosniff",
        });
        response.write(`${JSON.stringify({ type: "welcome", handle, state: publicState(repository) })}\n`);
        streams.add(response);
        request.on("close", () => streams.delete(response));
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/prod") {
        const handle = await authenticate(request, response);
        if (!handle) return;
        const prodRequest = await requestBody(request);
        const result = await exclusive(() => mediorgan.prod(handle, prodRequest));
        broadcast({ type: "snapshot", state: publicState(repository) });
        for (const event of result.outputs) broadcast({ type: "sonic", event });
        json(response, 202, { accepted: true, prodId: result.record.payload.prodId, eventSeq: result.record.seq, outputs: result.outputs });
        return;
      }
      json(response, 404, { error: "not found" });
    } catch (error) {
      json(response, /rate limit|too large/.test(error.message) ? 429 : 400, { error: error.message });
    }
  });

  await new Promise((resolveListen, reject) => {
    server.once("error", reject);
    server.listen(port, host, resolveListen);
  });
  const interval = tickMs > 0 ? setInterval(() => {
    void exclusive(() => repository.transact("advance", { ticks: 1 })).then((result) => {
      broadcast({ type: "snapshot", state: publicState(repository) });
      for (const event of result.outputs) broadcast({ type: "sonic", event });
    }).catch(onTickError);
  }, tickMs) : null;
  interval?.unref();

  return {
    server,
    repository,
    mediorgan,
    address: server.address(),
    async stop({ sleep = false } = {}) {
      if (interval) clearInterval(interval);
      for (const response of streams) response.end();
      await new Promise((resolveClose) => server.close(resolveClose));
      await writeTail;
      await repository.idle();
      return sleep ? sleepCommit(repository) : null;
    },
  };
}

function option(name, fallback) {
  const index = process.argv.indexOf(name);
  return index === -1 ? fallback : process.argv[index + 1];
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const host = option("--host", "127.0.0.1");
  const port = Number(option("--port", "8787"));
  const root = resolve(option("--root", "./terrarium-state"));
  const capabilities = JSON.parse(process.env.TERRARIUM_DEV_CAPS || "{}");
  const app = await createTerrariumServer({ root, host, port, capabilities });
  console.log(JSON.stringify({ listening: `http://${app.address.address}:${app.address.port}`, binding: app.address, root }));
  const shutdown = async () => {
    await app.stop({ sleep: true });
    process.exit(0);
  };
  process.once("SIGINT", shutdown);
  process.once("SIGTERM", shutdown);
}
