import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { once } from "node:events";
import net from "node:net";
import test from "node:test";
import { boot } from "../../system/public/aesthetic.computer/disks/help.mjs";

test("help sends existing links and parameters to the human chat", () => {
  const params = ["hello", "@jeffrey"];
  let destination;
  boot({ colon: "room", params, alias: (...args) => { destination = args; } });
  assert.deepEqual(destination, ["chat", "room", params]);
});

test("retired helper returns Gone while private bridge routes retain authentication", async () => {
  const reservation = net.createServer();
  reservation.listen(0, "127.0.0.1");
  await once(reservation, "listening");
  const port = reservation.address().port;
  await new Promise(resolve => reservation.close(resolve));
  const child = spawn(process.execPath, [new URL("./aa-bridge.mjs", import.meta.url).pathname], {
    env: { PATH: process.env.PATH, HOME: process.env.HOME, AA_PORT: String(port), ADMIN_SUB: "retirement-test", CLAUDE_BIN: "/nonexistent-retired-helper" },
    stdio: ["ignore", "pipe", "pipe"],
  });
  const exited = once(child, "exit");
  try {
    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error("Bridge did not start")), 5000);
      child.stdout.on("data", data => {
        if (data.toString().includes("aa-bridge listening")) { clearTimeout(timeout); resolve(); }
      });
      child.once("error", error => { clearTimeout(timeout); reject(error); });
    });
    const base = `http://127.0.0.1:${port}`;
    for (const path of ["/api/help/chat", "/api/help/chat?legacy=1"]) {
      const response = await fetch(base + path, { method: "POST", headers: { Origin: "https://aesthetic.computer" }, body: "{}" });
      assert.equal(response.status, 410);
      assert.equal(response.headers.get("access-control-allow-origin"), "https://aesthetic.computer");
      assert.equal((await response.json()).helpUrl, "https://aesthetic.computer/chat");
    }
    assert.equal((await fetch(base + "/health")).status, 200);
    for (const path of ["/api/chat", "/api/pp/chat"]) {
      assert.equal((await fetch(base + path, { method: "POST", body: "{}" })).status, 401);
    }
    assert.equal((await fetch(base + "/api/pp/whoami")).status, 401);
  } finally {
    child.kill("SIGTERM");
    await exited;
  }
});
