#!/usr/bin/env node
/**
 * 🌸 Poppy - Open an AC piece and stream console logs
 *
 * Usage:
 *   node artery/poppy.mjs <piece>
 *
 * Examples:
 *   node artery/poppy.mjs notepat
 *   node artery/poppy.mjs prompt
 */

import { getArtery } from "./artery-auto.mjs";

const PURPLE_BG = "\x1b[45m";
const WHITE = "\x1b[97m";
const RESET = "\x1b[0m";
const GREEN = "\x1b[92m";
const YELLOW = "\x1b[93m";
const CYAN = "\x1b[96m";

const poppyLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🌸${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️ ${msg}${RESET}`);
const infoLog = (msg) => console.log(`${CYAN}ℹ️ ${msg}${RESET}`);

const args = process.argv.slice(2);
const piece = args.find((arg) => !arg.startsWith("-")) || "prompt";

function formatConsoleMessage(type, message) {
  const icon =
    type === "error"
      ? "❌"
      : type === "warn"
      ? "⚠️"
      : type === "info"
      ? "ℹ️"
      : "📝";
  return `${icon} [${type}] ${message}`;
}

async function main() {
  poppyLog(`Poppy starting → ${piece}`);

  try {
    const Artery = await getArtery();

    if (typeof Artery.openPanelStandalone === "function") {
      await Artery.openPanelStandalone();
    }

    const client = new Artery();
    await client.connect();

    await client.enableConsole((type, message) => {
      console.log(formatConsoleMessage(type, message));
    });

    if (typeof client.send === "function") {
      await client.send("Log.enable");
      await client.send("Runtime.enable");
    }

    if (typeof client.on === "function") {
      client.on("Runtime.exceptionThrown", (params) => {
        const text = params?.exceptionDetails?.text || "Exception";
        const description = params?.exceptionDetails?.exception?.description;
        console.log(`💥 ${text}`);
        if (description) console.log(description);
      });

      client.on("Log.entryAdded", (params) => {
        const entry = params?.entry;
        if (!entry) return;
        console.log(`📋 [${entry.level}] ${entry.text}`);
      });
    }

    if (typeof client.jump === "function") {
      await client.jump(piece);
    }

    successLog(`Connected and streaming logs for ${piece}`);
    infoLog("Press Ctrl+C to stop.");

    process.on("SIGINT", async () => {
      warnLog("Stopping Poppy...");
      try {
        client.close();
      } catch (err) {
        // ignore
      }
      if (typeof Artery.closePanelStandalone === "function") {
        await Artery.closePanelStandalone();
      }
      process.exit(0);
    });

    // Keep alive for console streaming
    await new Promise(() => {});
  } catch (err) {
    console.error(`${YELLOW}Error:${RESET}`, err?.message || err);
    process.exit(1);
  }
}

main();
