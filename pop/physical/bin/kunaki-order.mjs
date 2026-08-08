#!/usr/bin/env node
// Order a physical kit from Kunaki: quote the shipping, place the order,
// then follow it until tracking appears.
//
// Credentials and the recipient live outside the repository, in
// ~/.config/kunaki/order.json:
//
//   { "credentials": { "userId": "…", "password": "…" },
//     "recipient": { "name": "…", "address1": "…", "city": "…",
//                    "stateProvince": "CA", "postalCode": "…",
//                    "country": "United States" } }
//
// A quote is free and anonymous. `place` is a test order unless --live is
// given, and a live order manufactures and bills — Kunaki does not cancel.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { orderUrl, request, shippingOptionsUrl, statusUrl } from "../../../marketing/podcast/lib/kunaki.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const argv = process.argv.slice(2);
const [command = "quote", ...rest] = argv;
const value = (name, fallback) => {
  const index = argv.indexOf(`--${name}`);
  return index === -1 ? fallback : argv[index + 1];
};

const configPath = resolve(value("config", `${homedir()}/.config/kunaki/order.json`));
const config = existsSync(configPath) ? JSON.parse(readFileSync(configPath, "utf8")) : {};
const credentials = {
  userId: process.env.KUNAKI_USER_ID || config.credentials?.userId,
  password: process.env.KUNAKI_PASSWORD || config.credentials?.password,
};
const recipient = config.recipient || {};

const kitPath = resolve(value("kit", resolve(HERE, "../out/pixsies-so-far-kunaki-cd")), "manifest.json");
const kit = JSON.parse(readFileSync(kitPath, "utf8"));
const productId = value("product", kit.vendor.productId);
const quantity = Number(value("quantity", 1));
if (!productId) throw new Error(`No product id yet — publish the product, then put it in ${kitPath} under vendor.productId`);
const items = [{ productId, quantity }];

const money = (usd) => `$${Number(usd).toFixed(2)}`;

function record(entry) {
  kit.vendor.orders = [...(kit.vendor.orders || []), entry];
  kit.state = entry.mode === "Live" ? "ordered" : kit.state;
  writeFileSync(kitPath, `${JSON.stringify(kit, null, 2)}\n`);
}

if (command === "quote") {
  const where = {
    country: value("country", recipient.country),
    stateProvince: value("state", recipient.stateProvince || ""),
    postalCode: value("postal", recipient.postalCode),
  };
  const { options } = await request(shippingOptionsUrl({ ...where, items }));
  console.log(`${quantity} × ${productId} → ${where.postalCode}, ${where.country}\n`);
  for (const option of options) {
    console.log(`  ${money(option.priceUsd).padStart(8)}  ${option.deliveryTime.padEnd(16)} ${option.description}`);
  }
  console.log(`\nPlace it with:\n  node pop/physical/bin/kunaki-order.mjs place --shipping "${options[0].description}" --quantity ${quantity}`);
} else if (command === "place") {
  const shippingDescription = value("shipping");
  if (!shippingDescription) throw new Error("Pass --shipping with a description exactly as the quote returned it");
  const mode = argv.includes("--live") ? "Live" : "Test";
  const result = await request(orderUrl({ credentials, recipient, shippingDescription, items, mode }));
  record({ mode, orderId: result.orderId, productId, quantity, shippingDescription, placedAt: new Date().toISOString() });
  console.log(`${mode} order ${result.orderId} · ${quantity} × ${productId} · ${shippingDescription}`);
  if (mode === "Test") console.log("Test orders are not manufactured. Repeat with --live and KUNAKI_ALLOW_LIVE=1 to commit.");
} else if (command === "status") {
  const orderId = rest.find((argument) => !argument.startsWith("--")) || kit.vendor.orders?.at(-1)?.orderId;
  const result = await request(statusUrl({ credentials, orderId }));
  console.log(`${orderId} · ${result.orderStatus}${result.trackingId ? ` · ${result.trackingType} ${result.trackingId}` : ""}`);
} else {
  throw new Error(`Unknown command "${command}" — quote, place, or status`);
}
