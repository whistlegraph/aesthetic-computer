#!/usr/bin/env node
import { readFileSync } from "node:fs";
import { CASSETTE_SPECS, orderUrl, request, shippingOptionsUrl, statusUrl } from "../lib/kunaki.mjs";

const [command = "specs", file] = process.argv.slice(2);
const input = file ? JSON.parse(readFileSync(file, "utf8")) : {};
const credentials = { userId: process.env.KUNAKI_USER_ID, password: process.env.KUNAKI_PASSWORD };

let result;
if (command === "specs") result = CASSETTE_SPECS;
else if (command === "shipping") result = await request(shippingOptionsUrl(input));
else if (command === "order") result = await request(orderUrl({ ...input, credentials, mode: input.mode || "Test" }));
else if (command === "status") result = await request(statusUrl({ credentials, orderId: input.orderId }));
else throw new Error("Usage: kunaki.mjs specs | shipping input.json | order input.json | status input.json");

console.log(JSON.stringify(result, null, 2));
