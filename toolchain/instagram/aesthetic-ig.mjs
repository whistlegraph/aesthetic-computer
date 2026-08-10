#!/usr/bin/env node
import { runReelApp } from "./reel-app.mjs";

try { await runReelApp("aesthetic"); }
catch (error) { console.error(`✗ ${error.message || error}`); process.exit(1); }
