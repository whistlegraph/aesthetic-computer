#!/usr/bin/env node
// femrag — microtrax's ragtime score played by short FEM-marimba bells.
if (!process.argv.includes("--fem")) process.argv.push("--fem");
await import("./render-microtrax.mjs");
