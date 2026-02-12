#!/usr/bin/env node
// Entry point for aesthetic.computer MCP server

import { AestheticComputerServer } from "./server.js";

// Get token from environment variable or command line arg
const token = process.env.AC_TOKEN || process.argv.find(arg => arg.startsWith('--token='))?.split('=')[1];

const server = new AestheticComputerServer(token);
server.run().catch((error) => {
  console.error("Server error:", error);
  process.exit(1);
});
