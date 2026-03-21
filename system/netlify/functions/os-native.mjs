// os-native, 2026.03.09
// Auth-gated endpoint for personalized FedAC OS images.
// Returns vmlinuz download URL + user config (handle, piece).
// Requires authentication — user must have a handle.
//
// GET /api/os-native?piece=notepat
// Authorization: Bearer <token>
// Returns: { vmlinuzUrl, config: { handle, piece }, flashScript }

import { authorize, handleFor } from "../../backend/authorization.mjs";

const RELEASES_BASE = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return { statusCode: 405, body: JSON.stringify({ error: "GET only" }) };
  }

  // Authenticate
  const user = await authorize(event.headers);
  if (!user) {
    return {
      statusCode: 401,
      body: JSON.stringify({ error: "Authentication required. Log in at aesthetic.computer first." }),
      headers: { "Content-Type": "application/json" },
    };
  }

  // Resolve handle
  const handle = await handleFor(user.sub);
  if (!handle) {
    return {
      statusCode: 403,
      body: JSON.stringify({ error: "You need a handle first. Visit aesthetic.computer/handle to claim one." }),
      headers: { "Content-Type": "application/json" },
    };
  }

  const piece = event.queryStringParameters?.piece || "notepat";
  const config = { handle, piece };

  // Get latest version from releases
  let vmlinuzUrl = `${RELEASES_BASE}/latest.vmlinuz`;
  let version = "latest";
  try {
    const res = await fetch(`${RELEASES_BASE}/latest.version`);
    if (res.ok) {
      version = (await res.text()).trim();
      vmlinuzUrl = `${RELEASES_BASE}/${version}.vmlinuz`;
    }
  } catch (_) {}

  // Generate a flash script the user can pipe to bash
  const configJson = JSON.stringify(config);
  const flashScript = [
    `#!/bin/bash`,
    `# FedAC OS flash script for @${handle}`,
    `# Usage: bash flash.sh /dev/sdX`,
    `set -e`,
    `DEV="\${1:?Usage: bash flash.sh /dev/sdX}"`,
    `echo "Flashing FedAC OS for @${handle} to \${DEV}..."`,
    `echo "Downloading vmlinuz (${version})..."`,
    `curl -fSL -o /tmp/ac-vmlinuz "${vmlinuzUrl}"`,
    `echo "Partitioning \${DEV}..."`,
    `sudo sfdisk "\${DEV}" <<EOF`,
    `label: gpt`,
    `type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M`,
    `EOF`,
    `echo "Formatting..."`,
    `sudo mkfs.vfat -F 32 -n AC-NATIVE "\${DEV}1"`,
    `echo "Writing files..."`,
    `sudo mmd -i "\${DEV}1" ::EFI ::EFI/BOOT`,
    `sudo mcopy -o -i "\${DEV}1" /tmp/ac-vmlinuz ::EFI/BOOT/BOOTX64.EFI`,
    `echo '${configJson}' | sudo mcopy -i "\${DEV}1" - ::config.json`,
    `sync`,
    `echo "Done! Boot from \${DEV} to start @${handle}/notepat"`,
  ].join("\n");

  return {
    statusCode: 200,
    body: JSON.stringify({
      handle,
      piece,
      version,
      vmlinuzUrl,
      config,
      flashScript,
    }),
    headers: { "Content-Type": "application/json" },
  };
}
