#!/usr/bin/env node
// Write the steamcmd build scripts for oskiewar's three depots.
//
//   node xbox/steam/depots/depots.mjs --appid=1234560 [--desc="v118 first upload"] [--branch=]
//
// Steam hands out the appid and reserves appid+1..+3 as depots by default;
// pass --depots=a,b,c to override. The VDFs are regenerated every run (they
// are build products, not source), and steamcmd is pointed at them by
// upload.sh. Content roots are the electron-builder `dir` outputs in
// ../shell/dist — run the three build:* scripts first.

import { mkdir, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const flags = new Map(process.argv.slice(2).map((entry) => {
  const [key, value = "true"] = entry.replace(/^--/, "").split("=");
  return [key, value];
}));
const appid = Number(flags.get("appid"));
if (!appid) { console.error("need --appid=<n> (the appid Steamworks assigned)"); process.exit(1); }
const depots = (flags.get("depots") || `${appid + 1},${appid + 2},${appid + 3}`)
  .split(",").map(Number);
const desc = flags.get("desc") || `oskiewar ${new Date().toISOString().slice(0, 10)}`;
const branch = flags.get("branch") || "";

const platforms = [
  { name: "windows", depot: depots[0], root: "../shell/dist/win-unpacked" },
  { name: "macos", depot: depots[1], root: "../shell/dist/mac-arm64/oskiewar.app" },
  { name: "linux", depot: depots[2], root: "../shell/dist/linux-unpacked" },
];

const out = resolve(here, "out");
await mkdir(out, { recursive: true });
for (const platform of platforms) {
  await writeFile(resolve(out, `depot_${platform.name}.vdf`), `"DepotBuildConfig"
{
  "DepotID" "${platform.depot}"
  "ContentRoot" "${resolve(here, platform.root)}"
  "FileMapping"
  {
    "LocalPath" "*"
    "DepotPath" "."
    "recursive" "1"
  }
  "FileExclusion" "*.pdb"
  "FileExclusion" "*.map"
}
`);
}
await writeFile(resolve(out, "app_build.vdf"), `"AppBuild"
{
  "AppID" "${appid}"
  "Desc" "${desc}"
  "BuildOutput" "${resolve(out, "logs")}"
  "ContentRoot" "${resolve(here, "../shell/dist")}"
  "SetLive" "${branch}"
  "Depots"
  {
${platforms.map((p) => `    "${p.depot}" "${resolve(out, `depot_${p.name}.vdf`)}"`).join("\n")}
  }
}
`);
console.log(`wrote ${out}/app_build.vdf for appid ${appid} (depots ${depots.join(", ")})`);
