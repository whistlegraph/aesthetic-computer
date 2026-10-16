#!/usr/bin/env node
// minibook-native.mjs — build the Xbox app (native-bios, UWP MSIX) on the
// Choowee, the Windows laptop beside the console and install it, instead of waiting on
// AppVeyor. Every stage posts to the laptop's on-screen display
// (~\ac-progress.log, tailed by progress-window.ps1) and is timed here.
//
//   node xbox/tools/minibook-native.mjs            sync + build + install
//   node xbox/tools/minibook-native.mjs --no-install
//   --force-sync refreshes restored source timestamps; --with-dependencies
//   also installs framework packages when preparing a fresh console.
//
// The laptop's OSD is toolchain/windows/progress-window.ps1.
//
// Sync sends only files whose content changed since the last run: untouched
// files keep their timestamps on the laptop, so msbuild's incremental state
// (and the incremental LTCG link) survives between builds. AppVeyor stays the
// release validator; this is the development loop (xbox/BUILD-RELEASE-PLAN.md).
import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { tmpdir, homedir } from "node:os";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const host = process.env.CHOOWEE_HOST || process.env.MINIBOOK_HOST || "choowee";
const remoteRoot = "src/ac";                       // under the laptop's $HOME
const state = join(homedir(), ".cache", "minibook-native");
const sentPath = join(state, `${host}-sent.json`);
const args = new Set(process.argv.slice(2));

// What the UWP project reads, relative to the repo root.
const roots = [
  "xbox/native-bios", "xbox/runtime/include", "xbox/themes/photorealistic/native",
  "xbox/tools/oskiewar-native-source.mjs",
  "xbox/live/oskiewar.js", "xbox/live/oskiewar-fighter.mjs", "xbox/live/native-account.mjs",
  "xbox/live/sfx/dojo-punch.wav", "pop/nullabye/c/ac_hrtf.h",
  "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs",
  "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf",
  "system/public/type/webfonts/ywft-processing-regular.ttf",
  "apple/oskiewar/Assets.xcassets/AppIcon.appiconset/AppIcon.png",
];
const skip = /[\\/](gdk|tests|generated|x64|Release|Debug|AppPackages|BundleArtifacts)[\\/]|\.(pfx|obj|pdb)$/;

const started = Date.now();
const seconds = () => ((Date.now() - started) / 1000).toFixed(1) + "s";
const ssh = (command, options = {}) =>
  execFileSync("ssh", ["-o", "BatchMode=yes", host, command], { encoding: "utf8", ...options });
const say = (line) => {
  console.log(`[${seconds()}] ${line}`);
  try { ssh(`Add-Content $HOME\\ac-progress.log ((Get-Date -Format HH:mm:ss) + '  xbox: ${line.replace(/'/g, "")}')`); } catch {}
};

// ── 1. sync what changed ────────────────────────────────────────────────────
function files() {
  const listed = execFileSync("git", ["-C", root, "ls-files", "-co", "--exclude-standard", "--", ...roots],
    { encoding: "utf8" }).split("\n").filter(Boolean);
  // Private product files (oskiewar.js and friends) are gitignored symlinks
  // into the vault, so ls-files never lists them; name them outright.
  for (const path of roots)
    if (!listed.includes(path) && /\.[a-z0-9]+$/i.test(path) && existsSync(join(root, path))) listed.push(path);
  return listed.filter((path) => !skip.test(path) && existsSync(join(root, path)));
}
function sync() {
  mkdirSync(state, { recursive: true });
  const sent = existsSync(sentPath) ? JSON.parse(readFileSync(sentPath, "utf8")) : {};
  const now = {};
  const changed = [];
  for (const path of files()) {
    now[path] = createHash("sha1").update(readFileSync(join(root, path))).digest("hex");
    if (args.has("--force-sync") || sent[path] !== now[path]) changed.push(path);
  }
  if (!changed.length) { say("sources unchanged"); return now; }
  say(`syncing ${changed.length} changed file${changed.length === 1 ? "" : "s"}`);
  const scratch = mkdtempSync(join(tmpdir(), "minibook-native-"));
  const tarball = join(scratch, "sync.tar");
  writeFileSync(join(scratch, "list"), changed.join("\n") + "\n");
  execFileSync("tar", ["-chf", tarball, "-C", root, "-T", join(scratch, "list")]);
  ssh(`New-Item -ItemType Directory -Force $HOME\\${remoteRoot.replace("/", "\\")} | Out-Null`);
  execFileSync("scp", ["-q", tarball, `${host}:${remoteRoot}/sync.tar`]);
  // A restored local file may be older than a generated Windows manifest.
  // Mark changed content fresh; untouched files retain their incremental state.
  ssh(`$root = "$HOME\\${remoteRoot.replace("/", "\\")}"; $files = tar -tf "$root\\sync.tar"; tar -xf "$root\\sync.tar" -C $root; $now = Get-Date; foreach ($file in $files) { (Get-Item (Join-Path $root $file)).LastWriteTime = $now }; Remove-Item "$root\\sync.tar"`);
  rmSync(scratch, { recursive: true, force: true });
  return now;
}

// ── 2. one-time laptop setup: tile assets + a persistent signing key ────────
const setupScript = String.raw`
$ErrorActionPreference = 'Stop'
$root = Join-Path $HOME 'src\ac'
$project = Join-Path $root 'xbox\native-bios'
$assets = Join-Path $project 'Assets'
if (-not (Test-Path (Join-Path $assets 'Square150x150Logo.png'))) {
  Add-Type -AssemblyName System.Drawing
  $master = [Drawing.Image]::FromFile((Join-Path $root 'apple\oskiewar\Assets.xcassets\AppIcon.appiconset\AppIcon.png'))
  New-Item -ItemType Directory -Force $assets | Out-Null
  foreach ($a in @(@('Square150x150Logo.png',150,150), @('Square44x44Logo.png',44,44), @('Wide310x150Logo.png',310,150), @('SplashScreen.png',620,300), @('StoreLogo.png',50,50))) {
    $bitmap = [Drawing.Bitmap]::new($a[1], $a[2]); $g = [Drawing.Graphics]::FromImage($bitmap)
    $g.Clear([Drawing.Color]::FromArgb(169, 190, 203)); $g.InterpolationMode = 'HighQualityBicubic'
    $s = [Math]::Min($a[1] / $master.Width, $a[2] / $master.Height)
    $w = [int]($master.Width * $s); $h = [int]($master.Height * $s)
    $g.DrawImage($master, [int](($a[1] - $w) / 2), [int](($a[2] - $h) / 2), $w, $h)
    $bitmap.Save((Join-Path $assets $a[0]), [Drawing.Imaging.ImageFormat]::Png); $g.Dispose(); $bitmap.Dispose()
  }
  $master.Dispose()
  'assets made'
}
$key = Join-Path $project 'NativeBios_TemporaryKey.pfx'
if (-not (Test-Path $key)) {
  $cert = New-SelfSignedCertificate -Type Custom -Subject 'CN=AestheticComputerDev' -KeyUsage DigitalSignature -KeyExportPolicy Exportable -HashAlgorithm SHA256 -TextExtension @('2.5.29.37={text}1.3.6.1.5.5.7.3.3') -FriendlyName 'AC Xbox minibook' -CertStoreLocation 'Cert:\CurrentUser\My'
  Export-PfxCertificate -Cert $cert -FilePath $key -Password (ConvertTo-SecureString 'aesthetic-xbox-dev' -AsPlainText -Force) | Out-Null
  'signing key made'
}
`;

// ── 3. build, streaming msbuild into the OSD ────────────────────────────────
const msbuildArgs = [
  "/restore", "/m", "/nologo", "/v:minimal", "/p:Configuration=Release", "/p:Platform=x64",
  "/p:PreferredToolArchitecture=x64", "/p:GenerateAppxPackageOnBuild=true", "/p:AppxBundle=Never",
  "/p:UapAppxPackageBuildMode=SideloadOnly", "/p:PackageCertificatePassword=aesthetic-xbox-dev",
].join(" ");
const buildScript = String.raw`
$root = Join-Path $HOME 'src\ac'
$log = Join-Path $HOME 'ac-progress.log'
$msbuild = 'C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\MSBuild\Current\Bin\amd64\MSBuild.exe'
$out = Join-Path $root 'artifacts\native-bios\'
& $msbuild (Join-Path $root 'xbox\native-bios\NativeBios.sln') ${msbuildArgs} "/p:AppxPackageDir=$out" 2>&1 | ForEach-Object { $line = "$_"; if ($line.Trim()) { Add-Content $log ('         ' + $line) }; $line }
exit $LASTEXITCODE
`;

function powershell(script) {
  const encoded = Buffer.from(script, "utf16le").toString("base64");
  return spawnSync("ssh", ["-o", "BatchMode=yes", host, `powershell -NoProfile -ExecutionPolicy Bypass -EncodedCommand ${encoded}`],
    { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"], maxBuffer: 64 << 20 });
}
const clean = (text) => text.split("\n").filter((line) => !/^#< CLIXML|<Objs /.test(line)).join("\n").trim();

const sentNow = sync();
const setup = powershell(setupScript);
if (setup.status !== 0) { say("setup FAILED"); console.error(clean(setup.stderr || setup.stdout)); process.exit(1); }
if (clean(setup.stdout)) say(clean(setup.stdout).replace(/\n/g, ", "));
say("building native-bios (msbuild, incremental)");
const buildStarted = Date.now();
const build = powershell(buildScript);
const buildSeconds = ((Date.now() - buildStarted) / 1000).toFixed(1);
if (build.status !== 0) {
  say(`BUILD FAILED after ${buildSeconds}s`);
  console.error(clean(build.stdout).split("\n").slice(-30).join("\n"));
  process.exit(1);
}
writeFileSync(sentPath, JSON.stringify(sentNow));
say(`built in ${buildSeconds}s`);

// ── 4. fetch the package and install it on the Xbox ─────────────────────────
if (args.has("--no-install")) { say(`done in ${seconds()}`); process.exit(0); }
const listing = clean(ssh(`Get-ChildItem $HOME\\src\\ac\\artifacts\\native-bios -Recurse -Include *.msix,*.appx | Sort LastWriteTime -Descending | ForEach-Object { $_.FullName.Substring($HOME.Length + 1).Replace('\\','/') }`));
const all = listing.split("\n").map((line) => line.trim()).filter(Boolean);
const manifest = readFileSync(join(root, "xbox/native-bios/Package.appxmanifest"), "utf8");
const version = manifest.match(/<Identity\b[^>]*\bVersion="([^"]+)"/)?.[1];
if (!version) throw new Error("Native BIOS manifest has no package version");
const main = all.find((path) => path.endsWith(`/NativeBios_${version}_x64.msix`));
if (!main) { say("no MSIX produced"); process.exit(1); }
// The devkit already has VCLibs. Reposting it with an update made Device Portal
// return HTTP 500; the same verified MSIX succeeds on its own.
const deps = args.has("--with-dependencies")
  ? [...new Map(all.filter((path) => /Dependencies\/x64\/.*\.appx$/i.test(path))
    .map(path => [path.split("/").pop(), path])).values()] : [];
const local = mkdtempSync(join(tmpdir(), "minibook-msix-"));
say("fetching package");
for (const path of [main, ...deps]) execFileSync("scp", ["-q", `${host}:${path}`, join(local, path.split("/").pop())]);
const packageManifest = execFileSync("unzip", ["-p", join(local, main.split("/").pop()), "AppxManifest.xml"], { encoding: "utf8" });
const packagedVersion = packageManifest.match(/<Identity\b[^>]*\bVersion="([^"]+)"/)?.[1];
if (packagedVersion !== version) throw new Error(`Package contains ${packagedVersion}, expected ${version}; use --force-sync to refresh generated Windows inputs`);
say("installing on the xbox");
const installed = spawnSync("node", [join(root, "xbox/tools/live.mjs"), "install",
  join(local, main.split("/").pop()), ...deps.map((path) => join(local, path.split("/").pop()))],
  { encoding: "utf8", stdio: "inherit" });
if (installed.status !== 0) { say("INSTALL FAILED"); process.exit(1); }
// An install leaves the app stopped; start it so the edit is on screen. A
// launch can fail where the install "succeeded": on 2026-10-12 a reinstall
// of the same version left the registration broken (launch 0x8007000B, the
// console's own 0x8007010b) until `live.mjs uninstall` + a fresh install —
// which wipes the app's data. Cause not yet pinned; check, don't assume.
const launched = spawnSync("node", [join(root, "xbox/tools/live.mjs"), "launch"], { encoding: "utf8" });
if (launched.status !== 0) {
  say("LAUNCH FAILED after install — the registration may be broken");
  console.error(launched.stderr.trim());
  console.error("recover: node xbox/tools/live.mjs uninstall, then install an AppVeyor build (wipes app data)");
  process.exit(1);
}
say(`running on the xbox in ${seconds()}`);
