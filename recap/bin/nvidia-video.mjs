#!/usr/bin/env node
// nvidia-video — exercise NVIDIA NIM video gen on stills we already have.
//
// Two backends:
//   --mode svd       Stable Video Diffusion (sync). image → ~25-frame mp4.
//                    Input must be exactly 1024x576. We auto-crop a 1024x576
//                    band out of the supplied still (default: top, where
//                    jeffrey's head usually is in the 1024x1536 portraits).
//   --mode cosmos    Cosmos predict1-7b text2world (async, NVCF). prompt → mp4.
//                    No image conditioning in this script — text only for now.
//
// Usage:
//   node recap/bin/nvidia-video.mjs --mode svd \
//        --image recap/out/jeffrey-photos/01_title.png \
//        [--seed 42] [--cfg 1.8] [--crop top|center|bottom] \
//        [--out recap/out/nvidia-video/01_title.mp4]
//
//   node recap/bin/nvidia-video.mjs --mode cosmos \
//        --prompt "a cinematic shot of ..." \
//        [--seed 4] [--out recap/out/nvidia-video/cosmos-test.mp4]

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { resolve, basename, dirname, join } from "node:path";
import { tmpdir } from "node:os";

const NVIDIA_KEY = readFileSync(
  "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.env",
  "utf8",
).match(/^NVIDIA_API_KEY=(\S+)/m)?.[1];
if (!NVIDIA_KEY) {
  console.error("✗ NVIDIA_API_KEY not in vault/.env");
  process.exit(1);
}

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = process.argv[i + 1];
}

const mode = flags.mode || "svd";
const outDefault = `recap/out/nvidia-video/${mode}-${Date.now()}.mp4`;
const out = resolve(flags.out || outDefault);
mkdirSync(dirname(out), { recursive: true });

if (mode === "svd") await runSvd();
else if (mode === "cosmos") await runCosmos();
else {
  console.error(`unknown --mode ${mode}`);
  process.exit(1);
}

async function runSvd() {
  const image = flags.image;
  if (!image) {
    console.error("✗ --image required for svd");
    process.exit(1);
  }
  const src = resolve(image);
  if (!existsSync(src)) {
    console.error(`✗ image not found: ${src}`);
    process.exit(1);
  }

  // Crop/resize to exactly 1024x576. SVD rejects anything else.
  const cropped = join(tmpdir(), `svd-${Date.now()}-${basename(src)}`);
  const cropMode = flags.crop || "top";
  cropTo1024x576(src, cropped, cropMode);
  const bytes = readFileSync(cropped);
  console.log(`→ cropped to 1024x576 (${cropMode}): ${cropped} (${bytes.length} B)`);

  // Inline base64 only works <200 KB. Bigger needs the NVCF asset upload path.
  let imageField;
  if (bytes.length < 195_000) {
    imageField = `data:image/png;base64,${bytes.toString("base64")}`;
    console.log("  using inline base64");
  } else {
    const assetId = await uploadAsset(cropped, "image/png", "svd input");
    imageField = `data:image/png;asset_id,${assetId}`;
    console.log(`  uploaded asset_id=${assetId}`);
  }

  const seed = Number.isInteger(+flags.seed) ? +flags.seed : 0;
  const cfg = flags.cfg ? +flags.cfg : 1.8;

  console.log(`→ POST svd  seed=${seed}  cfg=${cfg}`);
  const t0 = Date.now();
  const res = await fetch(
    "https://ai.api.nvidia.com/v1/genai/stabilityai/stable-video-diffusion",
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${NVIDIA_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({
        image: imageField,
        seed,
        cfg_scale: cfg,
        motion_bucket_id: 127,
      }),
    },
  );
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  if (!res.ok) {
    console.error(`✗ svd ${res.status} after ${elapsed}s`);
    console.error((await res.text()).slice(0, 600));
    process.exit(1);
  }
  const data = await res.json();
  const art = data.artifacts?.[0];
  if (!art?.base64) {
    console.error("✗ no artifact in response", JSON.stringify(data).slice(0, 500));
    process.exit(1);
  }
  if (art.finishReason && art.finishReason !== "SUCCESS") {
    console.error(`✗ finishReason=${art.finishReason} (safety filter?)`);
    process.exit(1);
  }
  writeFileSync(out, Buffer.from(art.base64, "base64"));
  console.log(`✓ svd ${elapsed}s → ${out}`);
}

async function runCosmos() {
  const prompt = flags.prompt;
  if (!prompt) {
    console.error("✗ --prompt required for cosmos");
    process.exit(1);
  }
  const seed = Number.isInteger(+flags.seed) ? +flags.seed : 4;

  // Cosmos predict1-7b uses Triton-style payload with a CLI-shaped command.
  const url =
    "https://ai.api.nvidia.com/v1/cosmos/nvidia/cosmos-predict1-7b";
  const body = {
    inputs: [
      {
        name: "command",
        shape: [1],
        datatype: "BYTES",
        data: [
          `text2world --prompt="${prompt.replace(/"/g, '\\"')}" --seed=${seed}`,
        ],
      },
    ],
    outputs: [{ name: "status", datatype: "BYTES", shape: [1] }],
  };

  console.log(`→ POST cosmos  seed=${seed}`);
  console.log(`  prompt: ${prompt.slice(0, 120)}${prompt.length > 120 ? "…" : ""}`);
  const t0 = Date.now();
  let res = await fetch(url, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${NVIDIA_KEY}`,
      "Content-Type": "application/json",
      Accept: "application/json",
      "NVCF-POLL-SECONDS": "5",
    },
    body: JSON.stringify(body),
  });

  let reqId = res.headers.get("nvcf-reqid");
  console.log(`  initial status=${res.status} reqId=${reqId}`);

  while (res.status === 202) {
    if (!reqId) {
      console.error("✗ 202 without nvcf-reqid header");
      process.exit(1);
    }
    await new Promise((r) => setTimeout(r, 5000));
    res = await fetch(
      `https://api.nvcf.nvidia.com/v2/nvcf/pexec/status/${reqId}`,
      {
        method: "GET",
        headers: {
          Authorization: `Bearer ${NVIDIA_KEY}`,
          Accept: "application/json",
          "NVCF-POLL-SECONDS": "5",
        },
        redirect: "manual",
      },
    );
    const elapsed = ((Date.now() - t0) / 1000).toFixed(0);
    process.stdout.write(`  poll ${elapsed}s  status=${res.status}\n`);
    reqId = res.headers.get("nvcf-reqid") || reqId;
  }

  if (res.status === 302) {
    const loc = res.headers.get("location");
    console.log(`  ↪ redirect → ${loc?.slice(0, 100)}…`);
    const zipRes = await fetch(loc);
    const zipBuf = Buffer.from(await zipRes.arrayBuffer());
    const zipPath = out.replace(/\.mp4$/, ".zip");
    writeFileSync(zipPath, zipBuf);
    console.log(`  ✓ saved zip ${zipBuf.length} B → ${zipPath}`);
    // Extract any .mp4 from the zip
    const r = spawnSync("unzip", ["-o", "-d", dirname(out), zipPath], {
      stdio: "inherit",
    });
    if (r.status !== 0) console.error("  ⚠ unzip failed; inspect zip manually");
    return;
  }

  if (!res.ok) {
    console.error(`✗ cosmos ${res.status}`);
    console.error((await res.text()).slice(0, 600));
    process.exit(1);
  }

  // Some NVCF flows return JSON inline.
  const ct = res.headers.get("content-type") || "";
  if (ct.includes("application/json")) {
    const j = await res.json();
    console.log("  json response:", JSON.stringify(j).slice(0, 800));
    if (j.asset_url) {
      const dl = await fetch(j.asset_url);
      writeFileSync(out, Buffer.from(await dl.arrayBuffer()));
      console.log(`  ✓ ${out}`);
    }
    return;
  }

  const buf = Buffer.from(await res.arrayBuffer());
  writeFileSync(out, buf);
  console.log(`  ✓ ${buf.length} B → ${out}`);
}

function cropTo1024x576(src, dst, where /* top|center|bottom */) {
  // Use sips: get height, then crop a 576-tall band, then enforce 1024 wide.
  const info = spawnSync("sips", ["-g", "pixelWidth", "-g", "pixelHeight", src], {
    encoding: "utf8",
  });
  const w = +(/pixelWidth: (\d+)/.exec(info.stdout || "")?.[1] || 0);
  const h = +(/pixelHeight: (\d+)/.exec(info.stdout || "")?.[1] || 0);
  if (!w || !h) {
    console.error("✗ sips could not read image dimensions");
    process.exit(1);
  }

  // Step 1: scale long edge so the smaller dimension is at least 1024×576-fitting.
  // Strategy: scale to width=1024, then crop a 576-tall band from the result.
  const scaled = dst.replace(/\.png$/, ".scaled.png");
  spawnSync("sips", ["-z", String(Math.round((1024 / w) * h)), "1024", src, "--out", scaled], {
    stdio: "ignore",
  });
  const info2 = spawnSync("sips", ["-g", "pixelHeight", scaled], { encoding: "utf8" });
  const sh = +(/pixelHeight: (\d+)/.exec(info2.stdout || "")?.[1] || 0);
  if (sh < 576) {
    console.error(`✗ scaled height ${sh} < 576; image too wide for top crop`);
    process.exit(1);
  }
  // Step 2: pick crop offset (sips --cropOffset is X Y, --crop is HEIGHT WIDTH).
  let yOff;
  if (where === "center") yOff = Math.floor((sh - 576) / 2);
  else if (where === "bottom") yOff = sh - 576;
  else yOff = 0; // top

  const r = spawnSync(
    "sips",
    [
      "--cropOffset",
      "0",
      String(yOff),
      "-c",
      "576",
      "1024",
      scaled,
      "--out",
      dst,
    ],
    { stdio: "ignore" },
  );
  if (r.status !== 0) {
    console.error("✗ sips crop failed");
    process.exit(1);
  }
}

async function uploadAsset(path, contentType, description) {
  // 1) ask for an upload URL
  const auth = await fetch("https://api.nvcf.nvidia.com/v2/nvcf/assets", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${NVIDIA_KEY}`,
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: JSON.stringify({ contentType, description }),
  });
  if (!auth.ok) {
    console.error(`✗ asset auth ${auth.status}`, (await auth.text()).slice(0, 300));
    process.exit(1);
  }
  const { uploadUrl, assetId } = await auth.json();
  // 2) PUT the bytes
  const put = await fetch(uploadUrl, {
    method: "PUT",
    headers: { "Content-Type": contentType, "x-amz-meta-nvcf-asset-description": description },
    body: readFileSync(path),
  });
  if (!put.ok) {
    console.error(`✗ asset put ${put.status}`, (await put.text()).slice(0, 300));
    process.exit(1);
  }
  return assetId;
}
