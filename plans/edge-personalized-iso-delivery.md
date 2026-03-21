# Edge Personalized ISO Delivery

## Problem

Personalized AC OS images (with handle, auth token, Claude/GitHub tokens baked
into `config.json`) are currently built on-demand by the oven in NYC. Users far
from NYC (e.g. Novosibirsk) see slow downloads because every request does a full
roundtrip to the origin.

## Approach: Edge-Side Byte Patching

The template ISO contains a fixed-size **identity block** at a known offset on
the FAT32 EFI System Partition. The block is zero-padded placeholder data.

A Cloudflare Worker fetches the template ISO from R2 (edge-cached globally),
patches the identity block with the user's config, and streams the result. No
origin roundtrip. First download is fast everywhere.

### Identity Block

The identity block lives on the uncompressed FAT32 partition inside the ISO —
not inside the compressed initramfs. This means byte-level patching with zero
decompression overhead.

**Current (v1): 32KB — credentials + config**
```json
{
  "handle": "max",
  "piece": "notepat",
  "sub": "auth0|...",
  "email": "max@example.com",
  "token": "eyJ0eXAi...",
  "claudeToken": "sk-ant-...",
  "githubPat": "ghp_..."
}
```
~300 bytes of JSON, zero-padded to 32,768 bytes. Plenty of room to add fields.

**Future (v2): 8MB — user identity pack**
The block grows to include the user's creative data:
```
[0x0000 - 0x7FFF]  32KB   config.json (zero-padded)
[0x8000 - 0x0FFF]  32KB   manifest.json (file index)
[0x10000 - ...]    ~7.9MB user data:
                           - paintings (PNG thumbnails)
                           - audio samples (WAV/PCM)
                           - KidLisp pieces (.lisp source)
                           - notepat patterns
                           - custom themes
```
This means a freshly flashed device boots with the user's creative identity
already present — their paintings on the wall, their samples loaded, their
pieces ready to run.

The 8MB block on a ~128MB ISO is only 6% overhead. Could go larger if needed.

### Marker & Offset

The build script writes a known marker at the start of the identity block:

```
AC_IDENTITY_BLOCK_V1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00
```

The build also records the byte offset in a manifest file uploaded alongside
the ISO:

```json
{
  "name": "oxide-tegu",
  "hash": "6108a9738",
  "timestamp": "2026-03-18T...",
  "identityBlockOffset": 1048576,
  "identityBlockSize": 32768
}
```

The Worker uses the offset from the manifest to seek directly — no scanning.
The marker is a safety check to verify alignment before patching.

## Architecture

```
User (Siberia)
  │
  ▼
Cloudflare Edge POP (nearest)
  │
  ▼
oven-edge Worker
  │
  ├─ 1. Authenticate user (verify AC token via oven-origin)
  ├─ 2. Fetch user config from oven-origin /api/user-config
  ├─ 3. Fetch template ISO from R2 (edge-cached, ~128MB)
  ├─ 4. Stream ISO, patching identity block on the fly
  └─ 5. Done — no origin needed for the bulk data
```

### Streaming Patch (no buffering the whole ISO)

The Worker streams the template ISO from R2 and patches on-the-fly:

```js
async function streamPatchedISO(templateBody, config, manifest) {
  const offset = manifest.identityBlockOffset;
  const size = manifest.identityBlockSize;
  const patch = makeIdentityBlock(config, size); // JSON + zero-pad

  // TransformStream that patches bytes at the known offset
  let bytesSeen = 0;
  const { readable, writable } = new TransformStream({
    transform(chunk, controller) {
      const chunkStart = bytesSeen;
      const chunkEnd = bytesSeen + chunk.length;
      bytesSeen = chunkEnd;

      // Does this chunk overlap the identity block?
      if (chunkEnd > offset && chunkStart < offset + size) {
        const buf = new Uint8Array(chunk);
        const patchStart = Math.max(0, offset - chunkStart);
        const patchOffset = Math.max(0, chunkStart - offset);
        const patchLen = Math.min(
          size - patchOffset,
          chunk.length - patchStart
        );
        buf.set(patch.subarray(patchOffset, patchOffset + patchLen), patchStart);
        controller.enqueue(buf);
      } else {
        controller.enqueue(chunk);
      }
    },
  });

  templateBody.pipeTo(writable);
  return readable;
}
```

This uses ~zero memory overhead — chunks flow through, only the identity block
region gets patched. The rest is untouched passthrough.

## Implementation Steps

### 1. Bump identity block to 32KB in build script

In `ac-os` (line ~646), change the config.json padding from 4096 to 32768.
Add the marker header. Record the offset.

### 2. Enable Cloudflare R2

- Enable R2 on the Cloudflare account (free 10GB, free egress)
- Create bucket: `ac-os-images`
- Bind to `oven-edge` Worker as `OS_IMAGES`

### 3. Upload to R2 on publish

Modify `ac-os upload`:

```bash
# After existing S3 upload:
wrangler r2 object put ac-os-images/builds/${BUILD_NAME}/template.iso \
  --file /tmp/ac-native.iso
wrangler r2 object put ac-os-images/builds/${BUILD_NAME}/manifest.json \
  --file /tmp/ac-manifest.json
```

### 4. Add user-config API to oven

New endpoint: `GET /api/user-config` (authenticated)

Returns the user's full identity payload:
```json
{
  "handle": "max",
  "piece": "notepat",
  "sub": "auth0|...",
  "email": "max@example.com",
  "token": "<ac-auth-token>",
  "claudeToken": "sk-ant-...",
  "githubPat": "ghp_..."
}
```

The Worker calls this once per download — tiny request, fast.

### 5. Edge-side patching in oven-edge Worker

Add `/os-image` route to the Worker:

```js
async function handleOSImage(request, env) {
  // 1. Auth — forward token to oven-origin
  const token = request.headers.get("Authorization");
  const configRes = await fetch(ORIGIN + "/api/user-config", {
    headers: { Authorization: token },
  });
  const config = await configRes.json();

  // 2. Get latest manifest
  const manifestObj = await env.OS_IMAGES.get("latest-manifest.json");
  const manifest = await manifestObj.json();

  // 3. Get template ISO from R2
  const iso = await env.OS_IMAGES.get(
    `builds/${manifest.name}/template.iso`
  );

  // 4. Stream with patch
  const patched = await streamPatchedISO(iso.body, config, manifest);

  return new Response(patched, {
    headers: {
      "Content-Type": "application/octet-stream",
      "Content-Disposition": `attachment; filename="ac-${manifest.name}.iso"`,
      "Content-Length": String(manifest.isoSize),
      "X-Build": manifest.name,
      "X-Edge-Pop": request.cf?.colo || "unknown",
    },
  });
}
```

### 6. Cache invalidation

Only the template ISO is cached (in R2). Personalized ISOs are generated
on-the-fly by patching — nothing to invalidate. When a new build drops:

```bash
# Upload new template + manifest, overwrite latest pointer
wrangler r2 object put ac-os-images/latest-manifest.json \
  --file /tmp/ac-manifest.json
```

Old builds auto-expire via R2 lifecycle rules (e.g. delete after 30 days).

### 7. Native C code: read from identity block

On boot, `ac-native.c` already reads `/mnt/config.json`. The FAT32 mount
exposes the identity block as a regular file. No C changes needed for v1.

For v2 (8MB identity pack), add a boot-time unpacker:
```c
// Read /mnt/identity.bin → extract paintings to /mnt/user/paintings/
// Extract samples to /mnt/user/samples/
// Extract pieces to /mnt/user/pieces/
```

## Cost Estimate

- **R2 storage**: ~128MB per build (template only). 10 builds = 1.3GB. Free tier.
- **R2 reads**: Template fetched once per download. Class B reads are free.
- **R2 egress**: Free (!)
- **Worker CPU**: Streaming patch uses minimal CPU. Well within free tier.
- **No per-user storage**: Personalized ISOs are never stored, only streamed.

Effectively free at any scale.

## Rollout

1. Bump identity block to 32KB, add marker, record offset in manifest
2. Enable R2, create bucket, bind to Worker
3. Modify `ac-os upload` to push template ISO + manifest to R2
4. Add `/api/user-config` endpoint to oven
5. Add streaming patch to `oven-edge` Worker
6. Test with @jeffrey (NYC) and @sat (Novosibirsk)
7. Add edge POP display to os.mjs download UI

## Future: 8MB Identity Pack (v2)

When ready to include user data in the image:
1. Grow identity block to 8MB in build script
2. Add user data export API to oven (paintings, samples, pieces)
3. Worker fetches user data, packs into identity block format
4. Native C unpacker extracts on first boot
5. User boots a fresh device and their creative world is already there
