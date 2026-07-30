# Xbox JavaScript OTA lane

This lane updates sandboxed AC **piece JavaScript**, including a native No Paint
piece once its API port is complete. It cannot update the native BIOS, QuickJS,
graphics/audio bindings, permissions, or Store-declared functionality. Those
still require a signed MSIX release.

## Release and activation chain

1. CI runs the Xbox piece compatibility and runtime tests.
2. `release.mjs` hashes an immutable JS file and signs a canonical manifest with
   an offline/CI Ed25519 private key. The BIOS package embeds only public keys.
3. The Xbox fetches the manifest and source over HTTPS, verifies signature,
   expiry, monotonically increasing sequence, byte limit, and SHA-256.
4. The platform adapter writes the inactive `ApplicationData::LocalFolder`
   slot, flushes it, then atomically replaces the active-slot pointer.
5. `PieceSupervisor` compiles and boots the candidate before the frame-boundary
   swap. `Coordinator` keeps it on probation for 300 healthy frames.
6. A boot/callback/watchdog failure during probation restores the previous
   confirmed slot. Confirmation makes the new slot the last-known-good version.

`ac/ota.hpp` contains the platform-neutral state machine. The UWP adapter still
needs implementations for Ed25519 verification, strict manifest parsing,
`Windows.Web.Http.HttpClient`, SHA-256, and durable two-slot storage. It should
poll only while online and foregrounded, use a pinned AC update origin, and feed
runtime failures into `runtime_failure()`.
The slot record includes the accepted sequence, so downgrade protection
survives process and console restarts.

## Build a release

```sh
node xbox/ota/release.mjs \
  --source build/nopaint-xbox.js \
  --out build/xbox-ota/manifest.json \
  --key /secure/path/xbox-ed25519-private.pem \
  --key-id xbox-release-1 --version 2026.07.27.1 --sequence 1
```

The output directory is publish-once: the script refuses to overwrite the
manifest or source. Upload both with immutable cache headers, then update a
small channel pointer only after upload verification. Never commit private
keys or accept a key from downloaded content.

## Test

```sh
cmake -S xbox/runtime -B /tmp/ac-xbox-runtime
cmake --build /tmp/ac-xbox-runtime
ctest --test-dir /tmp/ac-xbox-runtime --output-on-failure
```

Store certification needs an explicit policy review: downloaded pieces must
remain content within AC's declared creative runtime, not arbitrary programs.
