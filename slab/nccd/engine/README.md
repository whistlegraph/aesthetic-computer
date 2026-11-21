# nccd-engine

NCCd's Signal engine, as a library inside the app. It replaces `signal-cli`:
no Java, no Homebrew, and the engine updates with the app.

It is a small Rust crate on [presage](https://github.com/whisperfish/presage),
which is on libsignal, Signal's own library. The app talks to it through three
C functions (`include/nccd_engine.h`) in JSON lines, in the shape
`signal-cli jsonRpc` uses. `Sources/SignalEngine.swift` is the app's side.

This is not in a release yet. The app uses it only when its config has
`"engine": "lib"` (or `NCC_ENGINE=lib` is in the environment).

## The rules it keeps

1. The store is encrypted (SQLCipher). The key comes from the app, which keeps
   it in the login Keychain. The key is in no log.
2. The engine keeps keys and groups, and no messages.
3. Nothing leaves the engine until the app names the club's groups. Then only
   those groups do, and the engine sends only to them.
4. A file's name on disk comes from the engine, never from the sender.
5. When a member's identity key changes, the app is told.

`src/lib.rs` has the engine. `src/wire.rs` has every rule that is plain text
or bytes, with its tests.

## Build and test

You need Rust, `protoc` and `cmake`. The build takes about 6 GB of memory, so
use a Mac with 16 GB.

```
cargo test --release          # 16 tests
cargo audit                   # known faults in the dependencies
```

The library the app links, for both kinds of Mac:

```
export MACOSX_DEPLOYMENT_TARGET=13.0
cargo build --release --lib --target aarch64-apple-darwin
cargo build --release --lib --target x86_64-apple-darwin
mkdir -p lib
lipo -create -output lib/libnccd_engine.a \
  target/aarch64-apple-darwin/release/libnccd_engine.a \
  target/x86_64-apple-darwin/release/libnccd_engine.a
```

`../build.sh` links `lib/libnccd_engine.a` when it is there.

## Try it by hand

```
cargo build --release --example try
NCCD_KEY=<64 hex> target/release/examples/try <folder> link <device name>
NCCD_KEY=<64 hex> target/release/examples/try <folder> run 60
```

It prints what kind of thing arrived, and no words of any message.
`NCCD_TRACE=1` shows what the libraries below say.

## Known

- `strip` is off in the release profile. macOS 27 refuses a library that it
  touched.
- presage is pinned to one commit in `Cargo.toml`. Its lock file pinned two
  dependencies with known faults (RUSTSEC-2026-0385, RUSTSEC-2026-0383); ours
  has the fixed versions.
- A first contact's identity key is trusted, as in Signal. A changed key is
  trusted too and the app is told, as Signal Desktop does.
- Pictures and files are written as plain files that only the owner reads.
- The engine does not know if a group lets only admins write.
