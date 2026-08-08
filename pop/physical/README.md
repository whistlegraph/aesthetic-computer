# Physical /pop

`pixsies-so-far.json` is the nine-track program of every title marked
`RELEASED` in `pop/RELEASES.md` through August 7, 2026. Two kits are built
from it — a jewel-case CD and a cassette — and both press the same masters.

```sh
node pop/physical/bin/masters.mjs             # Red Book masters, one per track
node pop/physical/bin/kunaki-cd-kit.mjs       # jewel-case CD kit
node pop/physical/bin/kunaki-cassette-kit.mjs # two-sided cassette kit
```

## Masters

`masters.json` says where each track's lossless master comes from: the
shelf-sync Space, the AC CDN, or a re-render through the lane's own engine
as `pop/RELEASES.md` documents it. Every master lands at 44.1 kHz / 16-bit
stereo in `masters/`, and the pipeline refuses any result that drifts more
than half a second from the released recording.

Seven of the nine are lossless. **trancenwaltz** and **trancepenta** are
decoded from their released 320 k MP3s, because both bakes need layered
vocal stems (`trance-hook-layered.mp3`, `trancepenta-hum-layered.mp3`,
`.ac-dot-stamp-vocal.mp3`) that are gone from the tree, the Shelf, and the
Space. Re-singing them would produce a different record. `masters/manifest.json`
and each kit manifest carry a `lossy` flag per track so this never becomes
invisible.

## Kunaki

Kunaki opens the account when the first product is created, so product
creation is a signed-in browser step; the HTTP API begins once a
10-character product ID exists. Record that ID in the kit's `manifest.json`
under `vendor.productId`, then:

```sh
node pop/physical/bin/kunaki-order.mjs quote --quantity 3
node pop/physical/bin/kunaki-order.mjs place --shipping "…" --quantity 3
KUNAKI_ALLOW_LIVE=1 node pop/physical/bin/kunaki-order.mjs place --shipping "…" --quantity 3 --live
node pop/physical/bin/kunaki-order.mjs status
```

Orders are test orders unless `--live` is given with `KUNAKI_ALLOW_LIVE=1`.
Credentials and the recipient live in `~/.config/kunaki/order.json`, never
in the repository. A live order manufactures and bills; Kunaki does not
cancel one.
