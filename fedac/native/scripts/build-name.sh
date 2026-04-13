#!/bin/bash
# build-name.sh — Generate a unique build name (adj-animal-noun).
#
# Primary path: shell out to track-build.mjs which atomically increments
# a Mongo counter and checks for collisions. Names look like
# "swift-otter-ember" — guaranteed unique, no hex suffix.
#
# Fallback path: if Mongo is unreachable (offline dev), generate a
# three-word name from the same pools using git hash + epoch + nanos
# as deterministic-ish entropy. Still no hex suffix.
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACK="${SCRIPT_DIR}/track-build.mjs"

# Try Mongo-backed unique-name generation first.
if command -v node >/dev/null 2>&1 && [ -f "${TRACK}" ]; then
  NAME="$(node "${TRACK}" next-name --plain 2>/dev/null | tr -d '[:space:]' || true)"
  if [ -n "${NAME}" ] && echo "${NAME}" | grep -Eq '^[a-z][a-z0-9-]*-[a-z][a-z0-9-]*-[a-z][a-z0-9-]*$'; then
    echo "${NAME}"
    exit 0
  fi
fi

# ── Offline fallback ──────────────────────────────────────────────
# Pools kept in sync with track-build.mjs (see ADJECTIVES, ANIMALS, NOUNS
# there). Trimmed to the smallest set that still gives reasonable variety
# without bloating this script — full pools live in the JS so update them
# there if you need more entropy.

ANIMALS=(
  otter fox wolf bear lynx puma deer hare mink vole
  hawk eagle falcon kite owl heron crane stork ibis loon
  gecko lizard skink iguana cobra mamba viper python
  trout salmon pike perch bass tuna marlin shark
  crab squid octopus nautilus beetle moth dragonfly firefly
  dragon phoenix griffin sphinx hydra wyvern roc kraken
  raptor rex bronto stego trike ankylo ptero diplo
  pulsar quasar nebula nova comet meteor asteroid aurora
  river brook delta marsh fjord glacier canyon ridge summit
)

ADJECTIVES=(
  swift bold keen sharp bright vivid lucid agile nimble deft
  brave calm clear crisp eager fierce gentle grand hardy jolly
  amber ashen azure blaze bronze cedar coral crimson dusk ember
  flint frost golden hazel indigo ivory jade khaki lemon lilac
  ancient arcane astral cosmic cryptic digital feral hollow lunar mystic
  atomic binary cubic delta fractal harmonic infinite kinetic lattice modular
)

NOUNS=(
  dawn dusk ember cinder spark flame glow flare ash mist
  fog frost rain dew hail haze veil cloud gale storm
  tide wave breeze zephyr eddy gust whirl peak ridge crag
  shard prism crystal jewel orb spiral coil blade arrow spear
  echo hush chime hum tone song chord pulse beat shroud
  halo aura quill thread weave warp loom comet nebula vortex
  core seed root branch leaf vine moss fern bloom petal
)

GIT_HASH=$(git rev-parse HEAD 2>/dev/null || echo "0000000000000000000000000000000000000000")
EPOCH=$(date +%s)
NANO=$(date +%N 2>/dev/null || echo 0)

ADJ_IDX=$(( 0x${GIT_HASH:0:8} % ${#ADJECTIVES[@]} ))
ANIMAL_IDX=$(( (EPOCH + 0x${GIT_HASH:8:4}) % ${#ANIMALS[@]} ))
NOUN_IDX=$(( (10#${NANO:0:6} + 0x${GIT_HASH:12:4}) % ${#NOUNS[@]} ))

echo "${ADJECTIVES[$ADJ_IDX]}-${ANIMALS[$ANIMAL_IDX]}-${NOUNS[$NOUN_IDX]}"
