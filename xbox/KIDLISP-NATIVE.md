# Native KidLisp on Xbox

The Native BIOS can run a deliberately small KidLisp subset by compiling the
published source on the trusted host into the BIOS's existing sandboxed
QuickJS lifecycle. This avoids shipping a second interpreter and does not add
`fetch`, DOM, filesystem, process, Device Portal, or WinRT access to pieces.

The first supported proof is the live `$obk` program by `@fifi`:

```lisp
lightblue
stamp #j8t
blur 4
```

Its complete external dependency closure is the painting `#j8t`. The host
fetches `$obk` only from
`https://aesthetic.computer/api/store-kidlisp?code=obk`. The Xbox resolves
`#j8t` only through
`https://aesthetic.computer/media/paintings/j8t.png`; redirects may reach AC's
media storage, but a piece cannot supply a URL.

## Supported subset v1

- bare named-color wipes: `black`, `white`, `red`, `green`, `blue`, `lightblue`
- `wipe color` or `wipe r g b`
- `stamp #code [x y scale]`, with 1-8 alphanumeric painting codes
- `blur strength` from 0-48; the compiler applies KidLisp's divide-by-three
  kernel scaling and the runtime caps the resulting radius at 16

Unsupported forms fail compilation rather than silently changing the artwork.
Published source is limited to 50 KB and 256 forms. The native image cache is
limited to eight entries, each compressed response to 8 MiB, and each decoded
image to a maximum 1024-pixel side. Native blur is a separable CPU box blur.

The current renderer batches drawing by primitive type, so subset v1 supports
one final blur pass per frame. That is faithful to `$obk`; broader KidLisp
command ordering, transforms, animation/timing, nested `$code` composition,
audio, and the rest of the browser runtime remain future work.

## Deploy over the wire

Install Native BIOS revision 13 or newer, load the Device Portal environment
from the vault as usual, then run:

```bash
node xbox/tools/live.mjs deploy-kidlisp '$obk'
```

The tool fetches and validates the current published source, emits a bounded JS
piece in memory, uploads it as `LocalState/live-piece.js`, launches the app, and
prints telemetry. A successful console run includes:

```text
AC_NATIVE_JS KIDLISP_BOOT $obk forms=3 paintings=j8t
AC_NATIVE_PAINTING_READY key=#j8t ...
AC_NATIVE_CPU_FRAME ... images=1 blur=1 ...
```

Portable preflight:

```bash
sh xbox/test-native.sh
```
