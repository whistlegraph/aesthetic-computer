# HP 7585B

A0/E-size 8-pen drafting plotter (1983) driven via HP-GL over RS-232 or HP-IB.
Workflow: SVG → vpype HPGL exporter → serial pipe to the plotter.

## One-time setup

### 1. Install vpype

```fish
pipx install "vpype[all]"   # or: brew install pipx && pipx install vpype
```

### 2. Register this device profile

Either link the bundled config from your home dir:

```fish
ln -sf ~/aesthetic-computer/plotters/hp7585b/vpype.toml ~/.vpype.toml
```

…or pass it explicitly on every invocation: `vpype --config ~/aesthetic-computer/plotters/hp7585b/vpype.toml ...`.

### 3. Wire up the plotter

- Default RS-232: **9600 8N1**, hardware (Xon/Xoff) flow control. DIP-switch
  configurable on the rear panel — verify before first plot.
- macOS USB-to-serial adapters appear as `/dev/cu.usbserial-*`. Export
  `HP7585B_TTY` (the helpers below read it) so you don't have to repeat the
  path:

  ```fish
  set -Ux HP7585B_TTY /dev/cu.usbserial-XXXX
  ```

- Set Metric/Imperial mode on the front panel to match the paper you're
  loading (the vpype profile says which is required per size).

## Doing a test print

```fish
# Convert SVG to HPGL on A3 metric
vpype read test-print.svg \
      write -d hp7585b -p a3 test-print.hpgl

# Send it
hp7585bplot test-print.hpgl
```

A small canned `test-print.svg` lives next to this README — it draws a frame,
diagonals, a circle, and a couple of pen-swap text labels so you can verify
clip limits and the carousel.

## Helpers

Source these from your fish config (or symlink into `~/.config/fish/functions/`):

- `hp7585bplot file.hpgl` – cats an HPGL file to `$HP7585B_TTY` with the
  right `stty` settings.
- `hp7585bsend "PU;PA0,0;"` – send a raw HP-GL fragment.
- `hp7585bhome` – pen up, pen home (`PU;PA0,0;`).
- `hp7585bpenup` / `hp7585bpendown` – `PU;` / `PD;`.
- `hp7585bpen N` – select pen N from the carousel (`SP N;`), 0 stows.
- `hp7585binit` – issue a fresh `IN;` (reinitialize plotter state).

## References

- HP 7585B Interfacing & Programming Manual (PN 07585-90002, Aug 1983) —
  scanned at bitsavers.
- HP Computer Museum — 7585B entry.
- vpype HPGL cookbook — schema for the TOML profile in this directory.
