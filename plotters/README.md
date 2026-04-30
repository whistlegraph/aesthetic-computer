# Plotters

Per-plotter helpers and configs. Each plotter has its own subdir with a
README, drivers, and any device profiles it needs. The flat `axi*.fish`
scripts at this level are the AxiDraw helpers (kept top-level for muscle
memory; conceptually they belong to AxiDraw).

## Devices

- **AxiDraw** — flat fish helpers (`axihome`, `axipenup`, `axipendown`,
  `axienable`, `axidisable`, `axicycle`, `axiplot file.svg ...`). Driven via
  `axicli`. See [Examples](#axidraw-examples) below.
- **HP 7585B** — `hp7585b/`. A0/E-size 8-pen drafting plotter, driven via
  vpype (SVG → HPGL) plus serial helpers. See `hp7585b/README.md`.

## Install (fish)

Symlink the helpers you want into `~/.config/fish/functions/` or `source`
them from `config.fish`. For the AxiDraw helpers:

```fish
for f in ~/aesthetic-computer/plotters/axi*.fish
    ln -sf $f ~/.config/fish/functions/(basename $f)
end
```

For the HP 7585B helpers:

```fish
for f in ~/aesthetic-computer/plotters/hp7585b/hp7585b*.fish
    ln -sf $f ~/.config/fish/functions/(basename $f)
end
```

## AxiDraw examples

```
axihome
axipenup
axipendown
axicycle
axiplot drawing.svg
axiplot drawing.svg --speed_pendown 40 --accel 60
```
