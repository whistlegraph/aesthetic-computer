# Plotters

Helper Fish shell functions for controlling an AxiDraw via `axicli`.

Place each `.fish` file in `~/.config/fish/functions/` or `source` them in your `~/.config/fish/config.fish` to make the commands available in your shell.

## Commands
- `axihome` – move to home
- `axipenup` – raise pen
- `axipendown` – lower pen
- `axienable` – enable XY motors
- `axidisable` – disable XY motors
- `axicycle` – run the AxiDraw demo cycle
- `axiplot file.svg [axicli args...]` – plot a file with optional extra `axicli` arguments

## Examples
```
axihome
axipenup
axipendown
axicycle
axiplot drawing.svg
axiplot drawing.svg --speed_pendown 40 --accel 60
```
