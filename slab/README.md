# slab

A lid-closed ambient + reactive audio system for the MacBook, gated on Claude
Code activity, with a completion chime that sleeps the Mac when the last
thread finishes.

Close the lid while Claude is working (with sleep suppressed) and the laptop
becomes a "sleeping slab": an ambient C-major-pentatonic bed loops through
the speakers while a mic-listener picks up claps, snaps, shushes, kisses,
hums, and close-up sings — responding in real time with short pentatonic
pluck-arpeggios that mirror each sound's pitch contour. Close the lid with
nothing running and it's silent. When Claude finishes a prompt, it beeps
once per remaining active thread; when the last thread completes, the
"all-done" chime plays — and if the lid is closed, the Mac sleeps right
after.

Active threads are tracked per session via marker files in
`$SLAB_HOME/state/active-prompts/<session_id>`: the `UserPromptSubmit` hook
creates one, the `Stop` hook removes it. The daemon reads this directory to
decide whether to run ambient.

## What it does

| Event | Sound |
|---|---|
| Lid close (`disablesleep=1`) | display sleep. Ambient only if a Claude prompt is active. |
| Claude prompt submitted while lid closed | ascending C-arp + ambient loop starts |
| Mic transient while ambient is playing (> 2 kHz) | 4-note pentatonic pluck arp, contour mirrors input |
| Lid open while ambient playing | descending ding + pitch-up stinger + ambient stops |
| Lid open with no ambient | silent |
| Claude Stop, **other active prompts remaining** | N ascending beeps (C6 D6 E6 G6 A6 C7 D7 E7) |
| Claude Stop, all prompts done, lid open | "all-done" chime |
| Claude Stop, all prompts done, lid closed | "all-done" chime → `pmset sleepnow` |
| User submits new prompt | touches active-prompts marker, sets `disablesleep=1` |

## Install

```sh
git clone git@github.com:whistlegraph/aesthetic-computer.git
cd aesthetic-computer/slab
./install.sh
```

The installer will:

1. Symlink scripts into `~/.local/bin/`
2. Copy sound assets into `~/.local/share/slab/sounds/`
3. Build a Python venv at `~/.local/share/slab/venv/` with `numpy` + `sounddevice`
4. Install the launchd agent `~/Library/LaunchAgents/computer.slab.daemon.plist`
5. Merge hooks into `~/.claude/settings.json` (Stop, SubagentStop, UserPromptSubmit, SessionStart)
6. Install a passwordless-sudo rule at `/etc/sudoers.d/slab-pmset` for `pmset` (prompts for password once)

Opt-outs: `--no-hooks`, `--no-sudoers`.

## Usage

```sh
claude-sleep awake     # disable all sleep (stay awake with lid closed)
claude-sleep auto      # restore normal sleep behavior
claude-sleep now       # sleep the Mac immediately
claude-sleep status    # show SleepDisabled state
```

Before using lid-closed ambient mode: `claude-sleep awake`. The first time the mic listener runs you'll get a macOS microphone permission prompt.

## Uninstall

```sh
./uninstall.sh           # unload agent, remove symlinks + hooks + sudoers
./uninstall.sh --purge   # also delete ~/.local/share/slab (sessions, logs, venv, sounds)
```

## Layout

```
slab/
├── bin/                                 # scripts (symlinked into ~/.local/bin/)
│   ├── lid-ambient.sh                   # launchd daemon, polls lid + active prompts
│   ├── lid-reactive.py                  # mic → pluck-arp synth (Python)
│   ├── lid-ambient-generate.py          # generate ambient.wav
│   ├── claude-sleep                     # sleep-state toggle
│   ├── claude-stop.sh                   # Stop-hook entry
│   ├── claude-prompt-log.sh             # UserPromptSubmit hook (touches active-prompts marker)
│   ├── claude-ping-repeat.sh            # legacy 30s pings (no longer invoked)
│   ├── claude-sleep-schedule.sh         # legacy delayed auto-sleep (no longer invoked)
│   ├── slab-monitor.sh                  # resource sampler
│   └── slab-zone                        # location-zone manager
├── sounds/                              # WAV assets (lid chimes, pings, beeps)
├── launchd/
│   └── computer.slab.daemon.plist.template
├── sudoers.d/
│   └── slab-pmset.template
├── settings-fragment.json               # Claude Code hooks, merged on install
├── install.sh
└── uninstall.sh
```

Runtime state lives under `~/.local/share/slab/`:

```
sessions/<stamp>-<zone>.wav    per-lid-close recording of Python-generated output
sessions/<stamp>-<zone>.jsonl  trigger events + location metadata
logs/lidalive.log              daemon transitions
logs/reactive.log              reactive listener triggers
logs/resources.jsonl           CPU/RSS samples every 15s
logs/claude-stop.log           Stop-hook activity
config/zones.json              geofenced zones + per-zone scale/dynamics
state/active-prompts/<id>      one file per Claude session with a prompt in flight
state/last-location.json       cached coords (if Location Services unreachable)
venv/                          Python venv for the reactive listener
sounds/                 installed sound assets (+ regenerated ambient.wav)
```

## Zones (location-aware ambient)

Each lid-close queries Core Location and picks the closest zone in
`~/.local/share/slab/config/zones.json`. The matching zone overrides the
reactive listener's scale, pitch mapping (`div_factor`), and amp. Coords +
zone name are written into the session JSONL and appended to the WAV
filename.

```sh
brew install corelocationcli                 # one-time (install.sh can run it)
# grant Location Services permission once
slab-zone where                              # show current coords + zone
slab-zone add home 150                       # pin current spot as 'home', 150m radius
slab-zone add studio 60
slab-zone list                               # dump zones.json
slab-zone remove home
```

Available scales: `major_pentatonic`, `minor_pentatonic`, `blues`,
`dorian`, `phrygian`, `lydian`, `whole_tone`, `chromatic`. Edit
`zones.json` directly to tweak a zone's `scale`, `div_factor`, or
`arp_amp`.

## Tuning

Most knobs live at the top of each script:

- **Ambient** — `bin/lid-ambient-generate.py`: scale, note-gap range, duration/fade ranges, detuning, drone frequency. Run `python3 bin/lid-ambient-generate.py [seed]` to regenerate a variation.
- **Reactive listener** — `bin/lid-reactive.py`: `HIGH_BAND`, `TRIGGER_RATIO`, `MIN_GAP`, `DIV_FACTOR`, `NOTE_DUR`, `PLUCK_TAIL`, `ARP_NOTES`, `ARP_AMP`. Scales live in `SCALE_INTERVALS`.
- **Lid-poll interval** — `bin/lid-ambient.sh`: `POLL` (default 0.5s).
- **Resource poll** — `bin/slab-monitor.sh`: `INTERVAL` (default 15s).

Edits to files in `slab/bin/` take effect immediately because the installed copies are symlinks back into the repo.

## Requirements

- macOS (Apple Silicon tested). Intel should work but the display-sleep-on-lid-close path depends on `pmset displaysleepnow`.
- Homebrew, Python 3.11+, `jq`.
- Microphone permission for the reactive listener (prompted on first run).

## Notes

- `pmset disablesleep 1` suppresses **all** sleep, including explicit `pmset sleepnow` — the sudoers rule lets the scheduler toggle it back off before suspending.
- The reactive listener writes 44.1 kHz mono int16 WAVs of only its own output (the pluck arps). The looping ambient `afplay` and the chimes aren't captured — that would need a loopback audio device like BlackHole.
- Session files accumulate forever by default. Purge periodically: `rm ~/.local/share/slab/sessions/*`.
- Apple Silicon MacBooks have no accelerometer, gyroscope, or accessible CPU temp via public APIs — so shake/gesture and temperature-modulation paths aren't implemented here yet.
