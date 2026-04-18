# slab

A lid-closed ambient + reactive audio system for the MacBook, plus Claude Code completion sounds, prompt logging, and auto-sleep.

When the lid closes (with sleep suppressed) the laptop becomes a "sleeping slab": an ambient C-major-pentatonic bed loops through the speakers while a mic-listener picks up claps, snaps, shushes, kisses, hums, and close-up sings — responding in real time with short pentatonic pluck-arpeggios that mirror each sound's pitch contour. When Claude Code finishes a prompt, it beeps once per remaining active thread; when all threads are done, it chimes an "all-done" arp, then — if nobody's returned after 2 minutes — plays a dreamy descending sleep-tone and suspends the Mac.

## What it does

| Event | Sound |
|---|---|
| Lid close (`disablesleep=1`) | ascending C-arp + ambient loop starts + display sleep |
| Mic transient while closed (> 2 kHz) | 4-note pentatonic pluck arp, contour mirrors input |
| Lid open | descending ding + pitch-up stinger + ambient stops |
| Claude Stop, **other threads remaining** | N ascending beeps (C6 D6 E6 G6 A6 C7 D7 E7) |
| Claude Stop, all done | "all-done" chime. If lid closed → repeat pings every 30s + 2-min sleep timer |
| 2 min after Stop with lid still closed | sleep tone + `pmset sleepnow` |
| User submits new prompt | cancels pings + sleep timer, sets `disablesleep=1` |

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
│   ├── lid-ambient.sh                   # launchd daemon, polls lid state
│   ├── lid-reactive.py                  # mic → pluck-arp synth (Python)
│   ├── lid-ambient-generate.py          # generate ambient.wav
│   ├── claude-sleep                     # sleep-state toggle
│   ├── claude-stop.sh                   # Stop-hook entry
│   ├── claude-ping-repeat.sh            # repeating 'done' pings
│   ├── claude-sleep-schedule.sh         # delayed auto-sleep
│   ├── claude-prompt-log.sh             # UserPromptSubmit hook
│   └── slab-monitor.sh                  # resource sampler
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
sessions/<stamp>.wav    per-lid-close recording of Python-generated output
sessions/<stamp>.jsonl  trigger events for that session
logs/lidalive.log       daemon transitions
logs/reactive.log       reactive listener triggers
logs/resources.jsonl    CPU/RSS samples every 15s
logs/claude-stop.log    Stop-hook activity
venv/                   Python venv for the reactive listener
sounds/                 installed sound assets (+ regenerated ambient.wav)
```

## Tuning

Most knobs live at the top of each script:

- **Ambient** — `bin/lid-ambient-generate.py`: scale, note-gap range, duration/fade ranges, detuning, drone frequency. Run `python3 bin/lid-ambient-generate.py [seed]` to regenerate a variation.
- **Reactive listener** — `bin/lid-reactive.py`: `HIGH_BAND`, `TRIGGER_RATIO`, `MIN_GAP`, `DIV_FACTOR`, `NOTE_DUR`, `PLUCK_TAIL`, `ARP_NOTES`, `ARP_AMP`, `PENT_MIDI`.
- **Claude ping interval** — `bin/claude-ping-repeat.sh`: `INTERVAL` (default 30s).
- **Auto-sleep delay** — `bin/claude-stop.sh`: the `120` argument to `claude-sleep-schedule.sh`.
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
