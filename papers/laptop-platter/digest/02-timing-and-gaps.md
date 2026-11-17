# 02 · Timing and gaps

**Shelf:** keystroke-timing · **Consumers:** `arxiv-laptop-performance` §8,
`aesel/src/keystroke-timing.mjs`

## The numbers

| Measure | Value | Source |
|---|---|---|
| Median interkey interval, transcription | 177 ms (vs 560 ms two-choice RT) | Salthouse 1984/1986 |
| Alternate-hand advantage | 30–60 ms (typewriters); 5–20 ms (modern) | Salthouse; Dhakal 2018 |
| Mean IKI, 168,960 people | 238.7 ms, SD 111.6, floor ~60 ms | Dhakal 2018 |
| Fast typists | ~120 ms, SD 11 ms; rollover on 40–70% of keys | Dhakal 2018 |
| Hold time | 116 ms, nearly constant across speed | Dhakal 2018 |
| Planning pause / P-burst boundary | 2 s (200 ms for motor vs language) | Chenoweth & Hayes 2001; Inputlog |
| Groove microtiming | about ±50 ms | Senn et al. 2016 |
| Movement timing | starts 6× as variable as ends | LNR 1982 |

## On a musical scale

- 120 ms is sixteenth notes at 125 BPM, and the 60 ms floor is thirty-second
  notes at the same tempo.
- 239 ms is eighth notes at 126 BPM.
- For a typed beat, jitter in the key-to-sound path matters more than the mean.
  `arxiv-latency` measures a 3–4 ms mean, but no jitter yet.

## What Aesel does with it

`aesel/src/keystroke-timing.mjs`. The TUI records when each key arrived, how
many characters it produced, and each erase. It never records which key. A read
that delivers many characters at once counts as a paste. Terminals report
presses only, so there is no hold time.

When a message is sent, the record becomes a summary:

- IKI p10, median and p90, and the log-SD
- pauses of 2 s or more, placed by character count
- bursts, erases and pasted characters
- time from the end of the reply to the first key
- the last 96 intervals, in 10 ms steps

Every engine takes the summary as `startTurn(text, {cadence})` and
`steer(text, {cadence})`: Codex app-server, the Claude CLI, the AC hosted
models and the remote relay. To turn it off, use `/cadence off` or
`AESEL_CADENCE=off`.

## Open questions

- **Biometric.** Killourhy & Maxion show that timing identifies typists, so the
  cadence is a weak biometric. It goes to the same provider that receives the
  text.
- **Does a model use it?** Untested. The study would run the same messages
  with and without cadence and have blind raters score the replies.
