# Real-time composition loop

The elected bed is a musical genome: chord progression, topline, rhythm, and
rate. A live piece may develop that genome measure by measure, but inference
never enters the audio callback.

## Three clocks

1. **Sample clock:** the C engine renders every audio period. It is sovereign.
2. **Measure clock:** a fixed-size `MeasurePlan` becomes immutable at the bar
   boundary. The engine always holds a committed next measure and a local
   fallback after it.
3. **Context clock:** an agent and data adapters update future plans. Time may
   update each minute; weather no faster than its source, currently five
   minutes in the existing AC weather piece.

At 120 BPM, a 4/4 measure lasts two seconds. While measure N plays, N+1 is
already committed. An agent normally proposes N+2, giving it about two seconds;
a network model should work four to eight measures ahead. A missed deadline
causes no silence: C advances the elected motif through a deterministic local
variation.

## Native boundary

`fedac/native/src/audio.c` already renders voices period by period. HDA uses a
roughly 1 ms period and 4 ms buffer; SOF uses 20 ms and 80 ms to avoid XRUNs.
Add a transport above those voices:

```c
typedef struct {
  uint64_t bar;
  float bpm;
  uint8_t chord_count, note_count, pulse_count;
  ACChord chords[4];
  ACNote notes[32];
  ACPulse pulses[32];
  uint64_t context_hash;
} ACMeasurePlan;
```

A control thread parses plans and writes a double buffer. At a measure
boundary the audio thread atomically swaps one pointer. It never parses JSON,
waits for a process, allocates memory, fetches data, or calls an agent.

## Agent observation and action

The agent receives the elected genome, the last eight measure summaries, the
next committed measure, listener actions, and a minimized context vector. It
may choose only bounded musical operations: retain, transpose a motif degree,
change chord inversion or extension, alter note density, switch an elected
rhythm family, or move rate inside the elected range. Every decision is logged
with its input vector and can be replayed without the model.

Time and weather are slow compositional forces:

| input | bounded musical control |
|-------|-------------------------|
| UTC measure number | shared seed and globally aligned form |
| local hour / sunrise | mode brightness and register |
| cloud cover | chord extensions and note density |
| wind | syncopation and rate within ±4% |
| precipitation | articulation and ornament probability |
| temperature | voicing width, not raw pitch |

Inputs must not directly trigger notes. They bias future-plan choices, are
quantized at measure or section boundaries, and retain the previous valid value
when unavailable.

## Latency experiment

1. Render the elected genome locally with no agent and verify indefinite,
   sample-continuous measure swaps.
2. Feed synthetic context changes and record decision-to-audible latency.
3. Add a deterministic local planner for N+1 and a remote agent for N+4.
4. Compare HDA and SOF using `fedac/native/scripts/latency.mjs`; the success
   condition is zero added XRUNs and zero missed-measure silence.
5. Only then connect live time, Open-Meteo, or listener data.

## Audible prototype

`pop/realtime/c/measure-engine.c` implements the fixed plan queue, atomic
measure-boundary swap, local fallback, and CoreAudio callback. The callback
does not allocate, parse, fetch, lock, or infer. `plan-agent.mjs` currently
stands in for an agent: it reads Los Angeles weather, combines it with a UTC
minute seed, and keeps the engine planned four measures ahead.

```bash
make -C pop/realtime
node pop/realtime/plan-agent.mjs --seconds 180
```
