# GM instrument balance measurement

2356 offline note renders; all128 programs, two seeds, 0.35s and1.5s notes, 10ms attack/120ms release, 44.1kHz. Each program uses its score pitch-range quartiles plus C4. Strongest100ms RMS and sample peaks are measured before effects, mastering, routing and speakers. Existing score GM trims are applied to the reported values.

Median program reference: -14.71dBFS. These are electrical RMS measurements, not LUFS or room loudness; they do not prove which patch the listener heard.

| GM (1-based) | Instrument | Above median | Pitch/envelope spread | Max peak |
|---|---|---:|---:|---:|
| 8 | Clavinet | +7.9dB | 4.1dB | 0.2dBFS |
| 50 | String Ensemble 2 | +6.2dB | 11.1dB | 4.6dBFS |
| 41 | Violin | +5.9dB | 6.1dB | -3.0dBFS |
| 45 | Tremolo Strings | +5.5dB | 6.6dB | -0.1dBFS |
| 49 | String Ensemble 1 | +5.1dB | 11.1dB | 3.0dBFS |
| 111 | Fiddle | +3.9dB | 3.3dB | -4.7dBFS |
| 99 | Crystal | +3.5dB | 10.2dB | 0.4dBFS |
| 56 | Orchestra Hit | +3.4dB | 2.6dB | -1.0dBFS |
| 103 | Echoes | +3.2dB | 10.7dB | -0.6dBFS |
| 51 | Synth Strings 1 | +3.2dB | 8.0dB | 0.9dBFS |
| 43 | Cello | +3.1dB | 5.3dB | -2.7dBFS |
| 44 | Contrabass | +2.9dB | 15.7dB | -1.5dBFS |
| 7 | Harpsichord | +2.9dB | 4.0dB | 1.0dBFS |
| 110 | Bagpipe | +2.7dB | 3.7dB | -4.9dBFS |
| 84 | Chiff Lead | +2.6dB | 5.6dB | -0.5dBFS |
| 42 | Viola | +2.6dB | 6.5dB | -4.4dBFS |
| 52 | Synth Strings 2 | +2.3dB | 7.2dB | 0.9dBFS |
| 116 | Woodblock | +2.3dB | 0.6dB | -1.6dBFS |
| 39 | Synth Bass 1 | +2.3dB | 5.7dB | -5.5dBFS |
| 4 | Honky Tonk | +1.8dB | 5.5dB | 2.7dBFS |

Synth SHA256: `2a0f39506d4c0f20cb81f9cb5c3c1a781a72376682267994c828fabe54fc709c`. Matches local source: True. Prior calibration source hash: `2a0f39506d4c0f20cb81f9cb5c3c1a781a72376682267994c828fabe54fc709c`. The installed OS binary/source match was not independently verified.

No new trims deployed. A single trim per patch does not remove register/envelope variation; next audition should target the outliers at their loudest measured pitches. Positive peaks here are unit-input diagnostics, not evidence of output clipping: actual note gain, spatial gain, master gain and processing follow this stage.

Reproduce from repository root:
```sh
cc -O2 -I fedac/native/src fedac/native/docs/performance/gm-balance-2026-09-24/measure.c fedac/native/src/gm_synth.c -lm -o /tmp/gm-measure
/tmp/gm-measure < fedac/native/docs/performance/gm-balance-2026-09-24/input.txt > /tmp/gm-measurements.csv
```
