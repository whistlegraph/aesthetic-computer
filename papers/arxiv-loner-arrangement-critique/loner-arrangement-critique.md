# Three Kinds of Time

Arrangement critique of `lonerclub`, derived from `lonr — Loner` (2021), composed and originally performed by Camille Klein.

@jeffrey · Aesthetic.Computer / Whistlegraph Dot Org · August 27, 2026

## Abstract

The 97.2-second `lonerclub` release candidate already has a convincing form: Camille Klein's one-sentence whistlegraph is sung three times while a 122 BPM floor continues and the orchestra thickens. Its problem is narrower than “needs a better arrangement.” The three passes are differentiated mainly by accumulation. The middle pass is roughly 1 dB RMS louder than either neighbor; detected onset density doubles from 4.2 to 8.0 events per second between the first two passes but stays flat in the third; and two transition rushes briefly move the spectral centroid from a 0.9–1.3 kHz baseline toward 4 kHz. They sound and measure as white-outs rather than hinges. The revision proposed here gives each pass one job: body, clock, club. It replaces each rush with a five-hit click figure, binds timbral events to specific lyric-marks, reserves low-frequency weight and reverberant width for the last pass, and keeps the direct vocal centered and continuous. The master already meets the studio's −13.5 LUFS release law; the next improvement is arrangement, not more mastering.

## 1. One sentence is enough form

`Loner` is one line: “sitting curled up in myself, I think of a stone, just waiting very patiently for time to pass.” Each lyric segment is also a drawn gesture. `lonerclub` turns that 30-second score into a three-pass club record. Repetition is therefore not a shortage of material. It is the material.

The release candidate's strongest decisions are already settled: the first sung word arrives at 0.84 seconds; the beat does not stop; the direct voice remains the narrative center; vocal arpeggiation has been removed; and the form ends after the third complete utterance rather than padding itself to a conventional runtime. Those constraints give the revision a clear task: make each return of the sentence change the listener's relation to time.

The proposed reading is:

1. **Body (0:00–0:31.83):** private time, curled inward.
2. **Clock (0:31.83–1:03.30):** measured time, waiting becomes pulse.
3. **Club (1:03.30–1:33.20):** shared time, the room moves with her.
4. **Stamp (1:33.20–1:37.20):** authorship after the sentence, not a fourth section.

That is a stronger orchestration story than quiet / fuller / fullest because the difference survives without counting tracks.

## 2. Project context

A whistlegraph binds visual, musical, and textual layers one-to-one: a sung phrase produces a drawn gesture, and the finished image is a score. Prior Aesthetic Computer work describes the score as an interface rather than an illustration. `lonerclub` extends the form, so its arrangement should not merely accompany the source song; it should disclose the same structure at a larger timescale.

The relevant studio doctrine is equally direct: composition is the arrangement of inspectable instruments, and a process that cannot be scored is not yet known. Sampling is treated as a first-class authored object rather than opaque infrastructure. The consequence for this track is that new layers earn their place only when their role can be named.

## 3. Method and snapshot

The critique fixes one reproducible object: `pop/loner/out/lonerclub-v4pid-release.wav`, modified 2026-08-27 10:41:54 EDT, SHA-256 `cfaed165…d6bec7b0`. The stereo 48 kHz master is 97.2 seconds. FFmpeg EBU R128 analysis reports −13.5 LUFS integrated, 2.3 LU loudness range, and −3.2 dBFS true peak.

The form was read against `wordclock.json`, the regulated 122 BPM note chart, the tracked C score, the release record, the active `neo:vod` arrangement history, and the rendered review-score timeline. The analysis script measures RMS level, spectral centroid, onset strength, detected onset count, stereo correlation, and side/mid energy. These descriptors support the musical reading; they do not decide whether the music is good.

## 4. Evidence: the build peaks early

| Pass | Time | RMS dBFS | Onsets/s | Median centroid | Side/mid |
|---|---:|---:|---:|---:|---:|
| Body | 0.00–31.83 | −13.3 | 4.2 | 929 Hz | −24.4 dB |
| Clock | 31.83–63.30 | −12.3 | 8.0 | 1,085 Hz | −16.2 dB |
| Club | 63.30–93.20 | −13.1 | 8.1 | 1,265 Hz | −19.4 dB |
| Stamp | 93.20–97.20 | −28.7 | 2.8 | 608 Hz | −13.6 dB |

The first pass is deliberately narrow and comparatively sparse. The second succeeds at opening the room: it becomes roughly twice as onset-dense and substantially wider. But it is also the loudest pass, and the third does not introduce a new rhythmic rate. The finale gets brighter, not more active, and narrows again relative to the middle. This is why adding another instrument now would not clarify the arc. The last pass needs a distinct obligation.

The two rushes are the clearest local fault. The first occupies approximately 30.0–31.83 seconds; the second 61.4–63.30 seconds. In two-second windows their median spectral centroids rise to about 3.8 and 4.0 kHz while the surrounding track sits near 0.9–1.3 kHz. Both span nearly the whole audible band. The second also begins before the final “pass” has fully released at 61.66 seconds. The fill announces the edit more strongly than the word that motivates it.

## 5. Revision score

### 5.1 Replace the rushes

Use one centered, filtered click voice and five onsets across the transition bar:

`x..x..x...x..x..`

At 122 BPM this spaces the hits by 3, 3, 4, and 3 sixteenth-note pulses, then leaves the last two pulses empty. Start after the vowel release. Remove the snare body; high-pass around 1.5–2 kHz, low-pass around 7 kHz, use a short room, and keep the final click below the vocal's consonant energy. The figure should imply a clock coming into focus, not a drum machine accelerating out of control.

### 5.2 Give each pass one exclusive device

- **Body:** direct voice, kick, quiet low piano, and pluck shadow. Let hats wait until `pa / tient / ly`; those three syllables already correspond to the three hair lines.
- **Clock:** introduce the click figure, swung/pitched hats, and the wider room. Keep it 0.5–1 dB below its current RMS or remove one persistent midrange layer so it does not become the record's loudest act.
- **Club:** introduce the wub and glass-bell decay as the only new families. Weight the floor below 120 Hz and widen only reverberant tails. The direct vocal stays centered. One counterline at `time / pass` is enough.

### 5.3 Orchestrate the drawing

The strongest possible arrangement rule is already in the source score:

- `stone` gets mass: bass sustain, less hat information.
- `pa / tient / ly` gets three discrete articulations, not a roll.
- `time` gets continuity: a held piano or glass tone that preserves the vocal contour.
- `pass` gets decay and clearance: no transition begins until its vowel has released.

This turns instrumentation into a reading of the work instead of a decoration around it.

### 5.4 Keep one human foreground

The current decision to remove phrase-long harmonies and vocal arpeggiation is correct. Keep one take and one stable direct position for all three passes. If harmony returns, confine it to the last vowels of the final `time` and `pass`, aligned to the singer rather than quantized against her. Let the wub duck 2–3 dB under voiced frames with a 200–260 ms release; the vocal should bend the club, not compete with it.

### 5.5 Make the stamp inherit the song

The four-second stamp tail is about 15 dB RMS below the body of the track. Its quietness is useful, but it risks reading as attached metadata. Let the last glass decay or A♯/C♯ pitch survive beneath it so the stamp is an epilogue in the same room. Do not add another beat.

## 6. Mastering boundary

The release-law master is already healthy: −13.5 LUFS, −3.2 dBFS true peak, no need to chase level. A 2.3 LU loudness range is compact for a track whose story is escalation, but compression is not the right correction. The figure shows that macro-energy is nearly level until the final lyric falls away. Create contrast by removing layers before doors, narrowing the first pass, and reserving low-frequency weight and long tails for the last. Re-run the same master after the arrangement is locked.

## 7. Acceptance test

The next render passes when:

1. The end of each `pass` is audible before any click.
2. Each pass can be named body, clock, or club without looking at a track list.
3. The finale feels larger although it is not louder than the middle.
4. Phone, laptop, mono, and full-range playback all keep the direct vocal in front.
5. Removing any new layer makes one of those claims fail. Otherwise remove it.

## 8. Ethics and limits

`Loner` was composed and originally performed by Camille Klein; the arrangement and release metadata must preserve that attribution. This critique does not authorize a synthetic clone of her voice. Any such use requires explicit consent and clear labeling.

The measurements describe one local render, not a listener study. Spectral brightness, onset count, and stereo width expose differences; they do not explain groove or emotional effect. The active `vod` session may produce a newer candidate after this snapshot. The hashes above define what this paper actually critiques.

## 9. Conclusion

`lonerclub` does not need more material. It needs sharper duties for the material it has. Keep the three complete utterances, keep the continuous floor, and let the arrangement move from body to clock to club. The fastest improvement is also the smallest: replace the broadband rushes with five clicks and leave air after `pass`. Then lower the middle's claim on the climax, make the final wub and glass decay answer the lyric, and stop. The track's subject is waiting; its arrangement improves when it learns to wait.
