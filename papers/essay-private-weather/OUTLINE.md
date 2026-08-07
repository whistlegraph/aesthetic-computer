# Private Weather

## Claim

We can grow small, situated intelligences for the desktop without training a large model. A KidLisp organism can combine an inherited pattern, a narrow view of its environment, persistent state, and explicit human selection. In No Paint 3, the painting is itself an Aesthetic Computer piece: an ordered stack whose accepted layers retain both executable code and rendered pixels. Jastow expands the population; Aesthetic Computer remains the place where organisms live, are judged, and are accepted.

## Arc

1. **The cheap surprise**
   - Ertle, Levin, and Scheutz show that arbitrary internal patterns can guide maze exploration.
   - Structured fractals and paintings beat shuffled equivalents in simple mappings.
   - Mixed pattern-and-sensor DQNs outperform sensor-only agents, including on novel mazes.
   - The noise control matters: for the learned agents, uniform noise works about as well as a fractal. The useful mechanism may be extra context or policy diversification, not hidden artistic meaning.

2. **The organisms are already visible**
   - `klpad.lisp` stores four decaying energies; sound and image are one state.
   - `klbutton.lisp` keeps a counter alive across frames, resets on a shared beat, and responds to touch.
   - KidLisp already provides body, clock, senses, memory, and action in a compact, editable score.

3. **Private weather**
   - Add a bounded internal pattern tape to each organism.
   - Sources: Halley fractal, accepted painting scan, AST walk, rhythm sequence, shuffled control, uniform-noise control.
   - The tape is not a command. It changes what the organism is ready to do when the same external state returns.

4. **The painting is a piece**
   - A proposal layer contains code, parameters, seed, runtime version, provenance, and rendered pixels.
   - No discards the candidate code and pixels together.
   - Paint appends the code layer and its pixel result atomically.
   - The composite bitmap is cached for immediate display and export; the layer score remains editable, replayable, forkable, and available for growth.
   - Raster-only imports enter as explicit raster layers, never as falsely recovered source.

5. **Jastow as night garden**
   - Jastow is a one-job-at-a-time RTX 3070 worker, not a public server or canonical brain.
   - Grow populations in bounded batches: mutate valid KidLisp, run deterministic scenes, reject unsafe or inert candidates, return compact candidate packets.
   - Candidate packet: source, parents, mutation, seeds, pattern hash, runtime version, metrics, thumbnail/video.

6. **The desktop as habitat**
   - Inputs are low-resolution and local: beat/time phase, pointer vector, touch/key events, canvas geometry, audio envelope, previous action, nearby organism state.
   - Never ingest chats, files, prompts, contacts, or raw screen/audio by default.
   - The organism must still run when Jastow is offline.

7. **The human jury**
   - Automatic tests enforce validity, runtime, safety, recovery, and behavioral diversity.
   - They do not define taste.
   - Pairwise choice or Yay/Nay decides what reproduces; explicit acceptance alone changes the canonical lineage.

8. **The first crop**
   - Six pattern families, 32 organisms each, three 60-second desktop scenes.
   - Compare structured sources with shuffled and noise controls.
   - Measure coverage, loop avoidance, response and recovery, frame cost, and human keep rate.
   - Falsifiable outcome: if pattern-bearing organisms do not outperform controls or improve the jury's keep rate, remove the pattern channel.

9. **Keep the word small**
   - Do not call animation intelligence.
   - Use the term only for a system that senses, retains state, chooses, changes across outcomes, and leaves descendants through selection.
   - The goal is not a desktop assistant. It is a desktop habitat whose inhabitants remain legible enough to edit and refuse.
