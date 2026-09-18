# Donkey work actions

`work.json` contains 48 hand-choreographed clips: six each for drawing, painting, writing, coding, music, paper, Game Boy, and thinking. These are new action sequences assembled from the existing 16 raster poses, not 48 newly drawn animations. No atlas pixels were changed or generated.

The renderer crops columns 0–45 of each 64×64 cell in `assets/aesel.png`; the original baked-in easel is excluded. Poses 0–1 rest, 2–3 attend, 8–11 work, and 12–15 run. Props and effects supply activity-specific details the small shared pose set cannot express alone. The choreography below describes the intended combined pose/transform/prop rendering; the JSON does not independently draw those props.

Frames use milliseconds, atlas-pixel x/y offsets, rotation in degrees, and dimensionless scale. Missing transforms mean x=0, y=0, rotate=0, flip=false, scaleX=scaleY=1. Each frame replaces the previous frame's values; omitted props/effects clear them. Rotate/scale/flip around the donkey's ground anchor, while translations offset that anchor. Looping clips repeat; finite clips finish in their last pose until the controller chooses another action. States constrain scheduling, not the application's actual progress or success: a decorative check effect must not assert a real build/test result.

Props (14): `brush`, `pencil`, `eraser`, `palette`, `ruler`, `keyboard`, `paper`, `book`, `note`, `drum`, `gamepad`, `cartridge`, `magnifier`, `gear`.

Effects (8): `ink` (a short mark), `paint` (small colored dabs), `spark` (brief star), `thought` (small bubble), `note` (rising music glyph), `dust` (eraser crumbs), `scan` (moving inspection line), `check` (a small decorative tick). Effects should clear or reset per frame, respect reduced motion, and remain within the companion scene.

Every clip has at least six frames and three distinct visual states. IDs are prefixed `work-` to avoid collisions with other sets. No two clips have the same sequence after duration values are removed. Differences include stroke paths, prop exchanges, direction changes, held inspection beats, and preparation/result poses; they are not renamed timing variants.

| Action | Choreography |
|---|---|
| Trace a contour | Follow an arch, lower the pencil, then inspect its endpoint. |
| Crosshatch | Short descending strokes followed by a reverse diagonal pass. |
| Stipple | Three separated pencil taps at different positions, then lift. |
| Erase a mark | Lean into a brisk horizontal scrub and brush away the crumbs. |
| Measure a line | Place the ruler, sight along it, mark each end, and remove it. |
| Draw a spiral | Widen a small wrist circle into a larger looping stroke. |
| Mix a color | Dip low, stir in both directions, and compare the raised palette. |
| Lay a wash | Broad left-to-right sweep, drop a row, then sweep back. |
| Dab highlights | Load paint once, then make two light bouncing dabs. |
| Paint an edge | Hold the brush upright and climb a narrow vertical edge. |
| Flick paint | Wind the brush back, flick forward, recoil, and admire the spray. |
| Clean the brush | Lower the brush, swish it twice, shake it out, and put it away. |
| Write a sentence | Advance across a line, pause for a word, and return to the margin. |
| Start a paragraph | Read the page, indent, then settle into three writing beats. |
| Cross out a phrase | Inspect, make one firm strike, then write a correction above it. |
| Outline an idea | Place three staggered list entries and trace their structure. |
| Proofread | Scan three lines downward, double back, and mark one correction. |
| Sign the page | A low, sweeping signature ends in a lifted flourish. |
| Type code | Alternate key presses, lean in for a longer chord, and read back. |
| Track a bug | Sweep the magnifier, stop at a clue, then make one targeted fix. |
| Compile | Seat a gear, turn through four phases, and lift at completion. |
| Run checks | Tap to start, watch two results, fix once, and confirm. |
| Rearrange code | Lift a block from one side, turn, place it, and reconnect. |
| Compare versions | Alternate between two sides before reconciling in the middle. |
| Tap a beat | A downbeat, two light taps, and a held rest form a small rhythm. |
| Pick a melody | Rise through three pitches, pause, then answer with a lower note. |
| Conduct | Down, left, right, up: a four-beat conducting pattern. |
| Listen closely | Tilt toward the sound, close the eyes briefly, then nod to it. |
| Arrange a sequence | Place three notes at distinct steps, then read the whole bar. |
| Record a take | Count in with two nods, play a phrase, then stop recording. |
| Read a source | Lean into a book, follow a passage, and look up to consider it. |
| Add a citation | Check the book, note a detail, return to the page, and insert it. |
| Lay out a figure | Measure the frame, place the image, and draw its lower caption. |
| Work an equation | Write, hold a long thinking beat, erase one term, then resolve it. |
| Review annotations | Read a note, check the page, add a margin mark, and tick it off. |
| Gather pages | Reach to each side, stack pages low, press them flat, then lift. |
| Insert a cartridge | Lift a cartridge, align it, press down, and wait for the screen. |
| Test the D-pad | Lean left, right, up, and down while checking directional input. |
| Test A and B | Two separate button presses, then a deliberate two-button chord. |
| Place pixel tiles | Set a tile at each corner of a small grid, then inspect it. |
| Build a ROM | Type, turn the compiler gear, place the cartridge, then present it. |
| Playtest a level | A short run-in, two active control beats, a jump, and a result check. |
| Ponder | Tip the head, settle inward, then slowly look back up. |
| Try a hypothesis | Look up at an idea, sketch a test, inspect it, then nod. |
| Weigh alternatives | Hold attention on each side, hesitate in the middle, then choose. |
| Find an idea | Still concentration becomes a small squash, spring, and bright landing. |
| Plan the next steps | Consider a blank page, point out three steps, and trace the route. |
| Reconsider | Start a mark, pull back, erase it, and face a fresh direction. |
