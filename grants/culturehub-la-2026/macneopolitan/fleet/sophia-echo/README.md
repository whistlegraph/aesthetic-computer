# Sophia: voices around the room

22 actual prepared vocal phrases travel through seats0,1,2,4,3,5 (left front,
right front, right rear, center rear, left rear, held center). Each primary
phrase has half-beat and one-beat echoes at the next two seats,40% and20% of
primary gain. The original three Mac singers retain their parts.

116 quiet sine notes add octave and fifth support. Maximum simultaneous synth
events across the complete arrangement is9, plus one decoded vocal stream per
laptop. Room lights follow vocal destinations; held d041 follows its own vocal
arrivals. No strobe channels. Master volume follows the shared8795 controller.

`build.mjs INPUT OUTPUT` reads `prepared-source.json`, `plan-source.json` and
SHA-verified `assets/<member>/<phrase>.f32`. It writes six float WAVs, raw stems,
a route manifest and the modified plan. Each52.17s stem is faded at the end;
measured sample peaks range0.099–0.131 before master gain. This is not a room
loudness or acoustic synchronization measurement.

`deploy.py INPUT` stages from `INPUT/built`, reading `INPUT/native-source.mjs`
and adding the per-node decoded streams. It requires idle Trio receivers,
checks each transfer hash and waits for fresh decoder readiness. It does not
cue. Never run the original single-center staging script over this arrangement.
The new per-seat files are runtime-only, not persisted to USB.

Frisbee’s `~/Shelf/culturehub-wake-echo/` holds the conductor metadata. Its
`prepared.json` retains the current singer caches, but references the new held
stem; `native-loaded.json` lists all six verified streams. The SUB uses the new
arrangement hash even though its bass notes are unchanged. The initial demo is
at25%. Primary laptop vocals, echo stems and sine notes share that master control.
The three singing Mac OS mixers are separately set to25%. Faces use
`faceAlpha=1` and captions request110pt with automatic width fitting; changing
these fields requires refreshing the prepared singer fingerprint.
