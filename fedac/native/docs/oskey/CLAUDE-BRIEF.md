# oskey — paste this into Claude Code on your MacBook

You are helping me build oskey, my instrument on an AC OS ThinkPad. The ThinkPad is on the same wifi as this MacBook and runs a tiny HTTP server on port 80. Its name is printed on the ThinkPad's splash line as <something>-<animal>.local; you can also find it with `dns-sd -B _http._tcp` (ctrl-c after a second).

The loop:
- Status: `curl http://NAME.local/`
- Get the live piece: `curl http://NAME.local/pieces/oskey.mjs -o oskey.mjs` (do this first, then edit it)
- Push and hot-reload: `curl -X PUT --data-binary @oskey.mjs "http://NAME.local/pieces/oskey.mjs?jump=1"`
- Device log: `curl http://NAME.local/logs | tail -50` (console.log from the piece lands here)
- The drum kit it imports: `curl http://NAME.local/lib/percussion.mjs`
No git, no build, no auth. Lib changes need a device reboot; piece changes reload instantly.

The piece shape (QuickJS on Linux, no browser, no DOM, no npm):
- `export { boot, paint, act, sim, leave }`. paint runs at 60 fps, sim every frame, act gets events.
- act({ event: e, sound, system, screen }): `e.is("keyboard:down")`, `e.key` ("a", "f1", "space" is " ", "escape", "tab", "capslock", "shift", "arrowleft", "insert", "delete", "home", "end"), `e.repeat`; trackpad as a pointer: `e.is("touch")`/`e.is("draw")` with `e.x`, `e.y`; whole-hand contacts on `trackpad.contacts` (x, y 0..1, pressure).
- paint({ wipe, ink, box, line, write, screen }): `wipe(r,g,b)`, `ink(r,g,b[,a])`, `box(x,y,w,h[,"outline"])`, `line(x1,y1,x2,y2)`, `write(text,{x,y,size,font:"font_1"})` (6x10 px per size step), `screen.width/height`. Keep the top-left 20 px clear for the system label.
- Sound: `sound.synth({ type:"sine|triangle|sawtooth|square|noise|piano", tone:Hz, duration:s|Infinity, volume, attack, decay, pan:-1..1, gmProgram:0-127 })` returns a voice; `sound.kill(voice, fadeSeconds)`; `voice.update({tone, volume, pan})`.
- Drums: `import { playPercussion } from "/lib/percussion.mjs"; playPercussion(sound, "c", { volume, pan, pitchFactor })` with c d e f g a b = kick snare clap snap hat-c hat-o ride, c# d# f# g# a# = crash splash cowbell block tambo.
- Samples: `sound.zoo.play("cat", { volume, pan, pitch })` (bird cat cow dog frog horse lion owl pig sheep snake whale). Mic: `sound.microphone.rec()` starts, `sound.microphone.cut()` stops and returns length, then `sound.sample.play({ tone, base: 261.63, volume, pan, loop })` plays the recording pitched.
- Sends: `sound.room.setMix(0..1)` + `sound.room.toggle()`, `sound.echo.setMix(0..1)`, `sound.glitch.setMix(0..1)` + `toggle()`. `sound.bpm(120)`.
- Navigation: `system.jump("prompt")`. Escape three times in oskey goes to the prompt; typing `oskey` there comes back.

Rules: one file, keep it readable, test by pushing and playing, read /logs when something is silent. Describe what you want musically and let Claude map it onto these calls.
