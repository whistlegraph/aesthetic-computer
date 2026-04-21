# LACMA 2026 — Video Demo Script

**Target length:** 3:30–4:30 (under 5 min)
**Filming goal:** one continuous take where possible; the "instrument" framing rewards unedited real-time demos.
**Jeffrey on camera OR voiceover:** either works. Voiceover over screen/laptop footage is probably easier to land cleanly.

---

## Shot list & script

### 0:00–0:20 — Cold open: the claim

**Visual:** A closed laptop on a plain surface. USB stick plugged in. Held for a beat. Then hands press power.

**VO (Jeffrey):**
> What if your computer were a musical instrument? Not *had* a music app — but *was* an instrument. This is Aesthetic Computer, and it boots from this USB stick in under two seconds.

---

### 0:20–0:45 — Boot sequence

**Visual:** Power on. Brief BIOS flash. Black. Then the framebuffer comes alive — AC Native logo, then the default piece (*notepat*) appears. Time it so the boot is visible end to end with no cut.

**VO:**
> No desktop. No browser. No operating system in the way. The Linux kernel hands control to a single piece of art software, running straight on the framebuffer at sample-level audio precision.

---

### 0:45–1:30 — *notepat* as instrument

**Visual:** Hands on the keyboard. Play a short melody — maybe a rising arpeggio, a chord. Show the waveform selector (swap sine to square). Show the room-reverb. Show the time-of-day tint in the background visuals (if filming late, point at how the screen color reflects the hour).

**VO:**
> *notepat* turns any keyboard into a polyphonic synthesizer. Eight and a half thousand lines of handmade code. Every key is a pitch. Every chord is a real polyphonic voice through ALSA at 192 kilohertz. The background color tracks the time of day — this laptop knows what hour you're making music in.

---

### 1:30–2:30 — KidLisp: the language

**Visual:** Exit *notepat*, drop into the prompt. Type a short KidLisp program — something visual and quick, e.g.:
```
(wipe black)
(repeat 60 i
  (ink (rainbow))
  (line (/ width 2) (/ height 2) (* i 10) (* i 6)))
```
Show it rendering. Edit one number (change `60` to `200`, or swap `rainbow` for `red`) and watch it update live.

**VO:**
> This is KidLisp — a tiny programming language I wrote specifically for making art. One hundred and eighteen built-in functions. No build step. No imports. The entire program fits on a card. More than sixteen thousand KidLisp programs have already been written on the platform.

---

### 2:30–3:15 — The `code` command (Anthropic hook)

**Visual:** At the prompt, type `code`. Claude Code launches directly on the framebuffer. Ask it something small and specific — e.g. *"add a second line mirrored across the center"*. Let it think, show the diff, apply. Re-run the piece. Show it's now a symmetric pattern.

**VO:**
> And because this is a creative system, not a closed one — Anthropic's Claude Code runs as a first-class command. I can ask it to modify a piece while the piece is still running. The AI coding partner lives inside the instrument, not outside it.

---

### 3:15–3:45 — The network

**Visual:** Phone in frame. Open `aesthetic.computer` in a mobile browser. Scroll through a few pieces, tap into one. Then show the QR code — someone scans it on a second device (or just you showing both screens). Briefly show chat / the handle `@jeffrey`.

**VO:**
> The same pieces run in any browser on any device. Three hundred and seventy-one built-in pieces. Two hundred sixty-five more published by users. Twenty-eight hundred registered handles. Everything is URL-addressable, QR-shareable, and real-time multiplayer-capable.

---

### 3:45–4:15 — Close

**Visual:** Back to the laptop, running *notepat*, the room-tint showing the hour. Jeffrey's hands lift off the keys. Hold.

**VO:**
> The proposal is simple. With LACMA Art + Technology Lab support, we'll bring this system into the museum — as workshops, as an installation, and as USB drives that visitors can take home. Because the personal computer isn't finished being designed. It's still an instrument. And we're still tuning it.

**End card:** `aesthetic.computer` · LACMA Art + Technology Lab · 2026

---

## Production notes

**Audio:**
- *notepat* audio should be captured cleanly from the laptop's line-out if possible; if using room mic, test levels first (synth can clip).
- Record the VO separately in a quiet room — don't rely on the camera mic for narration.

**Visual:**
- Frame the laptop screen so text is legible at 1080p. Avoid glare from overhead lights on the screen surface.
- For the `code` / Claude Code section: if the terminal text is too small, zoom in afterwards in the edit. Don't try to solve it by cranking the font size mid-demo.
- The USB stick should be visible in the cold open — it's a key prop for the "boots from this" moment.

**Pacing:**
- Don't narrate every action. Let the boot happen silently — the two-second boot *is* the demo.
- Cut any dead air longer than ~1.5 seconds.

**Backup plan if filming runs tight:**
- Cut the "network" section (3:15–3:45) — the application text already covers it. Drops runtime by 30s.
- Skip the waveform/reverb detail in *notepat* — just play the melody and move on.

**Hosting:**
- Upload unlisted to YouTube or Vimeo. Link in Submittable form. Make sure the link is set before 11:58 PM PST tomorrow.
