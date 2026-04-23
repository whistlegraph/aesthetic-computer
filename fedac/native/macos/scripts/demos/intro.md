# title: AC Native — intro walkthrough
# voice: Samantha
# rate: 160
# handle: jeffrey
# city: Los Angeles
# hour: 13
# window: 1280x800
# duration: 22.5
# subtitles: true
# narration: true
#
# Canonical demo for lacma-2026 + general intro videos. Everything below
# is read by the `--demo` parser in main.c: keys get injected, say events
# shell out to macOS `say`, captions render as burn-in subtitles.

# ─── Boot greeting ───────────────────────────────────────
[0:00.5] say   hi @jeffrey

# ─── Type 'notepat' slowly, say each letter ──────────────
[0:02.0] say n
[0:02.0] key n
[0:03.0] say o
[0:03.0] key o
[0:04.0] say t
[0:04.0] key t
[0:05.0] say e
[0:05.0] key e
[0:06.0] say p
[0:06.0] key p
[0:07.0] say a
[0:07.0] key a
[0:08.0] say t
[0:08.0] key t
[0:09.5] key enter

# ─── In notepat: say "press C", play C ──────────────────
[0:12.5] say     Press the C key to play a C note
[0:12.5] caption Press 'C' to play a C note
[0:15.3] key     c

# ─── Back to prompt via triple-escape ───────────────────
[0:15.8] say     Now let's go back to the prompt
[0:15.8] caption Back to the prompt
[0:16.0] key     escape
[0:16.2] key     escape
[0:17.7] key     escape

# ─── Type 'off' and let shutdown animation fire ─────────
[0:18.5] say o
[0:18.5] key o
[0:19.5] say f
[0:19.5] key f
[0:20.5] say f
[0:20.5] key f
[0:20.8] key enter
# prompt.mjs calls system.poweroff() → bye @jeffrey animation plays
