# toolchain/macos

Host-specific helpers that only make sense on a macOS machine.

## sticky.mjs — the "sticky the X" workflow

Drops text into a macOS **Stickies** note, styled the way @jeffrey
likes it: **translucent**, **larger text**, **sized to fit the content
closely**, and **centered on screen**.

When @jeffrey says *"sticky the &lt;thing&gt;"* on a macOS host, run this
on that thing.

```bash
node toolchain/macos/sticky.mjs path/to/file.txt   # note from a file
node toolchain/macos/sticky.mjs --text "a note"    # note from a string
echo "a note" | node toolchain/macos/sticky.mjs    # note from stdin
```

Flags: `--bigger N` (font bumps over the Stickies default, default 3),
`--opaque` (skip translucency), `--no-center` (leave it where it lands).

Needs **Accessibility** permission for the host terminal — the script
drives Stickies with `osascript` / System Events keystrokes (⌘N, ⌘V,
font bumps, ⌥⌘T). If the paste lands empty, the content is still on the
clipboard; just ⌘V into the note by hand.
