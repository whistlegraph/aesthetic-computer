# Lectures

Local archive of lecture video/audio and machine-readable transcripts for AC-related talks.

## Conventions

Each lecture stores files under a single slug prefixed with the YouTube video
ID (when available) so the origin is obvious:

```
<video-id>-<slug>.mp4   // highest-quality MP4 (gitignored, local-only)
<video-id>-<slug>.mp3   // audio-only MP3 for offline/quick playback (gitignored)
<video-id>-<slug>.vtt   // raw YouTube captions (VTT)
<video-id>-<slug>.md    // clean prose transcript with frontmatter
```

The large binaries (`.mp4`, `.webm`, `.mkv`, `.mp3`, `.m4a`, `.wav`) are
gitignored; regenerate them locally with `yt-dlp` using the `source_url` in the
markdown frontmatter.

For each lecture that should appear on papers.aesthetic.computer, a
corresponding cleaned `.txt` lives under
`system/public/assets/papers/readings/text/` and is linked from
`system/public/papers.aesthetic.computer/platter.html` — this is the same
pattern used for other long readings (e.g. the Goodiepal Equation documentary
transcript).

## Regenerating a lecture from scratch

```bash
cd papers/lectures
yt-dlp --write-auto-subs --write-subs --sub-langs 'en.*' --skip-download \
  --output "%(id)s-%(title)s.%(ext)s" <youtube-url>
yt-dlp -f 'bv[ext=mp4]+ba[ext=m4a]/b[ext=mp4]/b' \
  --output "%(id)s-%(title)s.%(ext)s" <youtube-url>
yt-dlp -x --audio-format mp3 \
  --output "%(id)s-%(title)s.%(ext)s" <youtube-url>
```

Then slug-ify filenames and convert the VTT to prose (dedupe rolling captions,
strip inline `<c>` timing tags, reflow into paragraphs).
