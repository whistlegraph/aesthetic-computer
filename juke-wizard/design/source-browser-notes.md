# Menu Band Juke source browser

`Neo` → `Aesthetic` → `Spotify` → `Apple Music`

- `Neo` is the current computer name, shortened from `Jeffrey’s MacBook Neo`.
- `Aesthetic` contains cloud releases. Publish and account actions live inside this tab.
- Spotify and Apple Music each own their search, connection state, attribution, and errors.
- Browsing another tab never changes the playing track. Only Play changes the persistent transport.
- No global Cloud button and no mixed `All` tab.
- DJ, waveform editing, notes, and local-file actions appear only where the source permits them.

## FOSS case studies

| Project | Source model | Keep | Avoid |
| --- | --- | --- | --- |
| [Music Assistant](https://developers.music-assistant.io/) | Filesystem, SMB, Spotify, YouTube Music, and other services implement the same music-provider contract. | One provider interface with explicit capabilities. | Pretending every provider supports the same playback or mutation features. |
| [OwnTone](https://owntone.github.io/owntone-server/control-clients/web/) | One browser and queue span local music, files, radio, podcasts, and Spotify. | Persistent playback while browse context changes. | Burying source identity after content enters the queue. |
| [Moosync](https://github.com/Moosync/Moosync-electron) | Local, Spotify, and YouTube share one desktop shell with source filters. | Provider-scoped search and one visual grammar. | Its repository is archived; use it as interaction precedent, not a dependency. |
| [Strawberry](https://github.com/strawberrymusicplayer/strawberry) | Local collection and streaming services live in separate navigation branches. | Clear source boundaries and service-specific setup. | Making remote providers feel like secondary utilities in a dense sidebar. |

## Integration boundaries

- Apple Music is viable as a first-class macOS source through [MusicKit for Swift](https://developer.apple.com/documentation/musickit): catalog search, library access, and playback are supported after user authorization. Menu Band needs the MusicKit App Service and `NSAppleMusicUsageDescription` before the tab ships.
- Spotify stays a browse/playback source. Its [developer policy](https://developer.spotify.com/policy) prohibits mixing, remixing, overlapping, or integrating Spotify content with another service, so Menu Band must not expose Spotify tracks to DJ decks or effects.
- Aesthetic and local files can keep the full Juke toolset because Menu Band controls those media paths.
