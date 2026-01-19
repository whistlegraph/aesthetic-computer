# Spotify DJ Piece Feasibility Report

**Date:** January 18, 2026  
**Subject:** Feasibility of Streaming Spotify Through an Aesthetic Computer `dj.mjs` Piece

---

## Executive Summary

**Verdict: ❌ NOT FEASIBLE for DJ functionality**

Spotify's Developer Terms and Policy **explicitly prohibit** creating DJ applications that mix, crossfade, or overlap audio tracks. While basic playback control is possible through their Web Playback SDK, the core functionality needed for a DJ app is forbidden.

---

## What Spotify Offers

### 1. Web Playback SDK
- Creates a Spotify Connect device in your browser
- Streams audio directly in the browser
- **Requires Spotify Premium** subscription
- Supported on Chrome, Firefox, Safari, Edge (desktop & mobile)

### 2. Web API
- Search tracks, albums, artists
- Manage playlists
- Get metadata (BPM, key, energy, danceability via Audio Features endpoint)
- Control playback (play, pause, seek, skip)
- Get queue and currently playing

---

## ⛔ Critical Blockers for DJ App

### Explicit Prohibition #1: No Mixing/Crossfading
From the **Developer Policy Section III.7**:
> "Do not permit any device or system to **segue, mix, re-mix, or overlap** any Spotify Content with any other audio content (including other Spotify Content)."

This is a **complete blocker** for DJ functionality. You cannot:
- Crossfade between tracks
- Beat-match and blend songs
- Create mashups or remixes
- Layer multiple Spotify tracks

### Explicit Prohibition #2: No Audio Synchronization
From the **Developer Policy Section III.6**:
> "Do not synchronize any sound recordings with any visual media, including any advertising, film, television program, slideshow, video, or similar content."

This blocks:
- Visualizers synced to the music
- VJ-style audio-reactive graphics (core to an AC piece!)

### Explicit Prohibition #3: No Commercial Hardware/Public Performance
From **Section III.10**:
> "Do not build products or services which are targeted for use by businesses... Spotify is for personal, non-commercial use which means it can't be broadcast or played publicly from a business, such as radio stations, bars, restaurants, stores, dance studios, etc."

### Other Restrictions
- **No webcasting** (Section III.4): Can't stream to multiple simultaneous listeners
- **No games/trivia** (Section III.2): No "guess the song" features
- **No integration with other services** (Section III.5): Can't mix Spotify with SoundCloud, local files, etc.
- **Premium only** (Section IV.1): Streaming only available to Premium subscribers

---

## What IS Technically Possible (But Limited)

If you accept the restrictions, you could build a **very basic "music player" piece** (not a DJ app):

### Possible Features:
1. **Play/Pause/Skip** - Basic transport controls
2. **Seek** - Jump to position in track
3. **Display metadata** - Show cover art, track name, artist
4. **Show audio features** - Display BPM, key, energy, danceability (from API)
5. **Queue management** - Add tracks to play next
6. **Playlist browsing** - Search and browse your library
7. **Visualizations** - BUT only using the frequency data from your own AudioContext output, not synced to the actual audio content (gray area)

### What's NOT Possible:
- ❌ Two-deck mixing
- ❌ Crossfades/transitions
- ❌ Beat-matching
- ❌ Effects (reverb, delay, EQ on Spotify audio)
- ❌ Looping sections
- ❌ Speed/pitch adjustment
- ❌ Integration with local files or other streaming services

---

## Technical Implementation (If You Still Want a Basic Player)

### OAuth Flow
```javascript
// 1. User authorizes via Spotify login
// 2. Get access token
// 3. Initialize Web Playback SDK
const player = new Spotify.Player({
  name: 'Aesthetic Computer',
  getOAuthToken: cb => { cb(accessToken); }
});
```

### Required Scopes
- `streaming` - Required for playback
- `user-read-playback-state` - Read playback state
- `user-modify-playback-state` - Control playback
- `user-read-currently-playing` - Get current track

### Audio Features API (for "visual DJ" feel)
```javascript
// GET https://api.spotify.com/v1/audio-features/{id}
// Returns: tempo (BPM), key, time_signature, energy, danceability, etc.
```

---

## Alternatives to Consider

### 1. **Your Own Audio Files**
AC already has great audio support. Use your own licensed music or royalty-free tracks:
- Full control over mixing, effects, visualization
- No licensing restrictions on manipulation
- Your existing `sound.play()` API works great

### 2. **SoundCloud API** (limited)
- More permissive for some use cases
- Still has restrictions but different model
- Some tracks are streamable via API

### 3. **Web Audio API + Local Files**
Build a true DJ app using:
- Local audio file loading (drag & drop)
- Full Web Audio API processing
- Your existing speaker/audio worklet infrastructure

### 4. **Spotify "Remote Control" Piece**
A piece that controls Spotify playback on another device (like a fancy remote):
- No actual audio in AC
- Just displays now playing + controls
- Uses Spotify Connect to control your phone/desktop app
- Stays within terms (probably)

### 5. **30-Second Preview Clips**
Spotify allows 30-second preview clips without Premium:
- Could build a "discovery" app
- Play previews, not full tracks
- Must link back to Spotify for full play

---

## Legal Risk Assessment

| Action | Risk Level | Notes |
|--------|------------|-------|
| Basic playback only | ✅ Low | Allowed with Premium |
| Display metadata/art | ✅ Low | Required to show it |
| Queue management | ✅ Low | Allowed |
| Non-synced visualizer | ⚠️ Medium | Gray area |
| Crossfading tracks | ❌ High | Explicitly prohibited |
| Beat-matching/mixing | ❌ High | Explicitly prohibited |
| EQ/effects on audio | ❌ High | Modifying content |
| Public performance | ❌ High | Explicitly prohibited |

**Enforcement**: Spotify reviews apps and can revoke API access at any time. They have terminated apps for policy violations.

---

## Conclusion & Recommendation

**For a true DJ piece**: Use local audio files or royalty-free music with AC's existing audio infrastructure. You'll have complete freedom for mixing, effects, and visualization.

**For a Spotify integration**: Build a "now playing" display/remote that controls Spotify on another device without streaming the audio through AC. This keeps you compliant while still offering a Spotify-connected experience.

**The core DJ features you'd want (mixing, crossfading, effects) are explicitly prohibited by Spotify's terms.** There's no legal workaround for this—it's a licensing limitation from their agreements with record labels.

---

---

## How Professional DJ Software Actually Works

### The Standard: Owned Audio Files

Professional DJ software does **NOT** use streaming services for mixing. Here's what's standard:

#### Primary Music Sources for DJs

| Source | Type | Price | Format | DJ-Legal? |
|--------|------|-------|--------|-----------|
| **Beatport** | Download store | $1.29-2.49/track | MP3/WAV/AIFF (DRM-free) | ✅ Yes |
| **Traxsource** | Download store | $1.49-2.49/track | MP3/WAV/AIFF | ✅ Yes |
| **Bandcamp** | Download store | Varies | MP3/FLAC/WAV | ✅ Yes |
| **Juno Download** | Download store | £0.99-1.99/track | MP3/WAV/FLAC | ✅ Yes |
| **iTunes/Amazon** | Download store | $0.99-1.29/track | AAC/MP3 | ✅ Yes |
| **Record Pools** | Subscription | $20-50/month | MP3/WAV | ✅ Yes |
| **Vinyl** | Physical | $8-25/record | Analog | ✅ Yes |

#### Record Pools (DJ Subscription Services)
Record pools are subscription services where labels send promotional tracks to DJs in exchange for feedback and exposure:
- **DJcity** (~$20/month) - Hip-hop, pop, EDM
- **BPM Supreme** (~$20/month) - All genres
- **Heavy Hits** (~$15/month) - House, techno
- **Club Killers** (~$25/month) - Club edits, remixes
- **Digital DJ Pool** (~$15/month) - Various genres

DJs download files outright—no streaming, full ownership.

### DJ Software Architecture

**Major DJ Software:**
- **Serato DJ Pro** - Industry standard for hip-hop/scratch DJs
- **Rekordbox** - Pioneer DJ ecosystem
- **Traktor Pro** - Native Instruments, popular with techno/house DJs
- **VirtualDJ** - Consumer/prosumer, flexible
- **djay Pro** - Algoriddim, Apple ecosystem

**How They Work:**
1. **Local File Library** - You import your owned audio files (MP3, WAV, AIFF, FLAC)
2. **Analysis** - Software analyzes BPM, key, waveform, beat grids
3. **Database** - Metadata stored locally (cue points, loops, playlists)
4. **USB Export** - For club play on CDJs/media players

**Supported File Formats (Serato example):**
- MP3, OGG, ALAC, FLAC
- AIFF, WAV
- WMA, AAC/M4A
- MP4 video

### The USB Stick Workflow

Yes, USB sticks are standard! Here's why:

**Club Setup:**
- Most clubs have **Pioneer CDJ-3000s** or **Denon SC6000s**
- DJs bring USB drives with their library
- Rekordbox/Engine Prime exports playlists + analysis data to USB
- Walk into any club worldwide, plug in, play

**Typical DJ USB:**
- 32GB-256GB drive
- 500-5000 tracks
- Pre-analyzed with BPM, key, beat grids, cue points
- Hot cues and loops saved
- Organized into playlists/folders

### Streaming Integration (Limited)

Some DJ software offers **streaming integration**, but with caveats:

**Beatport LINK** ($15-25/month):
- Stream tracks directly into Rekordbox, VirtualDJ, djay
- Offline cache of 50-100 tracks
- **Full mixing allowed** (unlike Spotify!)
- Works with Pioneer CDJs via Rekordbox

**SoundCloud Go+** (via djay):
- Algoriddim's djay has SoundCloud integration
- Can mix SoundCloud tracks
- More permissive licensing

**TIDAL** (via djay, VirtualDJ):
- Hi-fi quality streaming
- DJ mixing allowed in partner apps
- Not as extensive as Beatport for electronic music

**Why Spotify Can't Do This:**
Spotify's licensing agreements with labels explicitly prohibit:
- Mixing/overlapping tracks
- Time-stretching, pitch-shifting
- Audio manipulation
- Offline caching for DJ use

Beatport, SoundCloud, and TIDAL have **different licensing deals** specifically negotiated for DJ use cases.

### Summary: The DJ Music Pipeline

```
Purchase/Download → Import to DJ Software → Analyze → Organize → Export to USB → Perform
     │                      │
     │                      └── Beatport LINK can skip this step
     │                           (streaming direct to software)
     │
     └── Sources: Beatport, Record Pools, Bandcamp, iTunes, Vinyl rips
```

**Bottom line:** Professional DJs own their music files. Streaming is supplementary at best, and only services with specific DJ licensing (Beatport LINK, not Spotify) support actual mixing.

---

## References

- [Spotify Developer Terms](https://developer.spotify.com/terms) (v10, May 15, 2025)
- [Spotify Developer Policy](https://developer.spotify.com/policy) (May 15, 2025)
- [Web Playback SDK Docs](https://developer.spotify.com/documentation/web-playback-sdk)
- [Web API Docs](https://developer.spotify.com/documentation/web-api)
- [Beatport](https://www.beatport.com/) - DJ-focused music store with LINK streaming
- [Wikipedia: Music Pool](https://en.wikipedia.org/wiki/Music_pool) - Record pool history
