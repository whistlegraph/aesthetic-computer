# Max for Live Production Guidelines Summary

Reference: https://github.com/Ableton/maxdevtools/blob/main/m4l-production-guidelines/m4l-production-guidelines.md

## Key Concepts

### Local vs Global Naming
- The 'name space' in Max is **global** - `[send]`, `[receive]`, `[coll]`, `[buffer~]` share data between devices
- Use `---` prefix for local names: `[s ---Cutoff]` becomes `[s 024Cutoff]` (unique per device)
- Signal processing space is independent per device

### Freezing
- Always freeze before distributing (snowflake icon)
- Dependencies are consolidated within the device
- Continue developing from unfrozen version

## Initialization Best Practices

### Avoid Time-Based Initialization
- **Don't use** `[delay]` or `[deferlow]` for initialization order
- **Use** `[trigger]` to keep initialization synchronous
- Time-sensitive operations don't execute when Live's audio engine is off!
- With audio off: `[loadbang]` => `[delay 1000]` => `[print]` - the print only happens after audio is turned on

### Async Operations
- For async processes (file loading), wait for completion callback
- Don't rely on arbitrary delays

## Live API Objects

### `[live.path]`
- Use to get path to Live objects (Song, Track, Device, etc.)
- `path live_set` - gets the Song object
- Output: id (left), type (middle), error (right)

### `[live.observer]`
- Watches properties and outputs when they change
- Connect `[live.path]` id to right inlet
- Bang left inlet to get current value
- Properties: `tempo`, `is_playing`, `current_song_time`

### `[live.object]`
- For getting/setting properties and calling functions
- Use `getattr <property>` to read
- Use `set <property> <value>` to write

### `[live.thisdevice]`
- Outputs when device loads (left outlet)
- Outputs device enabled state (right outlet)

## Sample Rate

### Getting Audio Engine Sample Rate
- Use `[dspstate~]` object
- Outputs: signal on/off (0), vector size (1), **sample rate (2)**, overdrive (3), takeover (4)
- Bang to query current state

### Important Note
- Audio must be ON for `[dspstate~]` to report correct values
- If audio is off, may report 0 or stale values

## Undo History

### Avoid Flooding Undo
- Internal modulations can create many undo events
- Set Parameter Visibility to **Hidden** for internally-controlled parameters
- Don't automate `[live.dial]` etc. from within the patch if set to "Automated and Stored"

## CPU Considerations

### Automation
- Enable **Defer Automation Output** for non-time-sensitive values
- Use higher **Update Limit** values

## UI Guidelines

### Colors
- Use dynamic colors to follow Live themes
- `[live.*]` objects default to dynamic colors

### Device Width
- Keep as narrow as possible
- Use fold-out, tabs, or overlay views for complex UIs

### `[live.*]` Objects
- Use whole pixels for Presentation Rectangle (avoid 4.3562635)
- Set Mouse Up as Output Mode for `[live.text]` buttons

## jweb~ Specifics

### AudioContext Activation
- jweb~ suspends AudioContext by default (no user interaction)
- Must programmatically resume when needed
- Can use `activate 1` message to jweb~

### Communication
- `executejavascript <code>` - run JS in the web view
- Messages from JS go out third outlet of jweb~

## Final Checklist

### Audio
- [ ] Sample rate consistency - works at all sample rates
- [ ] No audio clicks on parameter changes
- [ ] Plays in sync with Live Set (latency compensation)

### Parameters
- [ ] All parameters have meaningful Long Name and Short Name
- [ ] Info Text filled in for all parameters
- [ ] Parameters save/recall correctly with Live Set
- [ ] MIDI mappable

### Robustness
- [ ] No error messages in Max Console on load
- [ ] Multiple instances work simultaneously
- [ ] Works on macOS, Windows, Push 3 Standalone
