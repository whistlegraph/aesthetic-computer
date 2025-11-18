# Game Boy Audio-Visual Synchronization Analysis

## Current Implementation Status

### melody.c Architecture (Hybrid Rendering)
- **Background Layer**: White bars for all 48 notes (drawn once)
- **Sprite Layer**: Color overlays + letter sprites for visible notes
- **Timing**: Frame counting (24 frames @ 60Hz = 0.4s per note)
- **Scrolling**: Auto-scroll to follow current note
- **Audio**: NR13/NR14 register writes to trigger notes

### Current Timing Method
```c
frame_counter++;
if(frame_counter >= FRAMES_PER_NOTE) {
    frame_counter = 0;
    advance_to_next_note();
    play_sound();
    update_scroll();
}
```

## Problems with Current Approach

### 1. **Timing Drift**
- Frame counting assumes perfect 60 Hz vsync
- No verification that vsync is actually 60 Hz
- Audio APU runs independently from frame counter
- Over time, small errors accumulate = drift

### 2. **Audio-Visual Desynchronization**
- Visual update happens FIRST (frame counter check)
- Audio trigger happens SECOND (NR14 write)
- Gap between visual and audio = 1 frame minimum
- If audio has internal delays, gap increases

### 3. **No Feedback Loop**
- System doesn't measure actual audio playback timing
- Can't detect if audio is lagging or leading visuals
- No correction mechanism

## Game Boy APU Architecture (from Pandocs)

### Key Timing Components

#### 1. **DIV-APU Counter (512 Hz)**
- Increases when DIV register bit 4 goes from 1→0
- **Critical**: This is the MASTER CLOCK for all APU timing
- Frequency: 512 Hz (1.953125 ms per tick)
- Powers three timing systems:

| Event | DIV-APU Ticks | Frequency | Purpose |
|-------|---------------|-----------|---------|
| Length Timer | 2 | 256 Hz | Auto-shutoff channels |
| Envelope Sweep | 8 | 64 Hz | Volume changes |
| CH1 Freq Sweep | 4 | 128 Hz | Frequency changes |

#### 2. **Period Counter (Per Channel)**
- Channels 1&2 (Pulse): Clocked at 1048576 Hz (1/4 dots)
- Channel 3 (Wave): Clocked at 2097152 Hz (1/2 dots)
- Channel 4 (Noise): Configurable via NR43

#### 3. **APU-PPU Synchronization**
> "The APU runs off the same Master Clock as the rest of the Game Boy, which is to say, it is fully synced with the CPU and PPU."

**This is the KEY insight!**

### APU Timing Formula (Channels 1 & 2)

```
Sample Rate = 1048576 / (2048 - period_value) Hz
Tone Frequency = Sample Rate / 8 = 131072 / (2048 - period_value) Hz
```

For example, middle C (261.63 Hz):
```
period_value = 2048 - (131072 / 261.63) ≈ 1547 = $60B
```

## Perfect Synchronization Strategies

### Strategy 1: DIV-APU Driven Architecture (RECOMMENDED)

**Concept**: Let the APU's DIV-APU counter drive BOTH audio and visuals

#### Implementation:
```c
// Store last DIV-APU state
uint8_t last_div_state = 0;
uint8_t div_apu_counter = 0;
const uint8_t TICKS_PER_NOTE = 205; // ~0.4s at 512 Hz

void update() {
    vsync();
    
    // Check DIV bit 4 for falling edge (DIV-APU tick)
    uint8_t current_div = DIV_REG & 0x10;
    if(last_div_state && !current_div) {
        // DIV-APU tick occurred
        div_apu_counter++;
        
        if(div_apu_counter >= TICKS_PER_NOTE) {
            div_apu_counter = 0;
            
            // Advance note THEN trigger audio immediately
            current_note_index++;
            trigger_audio();  // NR14 write happens in same tick
            update_scroll();
        }
    }
    last_div_state = current_div;
    
    // Visual update every frame
    update_sprite_overlays();
}
```

**Advantages**:
- ✅ Uses the SAME clock that drives audio timing
- ✅ Zero drift - APU and visuals are locked together
- ✅ 512 Hz resolution = very precise (1.95 ms per tick)
- ✅ Can sync to envelope/length timer events
- ✅ Works identically at all CPU speeds (normal/double)

**Disadvantages**:
- ⚠️ Requires monitoring DIV register
- ⚠️ Slightly more complex logic

**Note Duration Examples** (at 512 Hz):
- 0.4 seconds = 205 ticks (current melody timing)
- Quarter note @ 120 BPM = 0.5s = 256 ticks
- Eighth note @ 120 BPM = 0.25s = 128 ticks
- Sixteenth note @ 120 BPM = 0.125s = 64 ticks (envelope sweep rate!)

### Strategy 2: Length Timer Driven Architecture

**Concept**: Use NRx1 length timer to auto-advance notes

#### Implementation:
```c
void play_note(uint8_t note, uint8_t duration_256hz_ticks) {
    // Set note frequency
    uint16_t period = note_to_period(note);
    NR13_REG = period & 0xFF;
    
    // Set length timer (auto-shutoff)
    NR11_REG = (0b10 << 6) | (64 - duration_256hz_ticks);
    
    // Trigger with length timer ENABLED
    NR14_REG = 0x80 | 0x40 | ((period >> 8) & 0x07);
}

void update() {
    vsync();
    
    // Check if channel turned off (NR52 bit 0)
    if(!(NR52_REG & 0x01)) {
        // Channel auto-shutoff = note finished
        current_note_index++;
        play_note(melody[current_note_index], note_duration);
        update_scroll();
    }
    
    update_sprite_overlays();
}
```

**Advantages**:
- ✅ APU automatically times note duration
- ✅ Perfect sync - visual checks when audio actually stops
- ✅ Very simple logic
- ✅ Leverages hardware timer

**Disadvantages**:
- ⚠️ 256 Hz resolution only (less precise than DIV-APU)
- ⚠️ Limited to 64 ticks max (~0.25 seconds)
- ⚠️ Can't have notes longer than 250ms without workarounds
- ⚠️ Melody has 0.4s notes = too long for single length timer

**Workaround for long notes**: Use multiple retriggering with same note

### Strategy 3: Hybrid DIV + NR52 Monitoring

**Concept**: Use DIV-APU for timing, verify with NR52 channel status

```c
uint8_t last_div_state = 0;
uint8_t div_apu_counter = 0;
const uint8_t TICKS_PER_NOTE = 205;
bool audio_playing = false;

void update() {
    vsync();
    
    // Monitor DIV-APU
    uint8_t current_div = DIV_REG & 0x10;
    if(last_div_state && !current_div) {
        div_apu_counter++;
    }
    last_div_state = current_div;
    
    // Check channel status
    bool channel_active = NR52_REG & 0x01;
    
    if(div_apu_counter >= TICKS_PER_NOTE && audio_playing) {
        // Time elapsed AND audio confirmed playing
        div_apu_counter = 0;
        current_note_index++;
        trigger_note();
        update_scroll();
    } else if(!channel_active && audio_playing) {
        // Audio stopped unexpectedly - resync
        trigger_note();
        div_apu_counter = 0;
    }
    
    audio_playing = channel_active;
    update_sprite_overlays();
}
```

**Advantages**:
- ✅ Self-correcting - detects desync
- ✅ Handles edge cases (channel off, envelope mute, etc.)
- ✅ Best of both worlds

**Disadvantages**:
- ⚠️ Most complex implementation

## Architectural Recommendations

### Phase 1: DIV-APU Implementation (PRIORITY)
**Goal**: Eliminate timing drift by using APU's master clock

**Files to modify**:
- `melody.c`: Replace frame counter with DIV-APU counter
- Add DIV monitoring logic
- Calculate TICKS_PER_NOTE for 0.4s = 205 ticks

**Expected outcome**: Perfect sync, zero drift over time

### Phase 2: Visual-to-Audio Delay Elimination
**Goal**: Ensure visuals update AFTER audio trigger confirmation

**Current flow**:
```
frame_counter++ → check → advance note → visual update → audio trigger
```

**Better flow**:
```
DIV-APU tick → advance note → audio trigger → visual update (same frame)
```

**Implementation**:
```c
// In update loop
if(div_apu_counter >= TICKS_PER_NOTE) {
    current_note_index++;
    trigger_audio();  // Audio FIRST
    update_scroll();  // Then visuals
}
update_sprite_overlays();  // Always update sprites
```

### Phase 3: Envelope-Based Note Shaping (ENHANCEMENT)
**Goal**: Use NR12 envelope for more musical note articulation

**Current**: Notes are square waves at constant volume
**Enhancement**: Use envelope to add attack/decay

```c
void trigger_note(uint8_t note) {
    uint16_t period = note_to_period(note);
    
    // Configure envelope: start at volume 15, decrease, pace 2
    NR12_REG = 0xF2;  // Volume 15, decrease, pace 2 (128ms decay)
    
    NR13_REG = period & 0xFF;
    NR14_REG = 0x80 | ((period >> 8) & 0x07);
}
```

**Result**: Notes fade out slightly, sound more natural

### Phase 4: PCM Register Visualization (DEBUG TOOL)
**Goal**: Display actual APU output for debugging sync issues

**CGB-only registers**:
- `FF76 (PCM12)`: Read digital output of CH1/CH2
- `FF77 (PCM34)`: Read digital output of CH3/CH4

**Debug visualization**:
```c
// Display actual audio waveform on screen
void debug_display_waveform() {
    uint8_t pcm12 = *(uint8_t*)0xFF76;
    uint8_t ch1_output = pcm12 & 0x0F;
    uint8_t ch2_output = (pcm12 >> 4) & 0x0F;
    
    // Draw waveform sprite or tile at screen position
    // Can verify audio is actually playing when expected
}
```

## Timing Calculations

### Converting Tempo to DIV-APU Ticks

Given a tempo in BPM (beats per minute):
```
Seconds per beat = 60 / BPM
Ticks per beat = (60 / BPM) * 512 Hz

Example: 120 BPM
Ticks per quarter note = (60 / 120) * 512 = 256 ticks
Ticks per eighth note = 128 ticks
Ticks per sixteenth note = 64 ticks
```

### Current Melody Timing
```
24 frames @ 60 Hz = 0.4 seconds
0.4 * 512 Hz = 204.8 ticks ≈ 205 ticks

// Use this in DIV-APU implementation:
const uint8_t TICKS_PER_NOTE = 205;
```

### Musical Note Frequencies to Period Values

```c
// Helper function
uint16_t note_to_period(uint8_t note) {
    // Formula: period = 2048 - (131072 / frequency_hz)
    // For GBDK note constants (C3, D3, etc.), frequencies are:
    const uint16_t freq_table[] = {
        131, 147, 165, 175, 196, 220, 247,  // C3-B3
        262, 294, 330, 349, 392, 440, 494,  // C4-B4
        523, 587, 659, 698, 784, 880, 988   // C5-B5
    };
    
    uint8_t index = note - C3;
    if(index >= sizeof(freq_table)/sizeof(freq_table[0])) return 0;
    
    return 2048 - (131072 / freq_table[index]);
}
```

## Testing & Verification

### Test 1: DIV-APU Stability
**Measure**: Count DIV-APU ticks over 10 seconds
**Expected**: Exactly 5120 ticks
**Verifies**: DIV-APU is running at stable 512 Hz

### Test 2: Audio-Visual Sync Drift
**Measure**: Start playback, let run for full 48 notes
**Expected**: Last note visuals and audio end simultaneously
**Verifies**: No accumulated drift

### Test 3: Frame Drop Resilience
**Measure**: Intentionally skip vsync() calls randomly
**Expected**: DIV-APU timing unaffected, still syncs
**Verifies**: Independence from frame rate

### Test 4: NR52 Channel Status
**Measure**: Read NR52 bit 0 every frame, log state changes
**Expected**: Bit flips exactly when notes start/stop
**Verifies**: Channel status accurately reflects playback

## Implementation Priority

### MUST HAVE (Phase 1)
1. ✅ Switch from frame counter to DIV-APU counter
2. ✅ Calculate correct TICKS_PER_NOTE (205 for 0.4s)
3. ✅ Trigger audio immediately when DIV-APU counter expires

### SHOULD HAVE (Phase 2)
4. ⚠️ Add NR52 monitoring for channel status verification
5. ⚠️ Ensure visual update happens AFTER audio trigger
6. ⚠️ Test on real hardware (emulator timing may differ)

### NICE TO HAVE (Phase 3+)
7. 💡 Add envelope shaping for musical articulation
8. 💡 PCM register visualization for debugging
9. 💡 Support variable tempo (BPM-based timing)
10. 💡 Multi-channel harmony (use CH2 for bass line)

## Hardware Differences to Consider

### Game Boy vs Game Boy Color
- **DIV-APU**: Same on both (512 Hz)
- **APU Clock**: Identical master clock sync
- **Length Timer**: Same 256 Hz frequency

### Game Boy Advance
- **APU Emulation**: Digital mixing instead of analog
- **DAC Behavior**: Different (DACs are always "on" in emulation)
- **HPF Aggressiveness**: More aggressive than GBC
- **CH3 Behavior**: Inverted DAC output, can cause pops

**Recommendation**: Test on DMG, GBC, and GBA separately

## Code Structure Proposal

```
src/melody.c:
├── Audio System
│   ├── note_to_period()      // Frequency conversion
│   ├── trigger_note()         // NR1x register writes
│   └── is_channel_active()    // NR52 monitoring
│
├── Timing System
│   ├── div_apu_tick()         // DIV register monitoring
│   ├── should_advance_note()  // Timing logic
│   └── reset_timing()         // For manual control
│
├── Visual System
│   ├── update_sprite_overlays()
│   ├── update_scroll()
│   └── draw_background_bars()
│
└── Main Loop
    └── update()               // Orchestrates all systems
```

## References

1. **Pan Docs - Audio Overview**: https://gbdev.io/pandocs/Audio.html
   - APU architecture, common concepts, triggering, timing

2. **Pan Docs - Audio Details**: https://gbdev.io/pandocs/Audio_details.html
   - DIV-APU counter (512 Hz master clock)
   - Length timer, envelope sweep, freq sweep timing
   - Channel behavior, DAC specifics

3. **Pan Docs - Audio Registers**: https://gbdev.io/pandocs/Audio_Registers.html
   - NRx0-NRx4 register details
   - Trigger mechanism, period values
   - NR52 channel status bits

4. **GBDK Documentation**: 
   - `gb/gb.h` - Hardware register defines
   - Sound register manipulation examples

## Next Steps

1. **Read melody.c current timing implementation**
2. **Implement DIV-APU monitoring function**
3. **Replace frame_counter with div_apu_counter**
4. **Test sync accuracy over full playback**
5. **Profile timing on emulator vs real hardware**
6. **Document any remaining drift or issues**

---

**Status**: Analysis complete, ready for implementation
**Expected Impact**: Zero timing drift, perfect audio-visual sync
**Risk**: Low - DIV-APU is stable, well-documented, hardware-synchronized
