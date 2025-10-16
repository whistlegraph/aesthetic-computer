# Fuzz

Automated fuzzing/generative testing for Aesthetic Computer pieces using browser automation.

## Overview

This directory contains documentation and tools for automating interactions with Aesthetic Computer pieces through Chrome DevTools Protocol (CDP). The goal is to create generative, randomized inputs that explore the creative space of interactive pieces.

## What We've Learned

### Browser Automation with Aesthetic Computer

Aesthetic Computer is a canvas-based, keyboard-driven environment. Traditional DOM manipulation doesn't work well because most interactions happen through keyboard events rather than clickable elements.

**Key insights:**
- Always take a screenshot first to understand the visual state
- Focus on keyboard event dispatching rather than DOM clicks
- Use `document.dispatchEvent()` with proper KeyboardEvent configuration
- Include `bubbles: true` and `cancelable: true` for events to propagate
- Some pieces may require focusing a specific element first (like a textarea)

### Notepat: Musical Fuzzing

Notepat is a musical sequencer piece that responds to keyboard input. We successfully created a fuzzing algorithm that generates randomized musical compositions.

#### Controls

**Notes:** `c`, `d`, `e`, `f`, `g`, `a`, `b`, `h`, `i`, `j`, `k`, `l`, `m`, `n`
- Each key plays a different note in the scale
- Hold duration affects the note length

**Octaves:** `3`, `4`, `5`, `6`, `7`, `8` (number keys)
- Changes the pitch range of subsequent notes
- 6 octave ranges available

**Wavetypes:** `Tab` key
- Cycles through 6 different waveform types
- Changes the timbre/sound character

#### Fuzzing Algorithm

```javascript
async () => {
  const notes = ['c', 'd', 'e', 'f', 'g', 'a', 'b', 'h', 'i', 'j', 'k', 'l', 'm', 'n'];
  const octaves = ['3', '4', '5', '6', '7', '8'];

  let running = true;
  window.stopNotepat = () => { running = false; };

  while (running) {
    // Randomly select octave
    const octave = octaves[Math.floor(Math.random() * octaves.length)];
    document.dispatchEvent(new KeyboardEvent('keydown', {
      key: octave,
      code: `Digit${octave}`,
      keyCode: 48 + parseInt(octave),
      bubbles: true,
      cancelable: true
    }));
    await new Promise(r => setTimeout(r, 30));
    document.dispatchEvent(new KeyboardEvent('keyup', {
      key: octave,
      code: `Digit${octave}`,
      keyCode: 48 + parseInt(octave),
      bubbles: true,
      cancelable: true
    }));
    await new Promise(r => setTimeout(r, 50));

    // Randomly change wavetype (50% chance)
    if (Math.random() > 0.5) {
      document.dispatchEvent(new KeyboardEvent('keydown', {
        key: 'Tab',
        code: 'Tab',
        keyCode: 9,
        bubbles: true,
        cancelable: true
      }));
      await new Promise(r => setTimeout(r, 30));
      document.dispatchEvent(new KeyboardEvent('keyup', {
        key: 'Tab',
        code: 'Tab',
        keyCode: 9,
        bubbles: true,
        cancelable: true
      }));
      await new Promise(r => setTimeout(r, 50));
    }

    // Play random portion of scale
    const goingUp = Math.random() > 0.5;
    const scaleNotes = goingUp ? notes : [...notes].reverse();
    const numNotes = 3 + Math.floor(Math.random() * 6); // 3-8 notes
    const startIdx = Math.floor(Math.random() * (scaleNotes.length - numNotes));
    const notesToPlay = scaleNotes.slice(startIdx, startIdx + numNotes);

    for (const note of notesToPlay) {
      if (!running) break;

      document.dispatchEvent(new KeyboardEvent('keydown', {
        key: note,
        code: `Key${note.toUpperCase()}`,
        keyCode: note.charCodeAt(0),
        bubbles: true,
        cancelable: true
      }));

      // Random hold duration
      await new Promise(r => setTimeout(r, 20 + Math.random() * 40));

      document.dispatchEvent(new KeyboardEvent('keyup', {
        key: note,
        code: `Key${note.toUpperCase()}`,
        keyCode: note.charCodeAt(0),
        bubbles: true,
        cancelable: true
      }));

      // Random gap between notes
      await new Promise(r => setTimeout(r, 10 + Math.random() * 30));
    }
  }
}
```

**Stop the fuzzer:** Call `window.stopNotepat()` in the console

#### Algorithm Features

- **Octave randomization:** Each phrase picks a random octave (3-8)
- **Wavetype variation:** 50% chance to change wavetype between phrases
- **Scale segments:** Plays 3-8 consecutive notes from the scale
- **Direction:** Randomly goes up or down the scale
- **Timing variation:**
  - Note hold: 20-60ms (randomized)
  - Note gap: 10-40ms (randomized)
- **Infinite loop:** Runs continuously until stopped

## Potential Pieces to Fuzz

Here are other Aesthetic Computer pieces that could benefit from fuzzing:

### Drawing/Painting Pieces
- **line:** Random line drawing with varying colors, thicknesses, positions
- **paint:** Brush strokes with randomized colors, sizes, and positions
- **wand:** Magic wand tool with random selections and fills
- **box:** Random rectangles with varying positions, sizes, and colors

### Interactive/Animation Pieces
- **prompt:** Random command entry and navigation
- **recorder:** Automated recording with random durations
- **video:** Random video playback controls
- **microphone:** Audio input fuzzing (if applicable)

### Text/Typography Pieces
- **type:** Random text entry with varying fonts and styles
- **label:** Random label creation with different parameters
- **freaky-flowers:** Text-to-visual with randomized input

### Mathematical/Generative Pieces
- **sotce-graph:** Random data visualization parameters
- **pixels:** Random pixel manipulation patterns
- **3d-line:** Random 3D path generation

## Fuzzing Strategies

### 1. Parameter Exploration
Systematically vary all available parameters to map the creative space:
- Color ranges
- Size/scale variations
- Position randomization
- Timing variations

### 2. Sequence Testing
Generate random sequences of valid commands:
- Mouse movements and clicks
- Keyboard combinations
- Command sequences in the prompt

### 3. Boundary Testing
Push pieces to their limits:
- Maximum values
- Rapid input
- Edge cases
- Invalid combinations

### 4. Emergent Behavior Discovery
Run long fuzzing sessions to discover:
- Unexpected interactions
- Visual glitches (for art!)
- Performance characteristics
- Hidden features

## Tools and Setup

### Chrome DevTools Protocol (CDP)
We use CDP through Claude Code's MCP chrome-devtools server:
- `mcp__chrome-devtools__evaluate_script`: Execute JavaScript
- `mcp__chrome-devtools__take_screenshot`: Capture visual state
- `mcp__chrome-devtools__take_snapshot`: Get DOM state
- Keyboard/mouse event dispatching

### Timing Considerations
- Allow time for animations: 30-50ms
- Network operations: 400-600ms
- Page loads: 2000-2500ms
- Note holds: 20-60ms (for musical pieces)

## Future Directions

### Recording and Playback
- Save interesting fuzzing sequences
- Replay specific patterns
- Create "fuzzing compositions" that can be shared

### Machine Learning
- Train models on successful fuzzing patterns
- Learn piece-specific parameter distributions
- Generate increasingly interesting outputs

### Multi-Piece Fuzzing
- Fuzz the prompt to navigate between pieces
- Create workflows that span multiple pieces
- Discover inter-piece interactions

### Performance Testing
- Stress test pieces with rapid input
- Monitor memory and CPU usage
- Identify optimization opportunities

## Contributing

When fuzzing a new piece:
1. Document the piece's controls and parameters
2. Create a basic fuzzing algorithm
3. Note interesting discoveries or emergent behaviors
4. Add timing recommendations
5. Include stop mechanisms for infinite loops

## See Also

- [Aesthetic Computer Main Repo](https://github.com/digitpain/aesthetic-computer)
- [Aesthetic Computer Docs](https://aesthetic.computer/docs)
- [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
