# 🎄 Merry Pipeline System

The `merry` command allows you to chain pieces together in a pipeline, similar to the `tape` system but for sequential playback with automatic transitions.

## Visual Progress Bar

When a merry pipeline is running, you'll see a **segmented colored progress bar at the top of the screen** (1px tall) that shows:

- **Completed pieces**: Full bright color (steady)
- **Current piece**: Bright color with pulsing effect, filling left to right
- **Upcoming pieces**: Dim preview showing what's next

Each segment's width is proportional to its duration, so you can see at a glance:
- Which piece you're currently on
- How much time is left in the current piece
- What pieces are coming up next
- The overall pipeline structure

### Example Visualization
```
[===green===][=>cyan......][...orange...][....pink....]
 ^completed   ^current      ^upcoming     ^upcoming
   tone:3s     clock:5s      wand:2s       line:4s
```

The progress bar uses a 6-color palette that cycles:
1. Bright Green
2. Cyan  
3. Orange
4. Pink
5. Purple
6. Yellow

## Basic Usage

### Default Duration (5 seconds per piece)
```
merry tone clock
```
This will:
1. Play `tone` for 5 seconds
2. Automatically transition to `clock` for 5 seconds
3. Return to `prompt`

### Custom Durations
```
merry tone:3 clock:5 wand:2
```
This will:
1. Play `tone` for 3 seconds
2. Automatically transition to `clock` for 5 seconds
3. Automatically transition to `wand` for 2 seconds
4. Return to `prompt`

### Mixed (Custom + Default)
```
merry tone:3 clock wand:2
```
This will:
1. Play `tone` for 3 seconds
2. Automatically transition to `clock` for 5 seconds (default)
3. Automatically transition to `wand` for 2 seconds
4. Return to `prompt`

- pieces without an explicit duration default to 5 seconds

### Prefix Durations (Shorthand)
```
merry 3-tone 5-clock 2-wand
```
- Numbers before the dash indicate seconds
- Mix and match with suffix syntax if you like

### Infinite Loop (`merryo`)
```
merryo 0.25-tone
```
- Repeats the pipeline forever until you run `stop`
- Useful for ambient loops or focused practice sessions
- The progress bar keeps cycling each time it wraps

## Commands

- **`merry piece1 piece2 ...`** - Start a merry pipeline with default 5-second durations
- **`merry piece1:duration1 piece2:duration2 ...`** - Start with custom durations (in seconds)
- **`merry 2-piece1 5-piece2`** - Alternate shorthand using numeric prefixes
- **`merryo piece1 piece2`** - Loop the sequence until stopped
- **`merry:stop`** or **`stop`** - Stop the current merry pipeline early and return to prompt
- **`tape merry piece1:duration1 piece2:duration2 ...`** - Record the entire pipeline (auto-calculates duration)

## Recording with Tape

You can record an entire merry pipeline using tape:

```
tape merry tone:3 clock:5 wand:2
```

This will:
- Automatically calculate total duration (3 + 5 + 2 = 10 seconds)
- Record the entire pipeline
- Show both progress bars (merry at top, tape at bottom)
- Automatically cut the tape when merry completes
- Jump to the video piece for export

See [TAPE_MERRY_INTEGRATION.md](TAPE_MERRY_INTEGRATION.md) for full details.

## Examples

### Simple Demo
```
merry tone clock
```

### Your Original Request
```
merry tone:3 clock:5
```
- Tone piece for 3 seconds
- Clock piece for 5 seconds
- Returns to prompt

### Longer Pipeline
```
merry line:2 oval:2 rect:2 wand:3
```
- Shows different geometric pieces in sequence

### Stopping Early
If a merry pipeline is running and you want to stop it:
```
stop
```
or
```
merry:stop
```

## Technical Notes

- Default duration: **5 seconds** per piece
- Custom durations are specified in **seconds** (not milliseconds)
- The system automatically handles transitions between pieces
- The pipeline state is stored in `system.merry`
- After the last piece finishes, you're automatically returned to `prompt`
- Use `stop` or `merry:stop` to interrupt a running pipeline at any time

## Implementation Details

The merry system:
1. Parses the command to extract pieces and durations
2. Builds a pipeline array with `{ piece, duration }` objects
3. Uses `setTimeout` to chain pieces together
4. Calls `jump(piece)` for each transition
5. Cleans up and returns to `prompt` when complete
