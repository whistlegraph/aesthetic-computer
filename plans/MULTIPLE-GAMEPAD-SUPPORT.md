# Multiple Gamepad Support - Implementation Plan

## Current State Analysis

### `lib/gamepad.mjs` (Already Multi-Gamepad Ready ✅)
The existing implementation **already supports multiple gamepads**:
- Uses `navigator.getGamepads()` which returns array of all connected gamepads
- Iterates through all gamepads with `forEach((gamepad, gi))`
- Stores separate state per gamepad index in `deviceData[gi]`
- Events include gamepad index: `gamepad:${gi}:button:${bi}:${action}`
- Already captures `gamepadId` (device name/type) in events

**No changes needed** - the low-level implementation is complete.

---

## What Needs To Be Added

### 1. **Separate Event Streams Per Gamepad**

Currently all gamepad events are merged into a single `gamepad.events` array that gets passed to disk pieces.

**Goal:** Provide separate event streams so pieces can easily differentiate between players:
- `gamepad.events[0]` - Player 1 (Gamepad index 0)
- `gamepad.events[1]` - Player 2 (Gamepad index 1)
- `gamepad.events[2]` - Player 3 (Gamepad index 2)
- `gamepad.events[3]` - Player 4 (Gamepad index 3)

**Alternative/Additional API:**
```javascript
// In disk piece act() function:
function act({ event: e, gamepads }) {
  // Check player 1 events
  if (gamepads[0]?.button(5)?.pushed) { /* ... */ }
  
  // Check player 2 events  
  if (gamepads[1]?.axis(0) > 0.5) { /* ... */ }
  
  // Or iterate all connected gamepads
  gamepads.forEach((gp, index) => {
    if (gp.button(0)?.pushed) {
      console.log(`Player ${index + 1} pressed A!`);
    }
  });
}
```

---

## Implementation Tasks

### Task 1: Update `lib/gamepad.mjs`
**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/gamepad.mjs`

**Changes:**
1. Keep single `events` array for backwards compatibility
2. Add new `eventsByGamepad` array structure:
   ```javascript
   export class Gamepad {
     events = [];           // All events (backwards compatible)
     eventsByGamepad = [];  // Events separated by gamepad index
     deviceData = [];
     
     // In the polling interval:
     // After pushing to this.events, also push to per-gamepad array:
     if (!this.eventsByGamepad[gi]) this.eventsByGamepad[gi] = [];
     this.eventsByGamepad[gi].push(event);
   }
   ```

3. Add method to clear per-gamepad events:
   ```javascript
   clearEvents() {
     this.events.length = 0;
     this.eventsByGamepad.forEach(arr => arr && (arr.length = 0));
   }
   ```

---

### Task 2: Update `bios.mjs` to expose per-gamepad streams
**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`

**Changes:**

#### A. Update the message sent to disk worker (around line 2785):
```javascript
// Current:
{
  keyboard: keyboard.events,
  gamepad: gamepad.events,
}

// Add:
{
  keyboard: keyboard.events,
  gamepad: gamepad.events,           // Keep for backwards compat
  gamepads: gamepad.eventsByGamepad, // NEW: Separate streams
}
```

#### B. Update event clearing (around line 2818):
```javascript
// Current:
gamepad.events.length = 0;

// Change to:
gamepad.clearEvents();
```

#### C. Also update other places where gamepad.events is cleared:
- Line 9849: Change to `gamepad.clearEvents();`

---

### Task 3: Update `disks/disk.mjs` to pass gamepads to pieces
**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/disk.mjs`

**Changes:**
1. Add `gamepads` to the API object passed to piece functions
2. Ensure it's available in `act()`, `sim()`, and other lifecycle functions
3. Create helper wrapper to make gamepad queries easier:

```javascript
// In disk.mjs, create a gamepad helper:
function createGamepadAPI(gamepadEvents) {
  return gamepadEvents.map((events, index) => ({
    index,
    events,
    
    // Helper to check if button was pushed
    button(buttonIndex) {
      const pushEvent = events.find(e => 
        e.button === buttonIndex && e.action === 'push'
      );
      const releaseEvent = events.find(e => 
        e.button === buttonIndex && e.action === 'release'
      );
      return {
        pushed: !!pushEvent,
        released: !!releaseEvent,
        event: pushEvent || releaseEvent
      };
    },
    
    // Helper to get axis value
    axis(axisIndex) {
      const axisEvent = events.findLast(e => e.axis === axisIndex);
      return axisEvent ? axisEvent.value : 0;
    },
    
    // Check if this gamepad is connected
    connected() {
      return events && events.length > 0;
    }
  }));
}

// Then pass to pieces:
api.gamepads = createGamepadAPI(message.content.event.gamepads);
```

---

### Task 4: Update existing `gamepad.mjs` piece to showcase multi-player
**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/gamepad.mjs`

**Changes:**
1. Display all 4 possible gamepads in a grid (2x2 or 1x4 layout)
2. Show which ones are connected vs disconnected
3. Color-code each player (P1: blue, P2: red, P3: green, P4: yellow)
4. Display real-time button/axis input for each controller separately
5. Show device ID/name for each connected gamepad

**Layout suggestion:**
```
┌─────────────┬─────────────┐
│   Player 1  │   Player 2  │
│   [Active]  │ [Inactive]  │
│  Xbox 360   │             │
│  Buttons... │             │
└─────────────┴─────────────┘
┌─────────────┬─────────────┐
│   Player 3  │   Player 4  │
│ [Inactive]  │ [Inactive]  │
└─────────────┴─────────────┘
```

---

### Task 5: Create example multi-player piece
**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/multi-pong.mjs` (new)

**Purpose:** Demonstrate the new multi-gamepad API

**Features:**
- 2-4 player pong/paddle game
- Each player controlled by their own gamepad
- Shows how to use `gamepads[0]`, `gamepads[1]`, etc.
- Gracefully handles players connecting/disconnecting mid-game

**Example code:**
```javascript
function act({ event: e, gamepads }) {
  // Player 1 (gamepad index 0)
  if (gamepads[0]?.axis(1) < -0.2) {
    player1.moveUp();
  }
  if (gamepads[0]?.axis(1) > 0.2) {
    player1.moveDown();
  }
  
  // Player 2 (gamepad index 1)  
  if (gamepads[1]?.axis(1) < -0.2) {
    player2.moveUp();
  }
  if (gamepads[1]?.axis(1) > 0.2) {
    player2.moveDown();
  }
  
  // Button to shoot/launch ball
  if (gamepads[0]?.button(0)?.pushed) {
    launchBall(1);
  }
  if (gamepads[1]?.button(0)?.pushed) {
    launchBall(2);
  }
}
```

---

## Testing Plan

### Test 1: Single Gamepad (Backwards Compatibility)
- Connect 1 Xbox controller
- Test existing pieces that use gamepad events
- Verify `event.gamepad === 0` works as before
- Verify `gamepad.events` still contains all events

### Test 2: Dual Gamepads
- Connect 2 controllers (Xbox + PS4/Switch Pro/etc)
- Open `gamepad` piece
- Verify both show as separate players
- Verify button presses don't interfere
- Verify each has correct device ID

### Test 3: Event Stream Separation
- With 2+ gamepads connected
- Press button on gamepad 0 - verify only `gamepads[0]` gets event
- Press button on gamepad 1 - verify only `gamepads[1]` gets event
- Move analog stick on gamepad 0 - verify axis events only in `gamepads[0]`

### Test 4: Connect/Disconnect
- Start with 1 gamepad
- Connect 2nd gamepad mid-session
- Verify it appears as Player 2
- Disconnect gamepad
- Verify graceful handling (no crashes)

### Test 5: Multi-Player Game
- Create/test `multi-pong` piece
- 2 players controlling separate paddles
- Verify no input crosstalk
- Verify button mappings work for different controller types

---

## API Design Decisions

### Option A: Array of Event Arrays (Simplest)
```javascript
function act({ gamepads }) {
  gamepads[0] // Array of events for player 1
  gamepads[1] // Array of events for player 2
  // Manual event parsing needed
}
```
**Pros:** Simple, direct access to raw events  
**Cons:** Requires manual event filtering/parsing

### Option B: Helper Object (Recommended)
```javascript
function act({ gamepads }) {
  gamepads[0].button(5).pushed  // Clean API
  gamepads[0].axis(1)           // Returns value
  gamepads[0].connected         // Boolean
  gamepads[0].id                // "Xbox 360 Controller"
}
```
**Pros:** Developer-friendly, self-documenting  
**Cons:** Slightly more code in disk.mjs

### Option C: Hybrid (Best of Both)
```javascript
function act({ gamepads }) {
  gamepads[0].events           // Raw events
  gamepads[0].button(5).pushed // Helper methods
  gamepads[0].axis(1)          // Helper methods
}
```
**Pros:** Flexibility + convenience  
**Cons:** None

**Recommendation:** Option C (Hybrid)

---

## Backwards Compatibility

### Existing Code Using `event.is("gamepad:0:...")`
✅ **Still works** - events still contain gamepad index

### Existing Code Iterating `gamepad.events`
✅ **Still works** - `gamepad.events` array unchanged

### Migration Path
Old code:
```javascript
function act({ event: e }) {
  if (e.is("gamepad:0:button:0:push")) {
    // Player 1 pressed A
  }
}
```

New code (optional upgrade):
```javascript
function act({ gamepads }) {
  if (gamepads[0]?.button(0).pushed) {
    // Player 1 pressed A
  }
}
```

---

## File Changes Summary

1. ✅ **lib/gamepad.mjs** - Add `eventsByGamepad` array + `clearEvents()`
2. ✅ **bios.mjs** - Pass `gamepads` array to disk worker, update clearing
3. ✅ **disks/disk.mjs** - Create gamepad helper API, pass to pieces
4. ✅ **disks/gamepad.mjs** - Update visualization for 4 players
5. ✅ **disks/multi-pong.mjs** - Create example multi-player piece (new file)
6. 📝 **Update docs** - Document new `gamepads` API in piece development guide

---

## Timeline Estimate

- **Task 1-2** (lib/gamepad.mjs + bios.mjs): 30 min
- **Task 3** (disk.mjs API): 45 min  
- **Task 4** (Update gamepad piece): 30 min
- **Task 5** (Multi-pong example): 60 min
- **Testing**: 30 min

**Total:** ~3 hours

---

## Future Enhancements

1. **Rumble/Haptic Support**
   - `gamepads[0].vibrate(duration, intensity)`
   - Uses Gamepad API's vibrationActuator

2. **Input Recording/Replay**
   - Record gamepad inputs for replays/demos
   - Useful for fighting games, speedruns

3. **Button Remapping UI**
   - Let users customize controls
   - Store preferences in localStorage

4. **Gamepad Profiles**
   - Pre-configured mappings for popular controllers
   - Handle Xbox vs PlayStation vs Switch differences

5. **Dead Zone Configuration**
   - Per-gamepad dead zone settings
   - Useful for worn/drifting analog sticks

---

## Questions to Resolve

1. Should `gamepads` be available in `sim()` or just `act()`?
   - **Recommendation:** Both, for flexibility

2. Should we expose raw `gamepad.deviceData` for advanced queries?
   - **Recommendation:** Yes, as `gamepads[i].raw` for power users

3. How to handle more than 4 gamepads?
   - **Recommendation:** Support up to 8 (theoretical browser limit)

4. Should button helpers use button numbers or semantic names?
   - **Recommendation:** Both - `button(0)` and `button('a')` via mappings

---

## Implementation Priority

### Phase 1 (MVP - This PR)
- ✅ Task 1: Update lib/gamepad.mjs
- ✅ Task 2: Update bios.mjs  
- ✅ Task 3: Update disk.mjs API

### Phase 2 (Polish)
- ✅ Task 4: Update gamepad.mjs piece
- ✅ Task 5: Create multi-pong example

### Phase 3 (Future)
- Add rumble support
- Add button remapping
- Add gamepad profiles

---

## Success Criteria

1. ✅ Can connect 4 controllers simultaneously
2. ✅ Each controller's input is separate/isolated
3. ✅ `gamepads[i].button(n).pushed` returns correct state
4. ✅ `gamepads[i].axis(n)` returns correct value (-1 to 1)
5. ✅ Backwards compatible - existing code still works
6. ✅ `gamepad.mjs` piece shows all 4 players
7. ✅ Multi-player example piece works smoothly
8. ✅ No performance degradation with 4 active controllers

---

## Notes

- Xbox Series X controller currently connected (based on user context)
- Existing gamepad.mjs piece already has good visualization foundation
- lib/gamepad.mjs is well-structured and ready for this enhancement
- Most work is in disk.mjs to create developer-friendly API
- This will enable local multiplayer pieces (pong, racing, fighting games, etc.)
