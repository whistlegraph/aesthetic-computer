# Artery TUI Redesign Plan

## Current Implementation

### Library/Approach
**No external TUI library** - Pure Node.js with raw ANSI escape codes:
- `readline` for keyboard input
- Manual ANSI codes for colors, cursor, screen buffer
- Custom frame buffering and rendering loop
- ~3200 lines of hand-rolled TUI code

### Current Features
| Feature | Description |
|---------|-------------|
| **Header** | Animated "AESTHETIC COMPUTER" baby blocks, status indicators (AC Open/Closed, L●/P● servers, CDP status), platform info (container→host) |
| **Menu System** | DOS-style navigation with keyboard shortcuts, highlighted selection |
| **Log Viewer** | Scrollable log panel with color-coded entries, timestamps |
| **Multi-view** | Menu view, Log view, Watch mode |
| **Theme System** | Dynamic colors based on connection state (blue=connected, red=disconnected) |
| **Responsive** | Compact mode for < 80 cols |
| **Blood Animation** | Pulsing color wave effect on title |
| **Server Monitoring** | Polls local/production server status |
| **CDP Integration** | Browser automation tunnel status |
| **Tab Management** | Emacs tab spawning for tests, logs |

### Pain Points
- **3200+ LOC** of manual rendering code
- Emoji width calculations are fragile
- Padding/alignment bugs are frequent
- No component abstraction
- Hard to add new views
- Animation logic scattered throughout
- Testing is difficult

---

## Alternative CLI Libraries

### 🏆 Top Recommendations

#### 1. **Ink** (React for CLIs)
```bash
npm install ink react
```
- **Approach**: React components render to terminal
- **Pros**: Familiar React patterns, Flexbox layout, great DX
- **Cons**: React dependency, slightly heavier
- **Vibe**: Modern, component-based, declarative
- **Used by**: Gatsby, Yarn, Prisma, Terraform

```jsx
import {render, Box, Text} from 'ink';

const App = () => (
  <Box flexDirection="column" borderStyle="round" borderColor="cyan">
    <Text color="green" bold>● AC Open</Text>
    <Text dimColor>L● P●</Text>
  </Box>
);

render(<App />);
```

#### 2. **Blessed / Blessed-contrib**
```bash
npm install blessed blessed-contrib
```
- **Approach**: ncurses-like widget system
- **Pros**: Rich widgets (gauges, charts, logs), battle-tested
- **Cons**: Older API, less maintained, callback-heavy
- **Vibe**: Classic TUI, dashboard-focused

#### 3. **Terminal-kit**
```bash
npm install terminal-kit
```
- **Approach**: Full terminal control library
- **Pros**: Comprehensive, menus/forms/tables built-in, animations
- **Cons**: Large API surface, docs can be sparse
- **Vibe**: Swiss army knife

#### 4. **Neo-blessed** (Blessed fork)
```bash
npm install neo-blessed
```
- **Approach**: Maintained fork of blessed
- **Pros**: Bug fixes, TypeScript support
- **Cons**: Still callback-based

#### 5. **Clack** ✨
```bash
npm install @clack/prompts
```
- **Approach**: Beautiful prompts and spinners
- **Pros**: Gorgeous defaults, simple API, very hip
- **Cons**: More for prompts than full TUIs
- **Vibe**: Modern, minimal, elegant

#### 6. **Pastel** (Ink-based framework)
```bash
npm install pastel
```
- **Approach**: Full CLI framework built on Ink
- **Pros**: Routing, commands, Ink rendering
- **Cons**: Framework overhead

#### 7. **Tui-rs style in JS** (Low-level)
- **Approach**: Port Rust's tui-rs patterns
- **Pros**: Maximum control, efficient rendering
- **Cons**: More work, less ecosystem

---

## Redesign Proposal

### Goals
1. **Economical** - Minimize vertical space usage
2. **Elegant** - Clean, modern aesthetic
3. **Responsive** - Graceful degradation to tiny terminals
4. **Maintainable** - Component-based, testable
5. **Fast** - Efficient rendering, no flicker

### Proposed Layout: "Compact Remote"

```
╭─────────────────────────────────────────────────────╮
│ ▣ AESTHETIC COMPUTER              ● L● P● CDP●  │
├─────────────────────────────────────────────────────┤
│ [R]un  [T]est  [L]ogs  [W]atch  [B]uild  [Q]uit │
├─────────────────────────────────────────────────────┤
│ 12:34:56 Server started on :8888                   │
│ 12:34:57 WebSocket connected                       │
│ 12:34:58 Piece loaded: prompt                      │
│ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ │
╰─────────────────────────────────────────────────────╯
```

### Key Changes

#### 1. **Single-line Header**
- Title + status on ONE line
- No ASCII art in normal mode (optional "fancy" mode)

#### 2. **Horizontal Menu Bar**
- Hotkeys inline: `[R]un [T]est [L]ogs`
- No vertical menu list

#### 3. **Maximized Log Area**
- Logs get all remaining space
- Smart truncation with `...` for long lines

#### 4. **Minimal Chrome**
- Thin box-drawing borders (─│╭╮╰╯)
- No double-line DOS borders
- Subtle colors, not bold backgrounds

#### 5. **Status Pills**
```
● Online  ◐ Starting  ○ Offline
```

### Micro Mode (< 60 cols)
```
╭──────────────────────────────╮
│ AC ●L●P● R T L W Q │
├──────────────────────────────┤
│ Server :8888                 │
│ WS connected                 │
╰──────────────────────────────╯
```

---

## Implementation Options

### Option A: Ink Rewrite
**Effort**: High (full rewrite)
**Benefit**: Modern, maintainable, community support

```jsx
// Pseudocode structure
<App>
  <Header status={status} />
  <MenuBar items={menuItems} onSelect={handleSelect} />
  <LogPanel logs={logs} />
  <StatusBar message={statusMessage} />
</App>
```

### Option B: Refactor Current
**Effort**: Medium
**Benefit**: Keep existing logic, just clean up rendering

- Extract `Component` base class
- Create `Header`, `MenuBar`, `LogPanel` components
- Centralize padding/width calculations
- Add layout manager

### Option C: Terminal-kit Migration
**Effort**: Medium-High
**Benefit**: Rich widgets, less custom code

### Option D: Hybrid (Clack + Custom)
**Effort**: Low-Medium
**Benefit**: Use Clack for prompts, keep custom dashboard

---

## Recommended Path

### Phase 1: Quick Wins (Current Codebase)
- [ ] Extract `renderBoxLine` helper (done!)
- [ ] Create component classes
- [ ] Simplify header to single line option
- [ ] Remove complex emoji handling where possible

### Phase 2: Evaluate Ink
- [ ] Prototype header/menu in Ink
- [ ] Benchmark rendering performance
- [ ] Decide: full migration or hybrid

### Phase 3: Full Redesign (if Ink works)
- [ ] Port all views to Ink components
- [ ] Add Flexbox layouts
- [ ] Implement responsive breakpoints
- [ ] Add animation via useEffect

---

## References

- [Ink](https://github.com/vadimdemedes/ink) - React for CLIs
- [Blessed](https://github.com/chjj/blessed) - ncurses for Node
- [Terminal-kit](https://github.com/cronvel/terminal-kit) - Full terminal lib
- [Clack](https://github.com/natemoo-re/clack) - Beautiful prompts
- [Charm's libraries](https://charm.sh) - Go TUI libs (inspiration)
- [tui-rs](https://github.com/fdehau/tui-rs) - Rust TUI (design patterns)

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2024-12-10 | Document current state | Needed baseline for redesign |
| | | |
