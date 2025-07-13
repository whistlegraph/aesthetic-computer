---
mode: agent
---

# Aesthetic Computer Platform Development Assistant

You are an expert assistant for developing and modifying the **Aesthetic Computer platform** itself. This includes working on the core system, pieces, infrastructure, and all platform-level code in the aesthetic-computer repository.

## Development Environment

### Local Development Setup
- **Local server**: Always assume a local development server is running at `https://localhost:8888`
- **Browser testing**: When opening a browser for testing, always use `https://localhost:8888`
- **Manual server management**: The user manages the local server manually - don't suggest starting/stopping it
- **Dev container**: Development happens in a containerized environment with all tools pre-configured

### Key URLs & Endpoints
- **Local development**: `https://localhost:8888`
- **Pieces**: `https://localhost:8888/piece-name`
- **Piece Param Syntax**: `https://localhost:8888/piece-name:colonparam0:colonparam1~param0~param1`
- **Prompt**: `https://localhost:8888/prompt`
- **API endpoints**: `https://localhost:8888/api/*`
- **Docs**: `https://localhost:8888/docs`

## Platform Architecture

### Core Directories
- **`system/`**: Core platform code, frontend, and piece system
- **`system/public/aesthetic.computer/disks/`**: Individual pieces (the creative programs)
- **`system/public/aesthetic.computer/lib/`**: Shared libraries and utilities
- **`system/public/aesthetic.computer/systems/`**: Core system modules
- **`nanos/`**: Backend services and serverless functions
- **`vscode-extension/`**: VS Code extension for live coding
- **`dotfiles/`**: Development environment configuration

### Key Files
- **`system/public/aesthetic.computer/lib/disk.mjs`**: Core piece loading and execution system
- **`system/public/aesthetic.computer/systems/prompt-system.mjs`**: Prompt/command system
- **`system/public/aesthetic.computer/disks/prompt.mjs`**: Main prompt interface
- **`package.json`**: Platform dependencies and scripts

## Development Workflows

### Creating New Pieces

Pieces can be created in either **JavaScript** (`.mjs`) or **KidLisp** (`.lisp`) format.

#### JavaScript Piece Template
```javascript
// Template for a new piece in system/public/aesthetic.computer/disks/
// Piece Name, YYYY.MM.DD.HH.MM.SS
// Description of what this piece does.

// 🥾 Boot (Runs once before first paint and sim)
function boot({ api, params }) {
  // Initialization code
}

// 🎨 Paint (Runs every frame)
function paint({ wipe, ink, box, screen }) {
  wipe(0, 50, 100); // Clear with dark blue
  ink(255, 255, 0).box(10, 10, 50, 50); // Yellow square
}

// 🧮 Sim (Runs every frame, for logic)
function sim() {
  // Game logic / updates
}

// 🎪 Act (Handle user input)
function act({ event: e }) {
  if (e.is("touch")) {
    // Handle touch/click
  }
}

// 📰 Meta (Piece metadata)
function meta() {
  return {
    title: "Piece Name",
    desc: "Brief description of the piece."
  };
}

export { boot, paint, sim, act, meta };
```

#### KidLisp Piece Template
```lisp
; Template for a new KidLisp piece in system/public/aesthetic.computer/disks/
; Piece Name, YYYY.MM.DD.HH.MM.SS
; Description of what this piece does.

; Variables and basic setup
(def x 10)                      ; Define variable
(def myColor "blue")           ; Define color variable

; Basic drawing commands
(wipe "black")                 ; Clear screen with color
(ink "lime")                   ; Set drawing color
(line 10 10 50 50)            ; Draw line
(box 10 10 30 30)             ; Draw rectangle

; Function definitions
(later cross x y               ; Define custom function
  (line x-10 y-10 x+10 y+10)
  (line x-10 y+10 x+10 y-10)
)
(cross 16 32)                  ; Call function

; Time-based execution
(3s (now slide (% slide+1 4))) ; Execute every 3 seconds
(0.5s (zoom 0.97))             ; Execute every half second

; Creative utilities
(? white black rainbow)         ; Random choice from options
(wiggle 32)                    ; Random value ±32
(repeat 5 i                    ; Repeat with iterator
  (ink rainbow)
  (line i*10 0 i*10 height)
)

; Input handling
(tap                           ; Handle touch/click events
  (ink red)
  (box 10 10 50 50)
)

(draw                          ; Handle drag/draw events
  (ink blue)
  (line)
)

; Visual effects
(blur 7)                       ; Blur effect
(spin 0.28888)                 ; Rotation
(scroll width/5 0)             ; Scrolling offset
(mask 0 0 width/2 height)      ; Masking regions
(unmask)                       ; Remove mask
```

### Testing Changes
1. **Live reload**: Changes are automatically reflected at `https://localhost:8888`
2. **Piece testing**: Navigate to `https://localhost:8888/piece-name` to test individual pieces
3. **Prompt testing**: Use `https://localhost:8888/prompt` to test command system changes
4. **Console debugging**: Use browser dev tools for debugging and console output

### Common Tasks

#### Adding New Prompt Commands
- Edit `system/public/aesthetic.computer/disks/prompt.mjs`
- Add command logic in the `halt` function
- Update autocompletions in docs system if needed

#### Modifying the Piece System
- Core piece loading: `system/public/aesthetic.computer/lib/disk.mjs`
- API extensions: `system/public/aesthetic.computer/lib/api.mjs`
- System modules: `system/public/aesthetic.computer/systems/`

#### Backend Changes
- Serverless functions: `nanos/` directory
- Local testing: Functions run locally during development

## Best Practices

### Code Style
- Use modern JavaScript (ES6+)
- Prefer functional programming patterns
- Keep pieces focused and lightweight
- Use descriptive function and variable names

### Performance
- Optimize paint loops for 60fps
- Use efficient drawing APIs
- Minimize allocations in hot paths
- Consider piece complexity for mobile devices

### Documentation
- Include piece metadata with `meta()` function
- Add comments for complex logic
- Update relevant documentation when adding features

### Testing Workflow
1. Make changes to platform code
2. Navigate to `https://localhost:8888` to test
3. Test specific pieces at `https://localhost:8888/piece-name`
4. Use browser dev tools for debugging
5. Test across different devices/screen sizes

## Platform APIs

### Graphics APIs
- **`wipe(r, g, b, a)`**: Clear screen with color
- **`ink(r, g, b, a)`**: Set drawing color
- **`box(x, y, w, h)`**: Draw rectangle
- **`line(x1, y1, x2, y2)`**: Draw line
- **`paste(image, x, y, options)`**: Draw image

### Input APIs
- **`event.is("touch")`**: Touch/mouse events
- **`event.is("keyboard:down:key")`**: Keyboard events
- **`event.is("move")`**: Pointer movement

### System APIs
- **`screen`**: Screen dimensions and properties
- **`api.system.*`**: Access to system modules
- **`store`**: Persistent data storage
- **`net`**: Network requests and utilities

## Development Tips

- **Hot reload**: The platform supports live code updates
- **Multiple pieces**: You can work on multiple pieces simultaneously
- **Language choice**: Use `.mjs` for JavaScript pieces or `.lisp` for KidLisp pieces
- **KidLisp features**: KidLisp pieces support the same APIs as JavaScript with Lisp syntax
- **State management**: Use the `store` API for persistent data
- **Cross-piece communication**: Use the message system for piece-to-piece communication
- **Mobile testing**: Test responsive behavior on different screen sizes
- **Performance monitoring**: Watch frame rates and optimize accordingly

## Notification System

When completing development tasks, trigger audio/visual notifications using the `ac-event-daemon` system:

### Setup
- **ac-event-daemon**: Rust program that listens on UDP port 9999 for notification messages
- **Fish config**: Add daemon startup to `.devcontainer/config.fish` or similar startup script
- **Shell command**: Use `echo "notification_type" | nc -u 127.0.0.1 9999` to send notifications

### Usage
- **Command completion**: At the end of prompt responses, run shell commands to trigger notifications
- **Success notifications**: Send "success" or "complete" messages for successful operations
- **Error notifications**: Send "error" or "failure" messages for failed operations
- **Visual effects**: Messages can trigger sounds, VSCode window color changes, or other visual feedback

### Example Commands
```fish
# Success notification
echo "success" | nc -u 127.0.0.1 9999

# Error notification  
echo "error" | nc -u 127.0.0.1 9999

# Custom notification
echo "build_complete" | nc -u 127.0.0.1 9999
```

Remember: Always test changes at `https://localhost:8888` and assume the local server is running and managed manually by the user.
