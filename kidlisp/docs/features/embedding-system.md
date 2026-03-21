# KidLisp Embed Feature

## Overview

The new `embed` command and `$`-prefixed function syntax allows you to import and run cached KidLisp snippets in their own isolated painting buffers, then paste the results onto your main canvas.

## Usage Examples

### Basic Embedding

```lisp
; If you have saved code with cache code "abc123XY":

; Method 1: Direct function call (auto-pastes at 0,0)
($abc123XY)

; Method 2: Direct function call with custom buffer size
($abc123XY 128 128)

; Method 3: Explicit embed syntax
(embed $abc123XY)
(embed $abc123XY 64 64)

; Method 4: Embed and paste at specific coordinates
(paste ($abc123XY 32 32) 100 100)
```

### Advanced Usage

```lisp
; Create a composition using multiple cached snippets
(wipe "black")

; Load a background pattern in a large buffer
(paste ($background 256 256) 0 0)

; Add smaller decorative elements
(paste ($stars 64 64) 50 50)
(paste ($moon 32 32) 200 20)

; Overlay text or UI elements  
(paste ($ui 128 32) 64 200)
```

### Buffer Dimensions

- Default: 256x256 pixels
- Custom: `($code width height)` 
- Position args ignored: `($code x y width height)` (x,y ignored for API consistency)

## How It Works

1. **Fetch**: Loads cached KidLisp source code from the network using the cache ID
2. **Isolate**: Creates a new painting buffer with specified dimensions  
3. **Execute**: Runs the cached code in its own KidLisp context within the buffer
4. **Return**: Returns the painted buffer as a painting object
5. **Paste**: Auto-pastes at (0,0) for standalone calls, or can be manually positioned

## Technical Details

- Each embedded snippet runs in its own isolated KidLisp environment
- The painting buffer has its own coordinate system starting at (0,0)
- Network requests are cached to avoid repeated fetching
- Supports all KidLisp graphics commands: `ink`, `box`, `line`, `circle`, etc.
- Error handling: Failed embeds show red error buffer

## Benefits

- **Modularity**: Break complex art into reusable components
- **Collaboration**: Share and remix cached code snippets
- **Performance**: Render complex patterns in appropriately-sized buffers
- **Composition**: Layer multiple elements at different scales and positions

## Error Handling

If a cached code fails to load or execute:
- A red semi-transparent buffer is returned to indicate the error
- Error details are logged to the console
- The main program continues to execute normally
