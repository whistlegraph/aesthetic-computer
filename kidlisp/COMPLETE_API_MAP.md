# 🎯 KidLisp Complete API Reference Map

Generated from `kidlisp.mjs` on 2025-09-06

## 📖 Core System Functions

### **Language & Control Flow**
| Function | Usage | Description |
|----------|-------|-------------|
| `def` | `(def name value)` | Define variables |
| `later` | `(later name params body)` | Define functions |
| `if` | `(if condition then else)` | Conditional execution |
| `once` | `(once expr)` | Execute only once per session |
| `not` | `(not expr)` | Logical negation |
| `now` | `(now)` | Execute immediately |
| `die` | `(die)` | Stop execution |

### **Math & Numbers**
| Function | Usage | Description |
|----------|-------|-------------|
| `+` | `(+ a b c...)` | Addition |
| `-` | `(- a b)` | Subtraction |
| `*` | `(* a b c...)` | Multiplication |
| `/` | `(/ a b)` | Division |
| `%` | `(% a b)` | Modulo |
| `sin` | `(sin x)` | Sine function |
| `cos` | `(cos x)` | Cosine function |
| `max` | `(max a b c...)` | Maximum value |
| `min` | `(min a b c...)` | Minimum value |
| `mod` | `(mod a b)` | Modulo operation |
| `mul` | `(mul a b c...)` | Multiplication (alias) |
| `random` | `(random max)` | Random number 0 to max |
| `range` | `(range start end)` | Create number range |
| `wiggle` | `(wiggle amount)` | Random ±amount/2 |

## 🎨 Graphics & Drawing

### **Screen Management**
| Function | Usage | Description |
|----------|-------|-------------|
| `resolution` | `(resolution w h)` | Set canvas resolution |
| `wipe` | `(wipe color)` | Clear screen with color |
| `coat` | `(coat color alpha)` | Semi-transparent overlay |
| `mask` | `(mask x y w h)` | Set drawing mask region |
| `unmask` | `(unmask)` | Remove drawing mask |

### **Drawing Primitives**
| Function | Usage | Description |
|----------|-------|-------------|
| `ink` | `(ink color)` | Set drawing color |
| `line` | `(line x1 y1 x2 y2)` | Draw line |
| `lines` | `(lines points...)` | Draw connected lines |
| `box` | `(box x y w h)` | Draw filled rectangle |
| `circle` | `(circle x y radius)` | Draw filled circle |
| `tri` | `(tri x1 y1 x2 y2 x3 y3)` | Draw triangle |
| `plot` | `(plot x y)` | Draw single pixel |
| `flood` | `(flood x y color)` | Flood fill area |
| `shape` | `(shape points filled)` | Draw custom polygon |

### **Color & Effects**
| Function | Usage | Description |
|----------|-------|-------------|
| `fade` | `(fade "red-blue" direction)` | Color gradients |
| `rainbow` | `(rainbow)` | Cycling rainbow colors |
| `zebra` | `(zebra)` | Black/white alternating |
| `backdrop` | `(backdrop color)` | Set background color |

## 🖼️ Images & Media

### **Image Functions**
| Function | Usage | Description |
|----------|-------|-------------|
| `paste` | `(paste url x y scale)` | Paste image at position |
| `stamp` | `(stamp url x y)` | Paste image centered |
| `painting` | `(painting x y)` | Paste current user's painting |
| `steal` | `(steal)` | Copy current buffer |
| `putback` | `(putback)` | Restore copied buffer |

### **Text Rendering**
| Function | Usage | Description |
|----------|-------|-------------|
| `write` | `(write text x y bg size)` | Draw text |
| `len` | `(len text)` | Get text length |

## 🔄 Transformations & Effects

### **Pixel Transformations**
| Function | Usage | Description |
|----------|-------|-------------|
| `scroll` | `(scroll dx dy)` | Scroll canvas pixels | [📖](docs/functions/scroll.md) |
| `zoom` | `(zoom factor centerX centerY)` | Zoom in/out from center | [📖](docs/functions/zoom.md) |
| `suck` | `(suck strength centerX centerY)` | Radial displacement (vortex) | [📖](docs/functions/suck.md) |
| `spin` | `(spin angle)` | Rotate canvas | [📖](docs/functions/spin.md) |
| `resetSpin` | `(resetSpin)` | Reset rotation |
| `smoothspin` | `(smoothspin angle)` | Smooth rotation |
| `sort` | `(sort)` | Sort pixels by brightness |
| `blur` | `(blur amount)` | Blur entire canvas |
| `contrast` | `(contrast amount)` | Adjust contrast |
| `pan` | `(pan dx dy)` | Pan camera view |
| `unpan` | `(unpan)` | Reset camera position |

## 🎵 Audio & Sound

### **Audio Input**
| Function | Usage | Description |
|----------|-------|-------------|
| `mic` | `(mic)` | Access microphone |
| `amplitude` | `(amplitude)` | Get audio amplitude |
| `speaker` | `(speaker)` | Audio output control |

### **Music Generation**
| Function | Usage | Description |
|----------|-------|-------------|
| `melody` | `(melody notes)` | Play musical sequence |
| `overtone` | `(overtone freq)` | Generate harmonic tones |
| `noise` | `(noise)` | Generate noise |

## 🎮 3D Graphics

### **3D Objects**
| Function | Usage | Description |
|----------|-------|-------------|
| `cube` | `(cube id)` | Create/reference cube |
| `quad` | `(quad)` | Create quad primitive |
| `form` | `(form objects...)` | Render 3D forms |
| `trans` | `(trans form transformations...)` | Transform 3D objects |

### **3D Transformations**
| Function | Usage | Description |
|----------|-------|-------------|
| `cubespin` | `(cubespin x y z)` | Animate cube rotation |
| `cubepos` | `(cubepos x y z)` | Set cube position |
| `cubescale` | `(cubescale factor)` | Scale cube |
| `cuberot` | `(cuberot x y z)` | Set cube rotation |

### **Camera Control**
| Function | Usage | Description |
|----------|-------|-------------|
| `camrot` | `(camrot x y z)` | Set camera rotation |
| `camrotx` | `(camrotx angle)` | Rotate camera X |
| `camroty` | `(camroty angle)` | Rotate camera Y |
| `camrotz` | `(camrotz angle)` | Rotate camera Z |
| `camspin` | `(camspin x y z)` | Animate camera rotation |
| `camspinx` | `(camspinx speed)` | Animate camera X rotation |
| `camspiny` | `(camspiny speed)` | Animate camera Y rotation |
| `camspinz` | `(camspinz speed)` | Animate camera Z rotation |

## 📊 Data & Values

### **System Properties**
| Function | Usage | Description |
|----------|-------|-------------|
| `width` / `w` | `(width)` | Canvas width |
| `height` / `h` | `(height)` | Canvas height |
| `frame` / `f` | `(frame)` | Current frame number |
| `clock` | `(clock)` | UTC timestamp |
| `fps` | `(fps rate)` | Set/get frame rate |

### **Data Manipulation**
| Function | Usage | Description |
|----------|-------|-------------|
| `repeat` / `rep` | `(repeat count expr)` | Repeat expression |
| `choose` | `(choose options...)` | Random selection |
| `source` | `(source code)` | Access AST source |
| `cache` | `(cache id)` | Store/retrieve data |

## 🔧 Utility Functions

### **Control & Logic**
| Function | Usage | Description |
|----------|-------|-------------|
| `tap` | `(tap)` | Handle touch/click |
| `draw` | `(draw)` | Force redraw |
| `hop` | `(hop url)` | Navigate to URL |
| `delay` | `(delay time expr)` | Delayed execution |
| `debug` | `(debug value)` | Debug logging |
| `log` | `(log message)` | Console logging |
| `label` | `(label text color offset)` | HUD label |

### **Boolean Values**
| Function | Usage | Description |
|----------|-------|-------------|
| `yes` | `(yes)` | Boolean true |
| `no` | `(no)` | Boolean false |

### **Advanced Features**
| Function | Usage | Description |
|----------|-------|-------------|
| `embed` | `(embed code args...)` | Execute embedded code |
| `bake` | `(bake)` | Finalize current frame |
| `jump` | `(jump piece)` | Navigate to piece |

## 🎨 Color Shortcuts

All CSS color names are available as functions:
- `red`, `blue`, `green`, `yellow`, `orange`, `purple`, `magenta`
- `cyan`, `teal`, `lime`, `gray`, `grey`, `white`, `black`
- Plus many more CSS colors

## 🌪️ Transformation Functions

KidLisp provides 11 transformation functions for pixel manipulation, each with equal importance:

```lisp
(scroll 5 0)            ; Translation with wrapping
(zoom 1.1)              ; Scale from center point
(spin 1)                ; Rotation around center
(suck 0.5)              ; Radial displacement
(blur 2)                ; Gaussian blur effect
```

All transformation functions follow similar patterns:
- **Accumulation systems** for continuous evolution
- **Deferred execution** compatible with embedded layers
- **Performance optimization** for real-time use
- **Composability** for layered effects

---

*This API map represents the complete KidLisp function set as of September 2025. Functions are dynamically resolved from global environment, API context, and user definitions.*
