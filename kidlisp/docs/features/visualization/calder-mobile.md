# Calder Mobile: A Dynamic S-Expression Visualizer

A visualization system inspired by Alexander Calder's kinetic sculptures—balanced hierarchical structures that move, respond to forces, and reveal relationships through physical metaphor.

## The Vision

```
                    ┌─────┐
                    │defun│
                    └──┬──┘
           ┌──────────┴──────────┐
           │                     │
        ┌──┴──┐               ┌──┴──┐
        │name │               │body │
        └─────┘               └──┬──┘
                          ┌─────┴─────┐
                          │           │
                       ┌──┴──┐     ┌──┴──┐
                       │ if  │     │args │
                       └──┬──┘     └─────┘
                    ┌────┴────┐
                    │         │
                 ┌──┴──┐   ┌──┴──┐
                 │cond │   │then │
                 └─────┘   └─────┘
```

But instead of static boxes—**hanging, swaying, balanced elements** that:
- Float in space
- Respond to mouse/touch like wind
- Settle into equilibrium
- Show balance/imbalance of the code structure

---

## Core Concepts

### 1. Balance as Meaning

In a Calder mobile, heavy elements hang closer to the pivot; light elements extend further out. For code:

| Code Property | Physical Property |
|---------------|-------------------|
| Expression depth | Distance from center |
| Expression complexity | Element weight/size |
| Evaluation frequency | Color intensity |
| Data flow | Wire thickness |
| Errors | Tilted/unbalanced branches |

### 2. Hierarchy as Suspension

```
S-expression:  (+ (* 2 3) (- 10 4))

Mobile:
                    [+]
                   /   \
                  /     \
               [*]       [-]
              / | \     / | \
             2  ·  3  10  ·  4
```

- **Parent** hangs above children
- **Children** hang from a shared bar (like Calder's horizontal elements)
- **Atoms** are the "leaves" at the bottom—small shapes that dangle

### 3. Motion as Execution

When code runs:
- **Evaluated expressions** swing/pulse
- **Data flow** creates ripples up the tree
- **Results** bubble up as color/glow changes
- **Errors** cause violent swinging/tilting

---

## Visual Elements

### Shapes by Type

| Type | Shape | Why |
|------|-------|-----|
| **Function call** | Circle/disc | Primary, balanced |
| **Special form** | Triangle | Distinctive, structural |
| **Number** | Small circle | Atomic, simple |
| **String** | Rectangle | Has "length" |
| **Symbol** | Diamond | Variable, precious |
| **List literal** | Bar with hooks | Container |
| **Lambda** | Spiral | Self-referential |

### Wires

- **Thin wire**: Normal parent-child
- **Thick wire**: Heavy data flow
- **Colored wire**: Type annotation
- **Pulsing wire**: Currently evaluating

### Colors (Calder-inspired palette)

```
Primary elements:  ● Red    ● Blue   ● Yellow  (bold, Calder's favorites)
Secondary:         ● Black  ○ White  
Accents:           ● Orange for errors
                   ● Green for successful evaluation
```

---

## Physics Simulation

### Forces

1. **Gravity** - Pulls everything down
2. **Wire tension** - Keeps structure connected
3. **Air resistance** - Dampens motion
4. **Wind** - Mouse/touch interaction
5. **Balance constraint** - Bars try to stay level

### The Balance Algorithm

Each horizontal bar maintains balance:

```javascript
// Torque must equal zero for balance
// τ = r × F (distance × force)

leftTorque = leftChild.weight * leftChild.distance
rightTorque = rightChild.weight * rightChild.distance

// Bar tilts until torques equalize
// Heavier children move closer to pivot
```

This means **complex subtrees pull their side down** unless balanced by equal complexity on the other side. Visually reveals asymmetry in code structure!

---

## Interaction

### Mouse/Touch as Wind

```javascript
// User movement creates "wind" force
windForce = {
  x: (mouseX - prevMouseX) * windStrength,
  y: (mouseY - prevMouseY) * windStrength
};

// Apply to nearby elements with falloff
elements.forEach(el => {
  const dist = distance(mouse, el);
  const force = windForce * (1 / (1 + dist * 0.01));
  el.applyForce(force);
});
```

### Click to Focus

- Click an element to "grab" it
- Drag to see how the mobile responds
- Release to let it settle
- Double-click to zoom/isolate that subtree

### Hover for Info

- Hovering shows:
  - Expression source code
  - Type information
  - Last evaluated value
  - Execution count

---

## Implementation Sketch

### Data Structure

```javascript
class MobileNode {
  constructor(expr, parent = null) {
    this.expr = expr;           // The S-expression
    this.parent = parent;
    this.children = [];
    
    // Physics
    this.position = { x: 0, y: 0 };
    this.velocity = { x: 0, y: 0 };
    this.weight = this.calculateWeight();
    
    // Visual
    this.shape = this.shapeForType();
    this.color = this.colorForType();
    this.size = Math.log(this.weight + 1) * 10;
  }
  
  calculateWeight() {
    if (this.children.length === 0) return 1;
    return 1 + this.children.reduce((sum, c) => sum + c.weight, 0);
  }
}

class MobileBar {
  constructor(parent, children) {
    this.parent = parent;
    this.children = children;
    this.angle = 0;  // Current tilt
    this.pivot = { x: 0, y: 0 };
  }
  
  calculateBalance() {
    // Find pivot point where torques balance
    let totalWeight = this.children.reduce((s, c) => s + c.weight, 0);
    let weightedPos = this.children.reduce((s, c) => s + c.weight * c.localX, 0);
    return weightedPos / totalWeight;
  }
}
```

### Render Loop

```javascript
function animate() {
  // Apply forces
  applyGravity();
  applyWindFromMouse();
  applyWireTension();
  applyBalanceConstraints();
  
  // Update physics
  nodes.forEach(node => {
    node.velocity.x += node.force.x / node.weight;
    node.velocity.y += node.force.y / node.weight;
    node.velocity.x *= damping;
    node.velocity.y *= damping;
    node.position.x += node.velocity.x;
    node.position.y += node.velocity.y;
  });
  
  // Draw
  ctx.clearRect(0, 0, width, height);
  drawWires();
  drawBars();
  drawNodes();
  
  requestAnimationFrame(animate);
}
```

---

## KidLisp Integration

### Building the Mobile from Code

```javascript
function buildMobile(sexpr) {
  if (isAtom(sexpr)) {
    return new MobileNode(sexpr);
  }
  
  const node = new MobileNode(sexpr);
  const children = sexpr.slice(1).map(child => buildMobile(child));
  
  // Create bar for children
  if (children.length > 0) {
    node.bar = new MobileBar(node, children);
    node.children = children;
    children.forEach(c => c.parent = node);
  }
  
  return node;
}

// Parse KidLisp, build mobile
const ast = parse("(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))");
const mobile = buildMobile(ast);
```

### Live Execution Visualization

```javascript
// Hook into KidLisp evaluator
function onEvaluate(expr, result) {
  const node = findNodeForExpr(expr);
  if (node) {
    // Visual feedback
    node.pulse();
    node.showResult(result);
    
    // Propagate "energy" up the tree
    let current = node.parent;
    while (current) {
      current.ripple();
      current = current.parent;
    }
  }
}
```

---

## Aesthetic Variations

### 1. Classic Calder
- Primary colors (red, blue, yellow, black)
- Thin black wires
- Simple geometric shapes
- High contrast

### 2. Organic
- Earth tones
- Flowing, curved wires
- Leaf/cell shapes
- Gentle motion

### 3. Neon
- Glowing colors on dark background
- Light trails on motion
- Pulsing connections
- Cyberpunk aesthetic

### 4. Paper
- Cream/sepia tones
- Rough textures
- Hand-drawn wire style
- Soft shadows

---

## Future Ideas

### 3D Mobile
- Full 3D rotation and depth
- VR/AR viewing
- Walk around your code

### Sound
- Each element has a tone
- Collisions create sounds
- Evaluation creates melody
- Wind creates ambient texture

### Collaboration
- Multiple people can blow wind
- See others' cursor as wind source
- Shared viewing of same codebase

### Time Travel
- Scrub through execution history
- See mobile reconfigure as code changes
- Git history as mobile evolution

---

## References

- [Alexander Calder Foundation](https://www.calder.org/)
- [Calder's mobiles at MoMA](https://www.moma.org/artists/898)
- [Physics simulation for games](https://gafferongames.com/post/integration_basics/)
- [Verlet integration](https://en.wikipedia.org/wiki/Verlet_integration) - Good for constrained systems
- [matter.js](https://brm.io/matter-js/) - 2D physics engine
- [p5.js](https://p5js.org/) - Creative coding framework

---

## Why Calder?

> "I want to make things that are fun to look at." — Alexander Calder

Code structure is usually invisible. A mobile makes it:
- **Tangible** - You can "feel" the structure
- **Dynamic** - It responds and moves
- **Beautiful** - Art, not just utility
- **Intuitive** - Balance is universal

The best visualizations don't just show data—they make you *feel* it. A mobile lets you feel the weight and balance of your code.

---

*Concept: December 2024*
