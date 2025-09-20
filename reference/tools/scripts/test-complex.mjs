// Complex test piece for real-time sixel rendering
// Features: animations, gradients, shapes, text, and geometric patterns

let frame = 0;

export function paint(api) {
  frame++;
  
  // Clear with animated background color
  const bgHue = (frame * 2) % 360;
  api.wipe(`hsl(${bgHue}, 20%, 10%)`);
  
  // Animated spiral pattern
  const centerX = 64;
  const centerY = 64;
  const maxRadius = 40;
  
  for (let i = 0; i < 20; i++) {
    const angle = (frame * 0.1 + i * 0.3) % (Math.PI * 2);
    const radius = (i / 20) * maxRadius;
    const x = centerX + Math.cos(angle) * radius;
    const y = centerY + Math.sin(angle) * radius;
    
    // Rainbow colors
    const hue = (i * 18 + frame * 5) % 360;
    api.ink(`hsl(${hue}, 80%, 60%)`);
    api.circle(x, y, 3 + Math.sin(frame * 0.2 + i) * 2);
  }
  
  // Animated border rectangles
  for (let i = 0; i < 5; i++) {
    const size = 10 + i * 20;
    const offset = Math.sin(frame * 0.15 + i) * 5;
    const hue = (frame * 3 + i * 60) % 360;
    
    api.ink(`hsl(${hue}, 70%, 50%)`);
    api.rect(
      centerX - size/2 + offset, 
      centerY - size/2 + offset, 
      size, 
      size
    );
  }
  
  // Animated waves at the top
  api.ink('cyan');
  for (let x = 0; x < 128; x += 2) {
    const y = 20 + Math.sin(x * 0.1 + frame * 0.2) * 10;
    api.point(x, y);
  }
  
  // Animated waves at the bottom
  api.ink('yellow');
  for (let x = 0; x < 128; x += 2) {
    const y = 108 + Math.sin(x * 0.15 + frame * 0.3) * 8;
    api.point(x, y);
  }
  
  // Moving text
  const textX = 20 + Math.sin(frame * 0.1) * 15;
  const textY = 15;
  api.ink('white');
  api.write(`Frame ${frame}`, textX, textY);
  
  // Corner indicators
  api.ink('red');
  api.circle(5, 5, 3);
  api.ink('green');
  api.circle(123, 5, 3);
  api.ink('blue');
  api.circle(5, 123, 3);
  api.ink('magenta');
  api.circle(123, 123, 3);
  
  // Geometric pattern in center
  const time = frame * 0.1;
  for (let i = 0; i < 8; i++) {
    const angle = (i * Math.PI / 4) + time;
    const x1 = centerX + Math.cos(angle) * 15;
    const y1 = centerY + Math.sin(angle) * 15;
    const x2 = centerX + Math.cos(angle + Math.PI) * 15;
    const y2 = centerY + Math.sin(angle + Math.PI) * 15;
    
    const hue = (i * 45 + frame * 2) % 360;
    api.ink(`hsl(${hue}, 90%, 70%)`);
    api.line(x1, y1, x2, y2);
  }
}

export const meta = {
  title: "Complex Real-time Test",
  description: "Complex animated graphics for testing real-time sixel rendering"
};