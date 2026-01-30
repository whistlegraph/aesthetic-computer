// Copilot, 2024.12.10.00.11.15.914
// A demonstration piece created by GitHub Copilot coding agent.
// Shows an animated AI-themed visualization with pulsing rings and particles.

let particles = [];
let frame = 0;
const MAX_PARTICLES = 50;

function boot({ wipe }) {
  wipe(20, 15, 35); // Dark purple background
}

function paint({ wipe, ink, line, circle, screen, num }) {
  frame += 1;
  
  // Animated dark background
  const bg = Math.floor(20 + Math.sin(frame * 0.02) * 5);
  wipe(bg, bg - 5, bg + 15);
  
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  
  // Draw pulsing concentric rings (AI "thinking" effect)
  for (let i = 0; i < 5; i++) {
    const pulse = Math.sin(frame * 0.05 + i * 0.5) * 0.3 + 0.7;
    const radius = (20 + i * 15) * pulse;
    const alpha = Math.floor(100 + Math.sin(frame * 0.03 + i) * 50);
    
    // Cyan/purple gradient rings
    const r = Math.floor(50 + i * 20);
    const g = Math.floor(150 + Math.sin(frame * 0.02) * 50);
    const b = Math.floor(200 + i * 10);
    
    ink(r, g, b, alpha);
    
    // Draw ring as segments
    const segments = 32;
    for (let j = 0; j < segments; j++) {
      const a1 = (j / segments) * Math.PI * 2 + frame * 0.01 * (i % 2 ? 1 : -1);
      const a2 = ((j + 1) / segments) * Math.PI * 2 + frame * 0.01 * (i % 2 ? 1 : -1);
      
      const x1 = cx + Math.cos(a1) * radius;
      const y1 = cy + Math.sin(a1) * radius;
      const x2 = cx + Math.cos(a2) * radius;
      const y2 = cy + Math.sin(a2) * radius;
      
      line(x1, y1, x2, y2);
    }
  }
  
  // Spawn particles occasionally
  if (frame % 5 === 0 && particles.length < MAX_PARTICLES) {
    const angle = num.rand() * Math.PI * 2;
    particles.push({
      x: cx,
      y: cy,
      vx: Math.cos(angle) * (0.5 + num.rand() * 1.5),
      vy: Math.sin(angle) * (0.5 + num.rand() * 1.5),
      life: 60 + Math.floor(num.rand() * 40),
      size: 1 + Math.floor(num.rand() * 2),
      hue: num.rand()
    });
  }
  
  // Update and draw particles
  for (let i = particles.length - 1; i >= 0; i--) {
    const p = particles[i];
    p.x += p.vx;
    p.y += p.vy;
    p.life -= 1;
    
    if (p.life <= 0) {
      particles.splice(i, 1);
      continue;
    }
    
    // Fade out as life decreases
    const maxLife = 100;
    const alpha = Math.floor((p.life / maxLife) * 255);
    const r = Math.floor(100 + p.hue * 100);
    const g = Math.floor(200 + Math.sin(p.hue * Math.PI) * 55);
    const b = 255;
    
    ink(r, g, b, alpha);
    circle(p.x, p.y, p.size, true);
  }
  
  // Draw center dot
  ink(255, 255, 255, 200);
  circle(cx, cy, 3, true);
}

// 📰 Meta
function meta() {
  return {
    title: "Copilot",
    desc: "An AI-themed visualization with pulsing rings and particles.",
  };
}

export { boot, paint, meta };
