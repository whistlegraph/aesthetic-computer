// Noise Pattern Test - Stress test blur with different patterns
// This generates the same noise patterns as our stress test

let frameCount = 0;

function paint({ api }) {
  const { wipe, ink, box, blur, spin, scroll, noise16 } = api;
  
  frameCount++;
  console.log(`🎨 Noise frame ${frameCount}`);
  
  // Only generate pattern and wipe on the first frame
  if (frameCount % 5 === 0) {
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
    ink().line();
  }
  
  // Apply blur every frame
  console.log(`🌀 Applying blur(3) on frame ${frameCount}`);
  blur(3);
  scroll(10);
  spin(1);
  console.log(`✅ Blur completed for frame ${frameCount}`);
}

export { paint };