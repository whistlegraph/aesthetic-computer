// mandelbrot.mjs - Zooming Mandelbrot set with pixel manipulation
// For recording gifs and mp4s with the orchestrator 

const { floor, abs, sqrt } = Math;

let time = 0;
let zoom = 1;
let maxIterations = 100;
const zoomSpeed = 1.02; // Zoom factor per frame
const centerX = -0.7269; // Interesting point in the Mandelbrot set
const centerY = 0.1889;

// Color palette for the fractal
const colorPalette = [
  [0, 7, 100],       // Deep blue
  [32, 107, 203],    // Blue
  [237, 255, 255],   // Light cyan
  [255, 170, 0],     // Orange
  [0, 2, 0],         // Dark green
  [0, 7, 100],       // Back to deep blue (for cycling)
];

function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { screen } = api;
  time += 0.02;
  
  // Increase zoom exponentially but reset periodically for endless zoom
  zoom *= zoomSpeed;
  if (zoom > 1000000) {
    zoom = 1; // Reset zoom for continuous loop
  }
  
  // Calculate the bounds of our view
  const scale = 4.0 / zoom;
  const minX = centerX - scale;
  const maxX = centerX + scale;
  const minY = centerY - scale;
  const maxY = centerY + scale;
  
  // Render each pixel
  for (let y = 0; y < screen.height; y++) {
    for (let x = 0; x < screen.width; x++) {
      // Map pixel coordinates to complex plane
      const real = minX + (x / screen.width) * (maxX - minX);
      const imag = minY + (y / screen.height) * (maxY - minY);
      
      // Calculate Mandelbrot iterations for this point
      const iterations = mandelbrotIterations(real, imag, maxIterations);
      
      // Get color based on iterations
      const color = getColor(iterations, maxIterations, time);
      
      // Set pixel in screen buffer
      const pixelIndex = (y * screen.width + x) * 4;
      screen.pixels[pixelIndex] = color[0];     // R
      screen.pixels[pixelIndex + 1] = color[1]; // G
      screen.pixels[pixelIndex + 2] = color[2]; // B
      screen.pixels[pixelIndex + 3] = 255;      // A
    }
  }
}

function mandelbrotIterations(cReal, cImag, maxIter) {
  let zReal = 0;
  let zImag = 0;
  let iterations = 0;
  
  while (iterations < maxIter) {
    // Calculate z^2 + c
    const zRealSquared = zReal * zReal;
    const zImagSquared = zImag * zImag;
    
    // Check if point escaped (magnitude > 2)
    if (zRealSquared + zImagSquared > 4) {
      break;
    }
    
    // z = z^2 + c
    const newZReal = zRealSquared - zImagSquared + cReal;
    const newZImag = 2 * zReal * zImag + cImag;
    
    zReal = newZReal;
    zImag = newZImag;
    iterations++;
  }
  
  return iterations;
}

function getColor(iterations, maxIterations, time) {
  if (iterations === maxIterations) {
    // Point is in the set - use black with slight animation
    const brightness = floor(10 + 5 * Math.sin(time * 2));
    return [brightness, brightness, brightness];
  }
  
  // Point escaped - use colorful gradient
  const colorIndex = (iterations + time * 10) % (colorPalette.length - 1);
  const colorFloor = floor(colorIndex);
  const colorFrac = colorIndex - colorFloor;
  
  // Interpolate between two colors in the palette
  const color1 = colorPalette[colorFloor];
  const color2 = colorPalette[colorFloor + 1];
  
  const r = floor(color1[0] + (color2[0] - color1[0]) * colorFrac);
  const g = floor(color1[1] + (color2[1] - color1[1]) * colorFrac);
  const b = floor(color1[2] + (color2[2] - color1[2]) * colorFrac);
  
  // Add some brightness variation based on iteration count
  const brightness = 0.5 + 0.5 * (iterations / maxIterations);
  
  return [
    floor(r * brightness),
    floor(g * brightness), 
    floor(b * brightness)
  ];
}

function act({ event, api }) {
  const { screen } = api;
  // Adjust zoom speed with keyboard
  if (event.key === "ArrowUp") {
    zoomSpeed = Math.min(1.1, zoomSpeed + 0.01);
  }
  if (event.key === "ArrowDown") {
    zoomSpeed = Math.max(1.001, zoomSpeed - 0.01);
  }
  // Reset zoom
  if (event.key === " " || event.key === "Spacebar") {
    zoom = 1;
  }
}

// Export the piece
export { paint, act };