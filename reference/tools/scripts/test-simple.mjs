// Simple 128x128 test - text and circles

export const api = {
  paint: ({ wipe, ink, write, circle, screen }) => {
    // Clear to dark blue
    wipe(0, 0, 100);
    
    // Write some text
    ink(255, 255, 255); // White
    write("HELLO", 10, 20);
    write("128x128", 20, 40);
    
    // Draw some circles
    ink(255, 0, 0); // Red
    circle(32, 80, 15);
    
    ink(0, 255, 0); // Green
    circle(64, 80, 12);
    
    ink(255, 255, 0); // Yellow
    circle(96, 80, 10);
    
    // Blue border circle
    ink(0, 100, 255);
    circle(64, 64, 50);
  }
};

export const meta = {
  title: "Simple 128x128 Test",
  author: "AC System",
  desc: "Simple test with text and circles",
  date: "2025.09.16",
  version: "1.0.0"
};