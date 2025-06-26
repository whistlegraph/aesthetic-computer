// Ticker Demo, 2025.6.26.xx.xx
// A demonstration of the Ticker class for scrolling text.

let tickers = [];

// 🥾 Boot
function boot({ gizmo }) {
  // Create multiple tickers with different configurations
  tickers.push(
    new gizmo.Ticker("Welcome to Aesthetic Computer!", { 
      speed: 2, 
      separator: " ★ " 
    })
  );
  
  tickers.push(
    new gizmo.Ticker("Scrolling text made easy with the Ticker class", { 
      speed: 1, 
      separator: " → " 
    })
  );
  
  tickers.push(
    new gizmo.Ticker("Different speeds and separators", { 
      speed: 3, 
      separator: " ♦ " 
    })
  );
}

// 🎨 Paint
function paint({ wipe, screen, ink }) {
  wipe("darkblue");
  
  // Update and paint each ticker at different positions
  tickers.forEach((ticker, index) => {
    ticker.update(arguments[0]); // Pass the API object
    
    const y = 50 + index * 80;
    const colors = [
      { color: "yellow", alpha: 255 },
      { color: "cyan", alpha: 200 },
      { color: "lime", alpha: 180 }
    ];
    
    ticker.paint(arguments[0], 0, y, {
      ...colors[index],
      width: screen.width,
    });
  });
  
  // Instructions
  ink("white").write("Ticker Demo", { center: "x", y: 20 });
  ink("gray").write("Multiple scrolling text examples", { center: "x", y: screen.height - 20 });
}

export { boot, paint };
