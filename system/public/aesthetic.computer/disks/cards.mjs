// Cards, 2025.1.17.21.30.00.000
// Display a single playing card that fills the screen.

/* 📝 Engineering Notes
  - Shows one card per screen that fills the display
  - Use parameters to specify card: "cards 3c" = 3 of clubs
  - Card notation: value + suit (a/2-9/t/j/q/k + s/h/d/c)
  - Click to flip card face up/down
  - Space to get random card
*/

// 🎴 Card System Data
const SUITS = {
  s: { symbol: "♠", color: "black", name: "spades", full: "Spades" },
  h: { symbol: "♥", color: "red", name: "hearts", full: "Hearts" },
  d: { symbol: "♦", color: "red", name: "diamonds", full: "Diamonds" },
  c: { symbol: "♣", color: "black", name: "clubs", full: "Clubs" }
};

const VALUES = {
  a: { display: "A", name: "Ace", value: 1 },
  "2": { display: "2", name: "Two", value: 2 },
  "3": { display: "3", name: "Three", value: 3 },
  "4": { display: "4", name: "Four", value: 4 },
  "5": { display: "5", name: "Five", value: 5 },
  "6": { display: "6", name: "Six", value: 6 },
  "7": { display: "7", name: "Seven", value: 7 },
  "8": { display: "8", name: "Eight", value: 8 },
  "9": { display: "9", name: "Nine", value: 9 },
  t: { display: "10", name: "Ten", value: 10 },
  j: { display: "J", name: "Jack", value: 11, face: true },
  q: { display: "Q", name: "Queen", value: 12, face: true },
  k: { display: "K", name: "King", value: 13, face: true }
};

// Global state
let currentCard = null;
let faceUp = true;

// 🃏 Card creation and utilities
function parseCardString(cardStr) {
  if (!cardStr || cardStr.length < 2) return null;
  
  const valueKey = cardStr.slice(0, -1).toLowerCase();
  const suitKey = cardStr.slice(-1).toLowerCase();
  
  const value = VALUES[valueKey];
  const suit = SUITS[suitKey];
  
  if (!value || !suit) return null;
  
  return { value, suit };
}

function getRandomCard() {
  const valueKeys = Object.keys(VALUES);
  const suitKeys = Object.keys(SUITS);
  
  const randomValue = valueKeys[Math.floor(Math.random() * valueKeys.length)];
  const randomSuit = suitKeys[Math.floor(Math.random() * suitKeys.length)];
  
  return {
    value: VALUES[randomValue],
    suit: SUITS[randomSuit]
  };
}

// Draw a playing card
function drawCard({ ink, box, write, line, screen }, card, faceUp) {
  const margin = 40;
  const cardWidth = screen.width - margin * 2;
  const cardHeight = screen.height - margin * 2;
  const cardX = margin;
  const cardY = margin;
  
  if (!faceUp) {
    // Draw card back
    ink("darkblue");
    box(cardX, cardY, cardWidth, cardHeight);
    
    // Ornate back pattern
    ink("gold");
    const centerX = cardX + cardWidth/2;
    const centerY = cardY + cardHeight/2;
    
    // Decorative border
    for (let i = 0; i < 10; i++) {
      const offset = i * 8;
      line(cardX + offset, cardY + offset, cardX + cardWidth - offset, cardY + offset);
      line(cardX + cardWidth - offset, cardY + offset, cardX + cardWidth - offset, cardY + cardHeight - offset);
      line(cardX + cardWidth - offset, cardY + cardHeight - offset, cardX + offset, cardY + cardHeight - offset);
      line(cardX + offset, cardY + cardHeight - offset, cardX + offset, cardY + offset);
    }
    
    // Center pattern
    write("🂠", centerX, centerY, 120, "center");
    return;
  }
  
  // Draw face-up card
  ink("white");
  box(cardX, cardY, cardWidth, cardHeight);
  
  // Card border
  ink("black");
  line(cardX, cardY, cardX + cardWidth, cardY);
  line(cardX + cardWidth, cardY, cardX + cardWidth, cardY + cardHeight);
  line(cardX + cardWidth, cardY + cardHeight, cardX, cardY + cardHeight);
  line(cardX, cardY + cardHeight, cardX, cardY);
  
  // Set color for suit
  ink(card.suit.color === "red" ? "red" : "black");
  
  // Card value and suit in corners
  const fontSize = Math.min(cardWidth, cardHeight) / 10;
  
  // Top-left
  write(card.value.display, cardX + 20, cardY + 30, fontSize);
  write(card.suit.symbol, cardX + 20, cardY + 60, fontSize);
  
  // Bottom-right (rotated)
  write(card.value.display, cardX + cardWidth - 40, cardY + cardHeight - 30, fontSize);
  write(card.suit.symbol, cardX + cardWidth - 40, cardY + cardHeight - 60, fontSize);
  
  // Center decoration
  const centerX = cardX + cardWidth/2;
  const centerY = cardY + cardHeight/2;
  
  if (card.value.face) {
    // Face cards: J, Q, K
    ink("purple");
    write(card.value.name.toUpperCase(), centerX, centerY, fontSize * 2, "center");
    
    // Ornate border for face cards
    ink(card.suit.color === "red" ? "red" : "black");
    const borderOffset = 40;
    for (let i = 0; i < 3; i++) {
      line(cardX + borderOffset + i, cardY + borderOffset, cardX + cardWidth - borderOffset - i, cardY + borderOffset);
      line(cardX + borderOffset, cardY + borderOffset + i, cardX + borderOffset, cardY + cardHeight - borderOffset - i);
      line(cardX + cardWidth - borderOffset - i, cardY + borderOffset, cardX + cardWidth - borderOffset - i, cardY + cardHeight - borderOffset);
      line(cardX + borderOffset, cardY + cardHeight - borderOffset - i, cardX + cardWidth - borderOffset, cardY + cardHeight - borderOffset - i);
    }
  } else if (card.value.value === 1) {
    // Ace - large center symbol
    write(card.suit.symbol, centerX, centerY, fontSize * 4, "center");  } else {
    // Number cards - draw suit symbols in pattern
    drawNumberPattern({ write, ink, box, line }, card, cardX, cardY, cardWidth, cardHeight, fontSize);
  }
}

// Draw number card patterns (2-10)
function drawNumberPattern({ write, ink, box, line }, card, cardX, cardY, cardWidth, cardHeight, fontSize) {
  const value = card.value.value;
  if (value < 2 || value > 10) return;
  
  const positions = [];
  const symbolSize = Math.floor(fontSize * 1.5);
  
  // Calculate positions based on card value
  switch (value) {
    case 2:
      positions.push({x: cardWidth/2, y: cardHeight/3});
      positions.push({x: cardWidth/2, y: cardHeight*2/3});
      break;
    case 3:
      positions.push({x: cardWidth/2, y: cardHeight/4});
      positions.push({x: cardWidth/2, y: cardHeight/2});
      positions.push({x: cardWidth/2, y: cardHeight*3/4});
      break;
    case 4:
      positions.push({x: cardWidth/3, y: cardHeight/3});
      positions.push({x: cardWidth*2/3, y: cardHeight/3});
      positions.push({x: cardWidth/3, y: cardHeight*2/3});
      positions.push({x: cardWidth*2/3, y: cardHeight*2/3});
      break;
    case 5:
      positions.push({x: cardWidth/3, y: cardHeight/4});
      positions.push({x: cardWidth*2/3, y: cardHeight/4});
      positions.push({x: cardWidth/2, y: cardHeight/2});
      positions.push({x: cardWidth/3, y: cardHeight*3/4});
      positions.push({x: cardWidth*2/3, y: cardHeight*3/4});
      break;
    case 6:
      positions.push({x: cardWidth/3, y: cardHeight/4});
      positions.push({x: cardWidth*2/3, y: cardHeight/4});
      positions.push({x: cardWidth/3, y: cardHeight/2});
      positions.push({x: cardWidth*2/3, y: cardHeight/2});
      positions.push({x: cardWidth/3, y: cardHeight*3/4});
      positions.push({x: cardWidth*2/3, y: cardHeight*3/4});
      break;
    case 7:
      positions.push({x: cardWidth/3, y: cardHeight/5});
      positions.push({x: cardWidth*2/3, y: cardHeight/5});
      positions.push({x: cardWidth/2, y: cardHeight*2/5});
      positions.push({x: cardWidth/3, y: cardHeight*3/5});
      positions.push({x: cardWidth*2/3, y: cardHeight*3/5});
      positions.push({x: cardWidth/3, y: cardHeight*4/5});
      positions.push({x: cardWidth*2/3, y: cardHeight*4/5});
      break;
    case 8:
      positions.push({x: cardWidth/3, y: cardHeight/6});
      positions.push({x: cardWidth*2/3, y: cardHeight/6});
      positions.push({x: cardWidth/3, y: cardHeight/3});
      positions.push({x: cardWidth*2/3, y: cardHeight/3});
      positions.push({x: cardWidth/3, y: cardHeight*2/3});
      positions.push({x: cardWidth*2/3, y: cardHeight*2/3});
      positions.push({x: cardWidth/3, y: cardHeight*5/6});
      positions.push({x: cardWidth*2/3, y: cardHeight*5/6});
      break;
    case 9:
      for (let row = 0; row < 3; row++) {
        for (let col = 0; col < 3; col++) {
          positions.push({
            x: cardWidth/4 + col * cardWidth/4,
            y: cardHeight/4 + row * cardHeight/4
          });
        }
      }
      break;
    case 10:
      for (let row = 0; row < 4; row++) {
        for (let col = 0; col < 3; col++) {
          if (row < 3 || col === 1) {
            positions.push({
              x: cardWidth/4 + col * cardWidth/4,
              y: cardHeight/5 + row * cardHeight/5
            });
          }
        }
      }
      break;
  }  
  // Draw symbols at calculated positions
  ink(card.suit.color === "red" ? "red" : "black");
  
  positions.forEach(pos => {
    const x = cardX + pos.x;
    const y = cardY + pos.y;
    
    // Draw suit symbol using simple shapes
    if (card.suit.name === "hearts") {
      // Draw heart shape with circles and triangle
      const size = symbolSize / 4;
      ink("red");
      // Two circles for top of heart
      box(x - size, y - size/2, size, size);
      box(x, y - size/2, size, size);
      // Triangle for bottom
      line(x - size, y, x + size/2, y + size);
      line(x + size/2, y + size, x + size, y);
      line(x + size, y, x - size, y);
    } else if (card.suit.name === "diamonds") {
      // Draw diamond shape
      const size = symbolSize / 3;
      ink("red");
      line(x, y - size, x + size, y);
      line(x + size, y, x, y + size);
      line(x, y + size, x - size, y);
      line(x - size, y, x, y - size);
    } else if (card.suit.name === "clubs") {
      // Draw club shape with circles
      const size = symbolSize / 4;
      ink("black");
      // Three circles
      box(x - size/2, y - size, size, size);
      box(x - size, y - size/2, size, size);
      box(x, y - size/2, size, size);
      // Stem
      line(x, y, x, y + size);
    } else if (card.suit.name === "spades") {
      // Draw spade shape
      const size = symbolSize / 3;
      ink("black");
      // Top point
      line(x, y - size, x - size, y);
      line(x, y - size, x + size, y);
      // Sides
      line(x - size, y, x - size/2, y + size/2);
      line(x + size, y, x + size/2, y + size/2);
      // Bottom curves
      line(x - size/2, y + size/2, x, y);
      line(x + size/2, y + size/2, x, y);
      // Stem
      line(x, y, x, y + size/2);
    }
  });
}

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params }) {
  // Parse card from parameters (e.g., "3c" for 3 of clubs)
  if (params[0]) {
    currentCard = parseCardString(params[0]);
  }
  
  // If no valid card specified, show random card
  if (!currentCard) {
    currentCard = getRandomCard();
  }
  
  faceUp = true;
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, box, write, line, screen }) {
  wipe(0, 100, 0); // Green felt background
  
  // Draw the card
  if (currentCard) {
    drawCard({ ink, box, write, line, screen }, currentCard, faceUp);
  }
  
  // Instructions at top
  ink(255, 255, 255, 200);
  write("Click: Flip | Space: Random Card", screen.width/2, 20, 16, "center");
}

// 🎪 Act (Runs once per user interaction)
function act({ event: e }) {
  if (e.is("touch") || e.is("draw")) {
    // Click anywhere to flip the card
    faceUp = !faceUp;
  }
  
  if (e.is("keyboard:down")) {
    if (e.key === " ") {
      // Space for random card
      currentCard = getRandomCard();
      faceUp = true;
    }
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Playing Card",
    desc: "Display a single playing card. Use 'cards 3c' to show specific cards.",
  };
}

// 🖼️ Preview
function preview({ ink, wipe, write, box, line }) {
  wipe(0, 100, 0);
  
  // Draw a simple card preview
  ink("white");
  box(5, 5, 40, 60);
  ink("black");
  line(5, 5, 45, 5);
  line(45, 5, 45, 65);
  line(45, 65, 5, 65);
  line(5, 65, 5, 5);
  
  ink("red");
  write("A", 8, 15, 12);
  write("♥", 8, 25, 12);
  write("♥", 25, 40, 24, "center");
}

export { boot, paint, act, meta, preview };
