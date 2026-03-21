import blessed from 'blessed';

// Create a screen object
const screen = blessed.screen({
  smartCSR: true,
  title: 'Blue Square'
});

// Create a box to represent the blue square
const blueSquare = blessed.box({
  top: 'center',
  left: 'center',
  width: '50%',
  height: '50%',
  style: {
    bg: 'blue'
  }
});

// Append the blue square to the screen
screen.append(blueSquare);

// Quit the program when 'q' is pressed
screen.key(['q', 'C-c'], () => process.exit(0));

// Render everything
screen.render();
