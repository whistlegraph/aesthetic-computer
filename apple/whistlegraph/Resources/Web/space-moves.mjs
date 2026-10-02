export const spaceMoves=[
 'Make a white line',
 'Transform this horizontal line into a small pointed spaceship hull, facing upward. Preserve its visual continuity: the old line becomes the wide base and two new diagonal edges meet at a nose above it. White outline on the same dark background. Static; no wings, windows, stars or exhaust yet.',
 'Add two symmetric swept-back wings to the same hull. Make the silhouette clearly recognizable as a little spaceship, keeping the pointed nose and body. Static; no stars or exhaust yet.',
 'Add a cyan cockpit window inside the upper hull and a small pink stripe on each wing. Keep all existing ship geometry. Static, no extra text or interface.',
 'Add a warm orange and yellow exhaust flame below the ship. Animate its length with a gentle repeating pulse, while the hull and wings remain stationary. Keep the cockpit and stripes. IMPORTANT: the nozzle is at the center of the REARMOST horizontal edge (the existing tail coordinate), not at the original horizontal line y inside the body. Both flame edges must start exactly at tail, with all flame geometry below tail. Pulse its length between 5% and 15% of screen height, clamping to leave a margin at the bottom. No flame inside the body. No stars yet.',
 'Make the same spaceship fly through space: add a sparse field of small stars moving downward behind it, with wrapping at the screen edge, and a slight gentle bank of the complete ship. Preserve the hull, two wings, cyan cockpit, pink stripes, and attached animated exhaust. Keep the ship fully on screen. No text or extra interface.'
];
