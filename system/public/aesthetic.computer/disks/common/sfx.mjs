export function push(sound) {
  sound.synth({
    type: "sine",
    tone: 800,
    attack: 0.1,
    decay: 0.99,
    volume: 0.75,
    duration: 0.005,
  });
}

export function down(sound) {
  sound.synth({
    type: "sine",
    tone: 600,
    attack: 0.1,
    decay: 0.99,
    volume: 0.75,
    duration: 0.001,
  });
}
