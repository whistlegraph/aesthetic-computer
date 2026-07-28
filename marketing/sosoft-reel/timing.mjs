import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

export const SCENES = [
  ["SSF-00", "Introduction", null],
  ["SSF-01", "Jeffrey Alan Scudder — Notepat", "My contribution"],
  ["SSF-02", "Æther Cavendish — Vigil Score", "Æther Cavendish"],
  ["SSF-03", "Chelly Jin — Software as a Choreography", "Chelly Jin"],
  ["SSF-04", "Jordan Silver — Sonic Architecture", "Jordan Silver"],
  ["SSF-05", "Em Lugo — Cues for Losing Direction", "Em Lugo"],
  ["SSF-06", "Darlyn Phan — Line Piece 1", "Darlyn Phan"],
  ["SSF-07", "Thomas Noya — Biophonía", "Thomas Noya"],
  ["SSF-08", "Banyi Huang — A Cosmographic Score", "Banyi Huang"],
  ["SSF-09", "Alexander Espinosa — Music for World Computers", "Alexander Espinosa"],
  ["SSF-10", "Mavyn Vu — The Radio Is an Altar: Portal", "Mavyn Vu"],
  ["SSF-11", "Closing", "Casey Reas"],
];

export function loadNarrationSource(root) {
  const out = resolve(root, "out");
  const sourcePath = resolve(out, "narration-source.json");
  if (existsSync(sourcePath)) {
    const source = JSON.parse(readFileSync(sourcePath, "utf8"));
    const audio = resolve(root, source.audio);
    if (existsSync(audio)) return { ...source, audio };
  }
  return { kind: "synthetic", audio: resolve(out, "narration.mp3") };
}

export function loadNarrationTimeline(root) {
  const timelinePath = resolve(root, "out", "narration-timeline.json");
  if (existsSync(timelinePath)) return JSON.parse(readFileSync(timelinePath, "utf8"));

  // Compatibility for a pre-wizard synthetic render.
  const narration = readFileSync(resolve(root, "narration.txt"), "utf8").trim();
  const { alignment } = JSON.parse(readFileSync(resolve(root, "out", "narration-alignment.json"), "utf8"));
  const totalDuration = alignment.character_end_times_seconds.at(-1);
  const starts = SCENES.map(([, , phrase]) => phrase ? alignment.character_start_times_seconds[narration.indexOf(phrase)] : 0);
  return {
    source: "synthetic",
    totalDuration,
    lines: SCENES.map(([id, title], i) => ({
      id, title, startSec: starts[i], endSec: starts[i + 1] ?? totalDuration,
    })),
  };
}

export function sceneStart(timeline, id) {
  const scene = timeline.lines.find((line) => line.id === id);
  if (!scene) throw new Error(`narration timeline is missing ${id}`);
  return scene.startSec;
}
