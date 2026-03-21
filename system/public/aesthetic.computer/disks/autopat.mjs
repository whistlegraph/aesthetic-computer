// Autopat — autoplay Notepat with a simple track.

import * as notepat from "./notepat.mjs";

const DEFAULT_RAW_SONG = `
  C:auto D:pat E:plays F:the G:keys A:by B:it- C:self.
  C:let D:it E:roll F:while G:you A:watch B:the C:track.
`;

const JUKEBOX = [
  {
    title: "Twinkle",
    bpm: 88,
    rawSong: `
      C:Twin- C:-kle G:twin- G:-kle A:lit- A:-tle G:star,
      F:how F:I E:won- E:-der D:what D:you C:are.
      G:Up G:a- F:-bove F:the E:world E:so D:high,
      G:like G:a F:dia- F:-mond E:in E:the D:sky.
    `,
  },
  {
    title: "Bach (excerpt)",
    bpm: 90,
    rawSong: `
      C:ba- E:ch G:pre- C:lude E:roll G:ing C:on.
      D:ar- F:pe- A:g- D:gi- F:a- A:tion.
    `,
  },
  {
    title: "Scale",
    bpm: 110,
    rawSong: `
      C:do D:re E:mi F:fa G:so A:la B:ti +C:do.
      +C:do B:ti A:la G:so F:fa E:mi D:re C:do.
    `,
  },
];

const PANEL_PADDING = 6;
const PANEL_WIDTH = 136;
const ROW_HEIGHT = 12;

let selectedIndex = 0;
let playing = false;
let playButton;
let tuneButtons = [];
let panel = { x: 0, y: 0, width: PANEL_WIDTH, height: 0 };

function pickRawSong(params) {
  if (!Array.isArray(params)) return DEFAULT_RAW_SONG;
  const customMelodyParams = params.filter((param) => param.includes(":"));
  if (customMelodyParams.length > 0) return customMelodyParams.join(" ");
  return DEFAULT_RAW_SONG;
}

function pickBpm(params) {
  if (!Array.isArray(params)) return 88;
  const numeric = params.find((param) => !Number.isNaN(Number(param)));
  return numeric ? Number(numeric) : 88;
}

function applyAutopatConfig({ rawSong, bpm, paused } = {}) {
  notepat.configureAutopat({
    enabled: true,
    rawSong,
    bpm,
    beatsPerNote: 0.75,
    restBeats: 0.1,
    startDelay: 0.2,
    ignoreInput: true,
    hudLabel: "autopat",
    paused,
    showTrack: false,
    sidePanelWidth: PANEL_WIDTH + PANEL_PADDING * 2,
  });
}

function layoutButtons({ screen, ui }) {
  const totalRows = 1 + JUKEBOX.length;
  panel.width = Math.min(PANEL_WIDTH, Math.max(96, screen.width - PANEL_PADDING * 2));
  panel.height = totalRows * ROW_HEIGHT + PANEL_PADDING * 2;
  panel.x = Math.max(PANEL_PADDING, screen.width - panel.width - PANEL_PADDING);
  panel.y = 36;

  playButton = new ui.Button(
    panel.x + PANEL_PADDING,
    panel.y + PANEL_PADDING,
    panel.width - PANEL_PADDING * 2,
    ROW_HEIGHT,
  );

  tuneButtons = JUKEBOX.map((_, index) =>
    new ui.Button(
      panel.x + PANEL_PADDING,
      panel.y + PANEL_PADDING + ROW_HEIGHT * (index + 1),
      panel.width - PANEL_PADDING * 2,
      ROW_HEIGHT,
    ),
  );
}

function boot($) {
  const rawSong = pickRawSong($.params);
  const bpm = pickBpm($.params);

  if (rawSong !== DEFAULT_RAW_SONG) {
    JUKEBOX.unshift({ title: "Custom", bpm, rawSong });
    selectedIndex = 0;
  }

  applyAutopatConfig({ rawSong, bpm, paused: true });
  layoutButtons($);

  return notepat.boot($);
}

function sim($) {
  return notepat.sim($);
}

function paint($) {
  notepat.paint($);

  if (!playButton) return;

  const { ink, screen } = $;
  const activeTune = JUKEBOX[selectedIndex];
  const playLabel = playing ? "PAUSE" : "PLAY";

  ink(0, 0, 0, 140).box(panel.x, panel.y, panel.width, panel.height);
  ink(0, 0, 0, 200).box(panel.x, panel.y, panel.width, panel.height, "outline");

  playButton.paint((btn) => {
    ink(60, 180, 140, btn.down ? 220 : 190).box(btn.box);
    ink(0, 0, 0, 200).box(btn.box, "outline");
    ink(10, 10, 10, 200).write(playLabel, btn.box.x + 8, btn.box.y + 2);
  });

  tuneButtons.forEach((btn, index) => {
    const tune = JUKEBOX[index];
    const isSelected = index === selectedIndex;
    const bg = isSelected ? [80, 140, 220, 210] : [20, 30, 40, 160];
    ink(...bg).box(btn.box);
    ink(0, 0, 0, 200).box(btn.box, "outline");
    ink(235, 235, 240, 230).write(
      tune.title,
      btn.box.x + 6,
      btn.box.y + 2,
    );
  });

  if (activeTune) {
    ink(255, 255, 255, 180).write(
      `${activeTune.bpm}bpm`,
      panel.x + panel.width - 52,
      panel.y + 2,
    );
  }
}

function act($) {
  const { event: e, ui, screen, sound, api } = $;

  if (e.is("reframed")) {
    layoutButtons($);
  }

  playButton?.act(e, {
    push: () => {
      playing = !playing;
      const tune = JUKEBOX[selectedIndex] || {};
      applyAutopatConfig({
        rawSong: tune.rawSong || DEFAULT_RAW_SONG,
        bpm: tune.bpm || 88,
        paused: !playing,
      });
      if (playing && api?.send) {
        api.send({
          type: "audio:reinit",
          content: {
            latencyHint: 0.005,
            sampleRate: 48000,
            speakerPerformanceMode: "disabled",
          },
        });
      }
    },
  });

  tuneButtons.forEach((btn, index) => {
    btn.act(e, {
      push: () => {
        selectedIndex = index;
        const tune = JUKEBOX[index];
        applyAutopatConfig({
          rawSong: tune.rawSong,
          bpm: tune.bpm,
          paused: !playing,
        });
      },
    });
  });

  return notepat.act($);
}

function meta() {
  return {
    title: "autopat",
    desc: "Autoplay jukebox for notepat with play/pause controls.",
  };
}

export { boot, sim, paint, act, meta };
