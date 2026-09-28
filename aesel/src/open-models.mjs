// open-models.mjs — the open-weight models Aesel offers, one list for both
// ways of paying for them: the person's own OpenRouter key (open-server.mjs)
// and braincells through aesthetic.computer (ac-server.mjs). No imports, so
// the phone's browser build can read it too.

// Names a person would type, and what the picker says about each: how capable
// (out of five) and how dear. The marks come from Aesel's own trials on
// 2026-09-28 — five coding tasks with hidden tests and a stick-figure piece —
// not from a leaderboard; prices are OpenRouter's that day, per million tokens
// as cache-read / input / output. An agent day is mostly cache reads.
export const OPEN_MODEL_INFO = {
  flash: { id: "deepseek/deepseek-v4.1-flash", label: "DeepSeek V4.1 Flash", smart: 4, cost: 1 }, // 5/5, best scene · 0.006 / 0.30 / 1.20
  deepseek: { id: "deepseek/deepseek-v4-pro", label: "DeepSeek V4 Pro", smart: 4, cost: 2 }, // 5/5, plain scene · 0.065 / 0.78 / 1.57
  kimi: { id: "moonshotai/kimi-k3", label: "Kimi K3", smart: 4, cost: 4 }, // 5/5, good scene · 0.30 / 3.00 / 15.0
  qwen: { id: "qwen/qwen3.7-plus", label: "Qwen 3.7 Plus", smart: 3, cost: 1 }, // 5/5, scene lost its ground · 0.064 / 0.32 / 1.28
  minimax: { id: "minimax/minimax-m3", label: "MiniMax M3", smart: 3, cost: 1 }, // 5/5 slowly, scene lost its ground · 0.06 / 0.30 / 1.20
  glm: { id: "z-ai/glm-5.3-flash", label: "GLM 5.3 Flash", smart: 2, cost: 1 }, // 5/5 slowly, piece drew nothing · 0.03 / 0.15 / 0.50
};
export const OPEN_MODELS = Object.fromEntries(Object.entries(OPEN_MODEL_INFO).map(([name, info]) => [name, info.id]));

export const DEFAULT_OPEN_MODEL = OPEN_MODELS.flash;
