// The look: before a cloud move, a vision model reads the painting so far and
// decides where to work, how much to change, and what the one move is. The
// painter's hint is read against the picture rather than pasted into a frame.
export const LOOK_MODEL = 'anthropic/claude-haiku-5.5';
export const LOOK_VERSION = 'look-v1';
export const SCALES = ['corner','half','whole'];

export const LOOK_SYSTEM = `You are the eye of No Paint, a turn-based painting game. The painting starts as noise. Each turn an image editor applies exactly one move to the current painting, and the painter says No or keeps it. You decide that move by looking.

Return only a JSON object with four string fields:
"region": where the move happens, named by what is actually in the picture (for example "the swirl in the lower left", "the pale band across the top", "the entire image"). Never answer with a quadrant unless something specific lives there.
"scale": "corner" (a small local area), "half" (a large part), or "whole" (the entire image).
"move": one sentence of concrete instruction for the image editor: a transformation of the pixels that are there, clearly visible, not a touch-up.
"why": one short sentence about what in the picture led you here.

Taste: one visible move per turn. Work on what has energy in the picture. Grow the painting rather than reset it; keep its material and rendering style unless asked otherwise. Vary your moves: bending, slicing, mirroring, recoloring, sorting, repeating, stippling, embossing, stretching, pinching, or introducing a figure or form that answers something already present. Early in a painting (few paints so far) moves can be bold and large. A painting with many kept moves wants smaller, more precise ones that respect what the painter has kept.

If the painter gives a hint, the hint is their wish and decides the scope. "Make it a cute bear" or "turn it into a forest" means the whole image becomes that, and the subject and style may change. "Add a bear" or "a bird in the corner" means a local move that fits the picture. Read the hint against the picture: put it where it belongs.`;

export function lookUserText({hint, paints}) {
  const count = Number.isInteger(paints) && paints >= 0 ? paints : null;
  return [
    count === null ? 'Here is the current painting.' : `Here is the current painting after ${count} kept ${count === 1 ? 'move' : 'moves'}.`,
    hint ? `The painter's hint: "${hint}".` : 'The painter gave no hint this turn.',
    'Decide the one move. Reply with only the JSON object.',
  ].join(' ');
}

export function parseLook(text) {
  if (typeof text !== 'string') return null;
  const start = text.indexOf('{'), end = text.lastIndexOf('}');
  if (start < 0 || end <= start) return null;
  let value;
  try { value = JSON.parse(text.slice(start, end + 1)); } catch { return null; }
  if (!value || typeof value !== 'object' || Array.isArray(value)) return null;
  const clean = (field, max) => typeof value[field] === 'string' ? value[field].replace(/\s+/g, ' ').trim().slice(0, max) : '';
  const region = clean('region', 160), move = clean('move', 360), why = clean('why', 200);
  const scale = SCALES.includes(value.scale) ? value.scale : null;
  if (!region || !move || !scale) return null;
  return {region, scale, move, why: why || undefined};
}

// Returns a look, or null when the model is unavailable or answers badly; the
// caller then falls back to the seeded move. Never throws over a bad look.
export function createLook({key, model = LOOK_MODEL, fetch = globalThis.fetch, timeout = 25_000}) {
  return async ({image, hint, paints}) => {
    try {
      const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
        method: 'POST', redirect: 'error', signal: AbortSignal.timeout(timeout),
        headers: {Authorization: `Bearer ${key}`, 'Content-Type': 'application/json'},
        body: JSON.stringify({model, max_tokens: 320, temperature: 0.8,
          messages: [
            {role: 'system', content: LOOK_SYSTEM},
            {role: 'user', content: [
              {type: 'text', text: lookUserText({hint, paints})},
              {type: 'image_url', image_url: {url: 'data:image/png;base64,' + image}},
            ]},
          ]}),
      });
      if (!response.ok) return null;
      const result = await response.json();
      const look = parseLook(result.choices?.[0]?.message?.content);
      if (!look) return null;
      return {...look, model, version: LOOK_VERSION,
        cost_usd: Number.isFinite(result.usage?.cost) ? result.usage.cost : null};
    } catch { return null; }
  };
}
