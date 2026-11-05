// Exact pitches and separate owners for every input/channel/note. No analytics.
export function createMidiPerformance({ start, stop, bend = () => {}, program = () => {} }) {
  const notes = new Map(), pedals = new Map(), bends = new Map();
  const channelKey = (source, channel) => `${source.length}:${source}:${channel}`;
  const release = id => { const voice = notes.get(id); if (!voice) return; notes.delete(id); stop(voice); };
  const clear = predicate => { for (const [id, voice] of notes) if (predicate(voice)) release(id); };
  return {
    notes,
    receive({ source = "default", data, disconnected = false }) {
      if (disconnected) {
        clear(v => v.source === source);
        for (const key of pedals.keys()) if (key.startsWith(`${source.length}:${source}:`)) pedals.delete(key);
        for (const key of bends.keys()) if (key.startsWith(`${source.length}:${source}:`)) bends.delete(key);
        return;
      }
      if (!data) return;
      const [status, value, amount = 0] = data, kind = status & 0xf0, channel = status & 15;
      const owner = channelKey(source, channel), id = `${owner}:${value}`;
      if (kind === 0x90 && amount > 0) {
        release(id);
        const voice = { id, source, channel, note: value, velocity: amount, released: false };
        voice.handle = start(voice);
        notes.set(id, voice);
        bend(voice, bends.get(owner) || 0);
      } else if (kind === 0x80 || kind === 0x90) {
        const voice = notes.get(id);
        if (!voice) return;
        if (pedals.get(owner)) voice.released = true; else release(id);
      } else if (kind === 0xe0) {
        const semitones = ((amount << 7 | value) - 8192) / 8192 * 2;
        bends.set(owner, semitones);
        for (const voice of notes.values()) if (channelKey(voice.source, voice.channel) === owner) bend(voice, semitones);
      } else if (kind === 0xc0) program(value);
      else if (kind === 0xb0) {
        const same = v => v.source === source && v.channel === channel;
        if (value === 64 || value === 121) {
          const down = value === 64 && amount >= 64;
          pedals.set(owner, down);
          if (!down) clear(v => same(v) && v.released);
          if (value === 121) { bends.delete(owner); for (const voice of notes.values()) if (same(voice)) bend(voice, 0); }
        } else if (value === 120) clear(same);
        else if (value === 123) for (const [key, voice] of notes) if (same(voice)) {
          if (pedals.get(owner)) voice.released = true; else release(key);
        }
      }
    },
    panic() { clear(() => true); pedals.clear(); bends.clear(); },
  };
}
