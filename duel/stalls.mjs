// 🧊 Stalls: when the picture stops for longer than a few frames, say what
// the main thread was doing. Numbers only: no handles, rooms, prompts or text.
//
// A stall is a gap between two frames. Its cause is the largest thing we can
// see inside the gap:
//   render / step  the frame before it ran long (drawing, or everything else)
//   socket:<type>  a message handler ran long (the kind of message is named)
//   starved        nothing of ours ran long: another script, a collection, the
//                  compositor, or the machine itself kept the frame away
export const STALL_MS = 50;
const TYPES = new Set(['welcome', 'role', 'presence', 'handle-history', 'braincells', 'chalk', 'chalk-ops',
  'dev-log', 'pong', 'identity', 'objects', 'build-accepted', 'build-result', 'snapshot']);

export function cause({ gap, work = 0, render = 0, message = 0, messageType = '' }) {
  if (work >= gap / 2) return render >= work / 2 ? 'render' : 'step';
  if (message >= gap / 2) return `socket:${TYPES.has(messageType) ? messageType : 'other'}`;
  return 'starved';
}

export class Stalls {
  constructor({ send = () => {}, now = () => performance.now() } = {}) {
    this.send = send;
    this.now = now;
    this.list = []; // The newest few, for the HUD.
    this.count = 0;
    this.lastAt = 0; // The frame before this one, on the frame clock.
    this.work = 0; // What that frame cost, and its parts.
    this.render = 0;
    this.predict = 0;
    this.message = 0; // Handler time since that frame, and its longest kind.
    this.messageMax = 0;
    this.messageType = '';
    this.sentAt = -1e9;
    this.sent = 0;
  }
  // A socket handler finished.
  handled(type, ms) {
    this.message += ms;
    if (ms > this.messageMax) {
      this.messageMax = ms;
      this.messageType = type;
    }
  }
  // A frame begins. `watching` is false while nobody is looking (a hidden tab,
  // a parked or resting world), so a throttled timer is never called a stall.
  begin(at, watching, context = {}) {
    const gap = this.lastAt ? at - this.lastAt : 0;
    if (watching && this.watched && gap >= STALL_MS) {
      const row = {
        gap: Math.round(gap),
        work: Math.round(this.work),
        render: Math.round(this.render),
        predict: Math.round(this.predict),
        message: Math.round(this.message),
        messageType: TYPES.has(this.messageType) ? this.messageType : '',
        late: Math.round(Math.max(0, this.now() - at)), // How long the frame waited to be run.
        ...context,
      };
      row.cause = cause(row);
      this.count++;
      this.list = [...this.list, row].slice(-4);
      // A few a second at most, and a ceiling for the whole visit.
      const t = this.now();
      if (t - this.sentAt > 250 && this.sent < 400) {
        this.sentAt = t;
        this.sent++;
        this.send({ type: 'stall', ...row });
      }
    }
    this.lastAt = at;
    this.watched = watching;
    this.message = this.messageMax = 0;
    this.messageType = '';
  }
  // The frame's own cost, once it is drawn.
  end({ work = 0, render = 0, predict = 0 }) {
    this.work = work;
    this.render = render;
    this.predict = predict;
  }
  // One short line for the fps readout.
  note() {
    const last = this.list.at(-1);
    return last ? `${this.count} stalls ${last.gap}ms ${last.cause}` : '';
  }
}

// The server's side: a bounded, anonymous ring of what clients reported.
const NUMBERS = ['gap', 'work', 'render', 'predict', 'message', 'late', 'building', 'objects', 'pending', 'rtt'];
const CAUSE = /^(render|step|starved|socket:[a-z-]{1,20})$/;
const ENUMS = { backend: ['webgpu', 'webgl2'], agent: ['app', 'safari', 'chrome', 'firefox', 'other'] };
export function stallRow(data, { at = Date.now(), session = '', revision = '' } = {}) {
  if (!data || !CAUSE.test(data.cause || '')) return null;
  const row = { at, session, revision, cause: data.cause };
  for (const k of NUMBERS) {
    const v = data[k];
    if (Number.isFinite(v) && v >= 0) row[k] = Math.min(600000, Math.round(v));
  }
  if (!(row.gap >= STALL_MS)) return null;
  if (TYPES.has(data.messageType)) row.messageType = data.messageType;
  for (const [k, allowed] of Object.entries(ENUMS)) if (allowed.includes(data[k])) row[k] = data[k];
  row.hosted = data.hosted === true;
  return row;
}
