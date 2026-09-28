// Native screen capture is a desktop capability. Never imply a phone capture succeeded.
export const FRAME_TOOL = {name:'ac_frame',description:'Desktop preview capture (unavailable in the phone client).',inputSchema:{type:'object',properties:{}}};
export async function captureFrame() { throw new Error('Native preview capture is unavailable in the phone client.'); }
