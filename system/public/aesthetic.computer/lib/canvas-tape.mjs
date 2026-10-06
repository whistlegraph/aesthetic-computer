// Shared hardware encoder for AC's HD tapes and Whistlegraph story exports.
// Only canvas pixels and explicitly supplied audio tracks enter the recording.
export function createCanvasTapeRecorder(canvas, {fps = 30, audioTracks = [], videoBitsPerSecond = 8e6, mp4Only = false} = {}) {
  const types = ['video/mp4;codecs=avc1.640028,mp4a.40.2', 'video/mp4', ...(!mp4Only ? ['video/webm;codecs=vp9,opus', 'video/webm'] : [])];
  const mimeType = types.find(type => MediaRecorder.isTypeSupported(type));
  if (!mimeType) throw Error('This device cannot encode an MP4 canvas tape.');
  const stream = canvas.captureStream(fps);
  const ownedTracks = stream.getVideoTracks();
  audioTracks.forEach(track => stream.addTrack(track));
  try {
    const recorder = new MediaRecorder(stream, {mimeType, videoBitsPerSecond});
    recorder.addEventListener('stop', () => ownedTracks.forEach(track => track.stop()), {once:true});
    return recorder;
  } catch (error) { ownedTracks.forEach(track => track.stop()); throw error; }
}
