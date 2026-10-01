// Recording metadata belongs to history/playback, not ordinary code generation.
export function wantsSoundEvidence(transcript) {
  const text = transcript.trim();
  return !text || /\b(?:like this|this (?:sound|melody|rhythm|tune|whistle|humming|recording)|my (?:voice|whistle|humming|singing|recording)|(?:match|follow|use|copy|visualize) (?:the |my )?(?:sound|audio|pitch|rhythm|melody))\b/i.test(text);
}
export function inferenceRequest(request) {
  const marker = '\nINPUT DATA:\n', at = request.indexOf(marker);
  if (at < 0) return request;
  const input = JSON.parse(request.slice(at + marker.length));
  const transcript = input.transcript.trim();
  if (!wantsSoundEvidence(transcript)) return transcript;
  const {recordingID, ...sound} = input.sound;
  return (transcript || 'Create a playable interpretation of this nonverbal sound.') +
    '\nSound reference: measured pitch and energy only; speech prosody is not necessarily music. Preserve the spoken intent.\n' +
    JSON.stringify({words: input.words, sound});
}
