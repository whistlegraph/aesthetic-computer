import {inputData,drawingEvidence} from './drawing-input.mjs';
// Recording identity belongs to history/playback, not ordinary code generation.
export function wantsSoundEvidence(transcript) {
  const text = transcript.trim();
  return !text || /\b(?:like this|this (?:sound|melody|rhythm|tune|whistle|humming|recording)|my (?:voice|whistle|humming|singing|recording)|(?:match|follow|use|copy|visualize) (?:the |my )?(?:sound|audio|pitch|rhythm|melody))\b/i.test(text);
}
export function inferenceRequest(request) {
  const input=inputData(request);
  if(!input)return request;
  const transcript=input.transcript.trim();
  let prompt=transcript|| (input.drawing?'Interpret this drawing.':'Create a playable interpretation of this nonverbal sound.');
  // Drawing makes gesture/sound alignment relevant even for ordinary speech.
  if(input.sound&&(input.drawing||wantsSoundEvidence(transcript))){
    const {recordingID,...sound}=input.sound;
    prompt+='\nSound reference: measured pitch and energy only; speech prosody is not necessarily music. Preserve the spoken intent. Loudness and pitch changes can suggest emphasis but do not establish emotion or a musical beat.\n'+JSON.stringify({words:input.words,sound});
  }
  if(input.drawing)prompt+=drawingEvidence(input.drawing);
  return prompt;
}
