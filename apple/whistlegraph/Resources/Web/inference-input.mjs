import {inputData,drawingEvidence,drawingGeometry} from './drawing-input.mjs';
// Recording identity belongs to history/playback, not ordinary code generation.
export function wantsSoundEvidence(transcript) {
  const text = transcript.trim();
  return !text || /\b(?:like this|this (?:sound|melody|rhythm|tune|whistle|humming|recording)|my (?:voice|whistle|humming|singing|recording)|(?:match|follow|use|copy|visualize) (?:the |my )?(?:sound|audio|pitch|rhythm|melody))\b/i.test(text);
}
export function inferenceRequest(request) {
  const input=inputData(request);
  if(!input)return request;
  const transcript=input.transcript.trim();
  if(input.performance?.schema==='whistlegraph-performance/v1') {
    const {recordingID,...sound}=input.sound||{};
    return 'Interpret this Whistlegraph performance using coordinated drawing, voice, and sound. Follow explicit words, then use the measured temporal relationships to shape the result. Sound and words are milliseconds from microphone start. For drawing samples, audioTimeMs = point[2] - drawing.speechStartMs. Negative times mean marks made before recording. Read ordered strokes as a whole gesture, with the attached image supplying their complete form. Pitch and energy onsets are estimates, not certain notes or beats. Preserve the established piece when revising. Do not display the input data as text.\n'+JSON.stringify({performance:input.performance,transcript,words:input.words,sound,...(input.drawing?{drawing:drawingGeometry(input.drawing)}:{})});
  }
  let prompt=transcript|| (input.drawing?'Infer the intended edit from these chalk gestures and the current piece.':'Create a playable interpretation of this nonverbal sound.');
  // Drawing makes gesture/sound alignment relevant even for ordinary speech.
  if(input.sound&&(input.drawing||wantsSoundEvidence(transcript))){
    const {recordingID,...sound}=input.sound;
    prompt+='\nSound reference: measured pitch and energy only; speech prosody is not necessarily music. Preserve the spoken intent. Loudness and pitch changes can suggest emphasis but do not establish emotion or a musical beat.\n'+JSON.stringify({words:input.words,sound});
  }
  if(input.drawing)prompt+=drawingEvidence(input.drawing);
  return prompt;
}
