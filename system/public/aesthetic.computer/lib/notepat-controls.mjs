import { createMidiController } from "./midi-controller.mjs";
import { createGmBridge } from "./notepat-gm-bridge.mjs";

// Browser audio/MIDI services only. The piece draws and handles its own AC panel.
export function createNotepatControls({ send, context, destination, focus = () => {},
  createMidi = createMidiController, createGm = createGmBridge,
  events = globalThis.window, visibility = globalThis.document }) {
  let disposed=false,current={wave:"gm",program:0,local:true},midiState={},soundStatus='',controlsOpen=false;
  const control=content=>send({type:'notepat:control',content});
  const publish=extra=>{if(!disposed)send({type:'notepat:state',content:{hostReady:true,controlsOpen,
    midiConnected:!!midiState.connected,midiOutput:!!midiState.outputId,midi:midiState,instrument:current,soundStatus,...extra}})};
  const midi=createMidi({message:content=>send({type:'midi:keyboard',content}),changed:state=>{midiState=state;publish()}});
  const gm=createGm({context,destination,status:content=>{
    soundStatus=content.error||(content.loading?'Loading instrument...':'');
    publish({gmReady:content.ready,gmLoading:!!content.loading});
  }});
  let preparedProgram,preparedContext;
  function prepareInstrument(){
    if(current.wave!=='gm'||!current.local)return;
    const ctx=context();
    if(preparedProgram===current.program&&preparedContext===ctx)return;
    preparedProgram=current.program;preparedContext=ctx;void gm.program(current.program);
  }
  const panic=()=>{midi.panic();gm.panic();control({kind:'panic'})};
  const hidden=()=>{if(visibility?.hidden)panic()};
  const escape=e=>{if(controlsOpen&&e.key==='Escape'){e.preventDefault();e.stopImmediatePropagation();controlsOpen=false;publish()}};
  events?.addEventListener('keydown',escape,true);
  events?.addEventListener('blur',panic);visibility?.addEventListener('visibilitychange',hidden);
  midiState=midi.state();publish();
  return {
    message(content){
      if(disposed||!content)return;
      const {op}=content;prepareInstrument();
      if(op==='open'){panic();controlsOpen=true;publish()}
      else if(op==='close'){controlsOpen=false;publish()}
      else if(op==='state'){current={...current,...content};prepareInstrument();publish()}
      else if(op==='connect')void midi.connect();
      else if(op==='select-input')midi.selectInput(content.value);
      else if(op==='select-output'){midi.selectOutput(content.value);if(current.wave==='gm')midi.program(current.program)}
      else if(op==='select-channel'){midi.selectChannel(Number(content.value));if(current.wave==='gm')midi.program(current.program)}
      else if(op==='select-sound'){
        const value=String(content.value);
        control(value.startsWith('gm:')?{kind:'program',program:Number(value.slice(3))}:{kind:'wave',wave:value});
      }
      else if(op==='local')control({kind:'local',enabled:content.enabled===true});
      else if(op==='record')control({kind:'record'});
      else if(op==='program'){gm.program(content.program);if(!content.fromMidi)midi.program(content.program)}
      else if(op==='gm-on')gm.note(content);
      else if(op==='gm-off')gm.stop(content.id,content.fade);
      else if(op==='gm-bend')gm.update(content);
      else if(op==='midi-on')midi.noteOn(content.id,content.note,content.velocity);
      else if(op==='midi-off')midi.noteOff(content.id);
      else if(op==='stop-notes')panic();
      else if(op==='panic'){midi.panic();gm.panic()}
    },
    dispose(){disposed=true;midi.disconnect();gm.panic();focus(false);events?.removeEventListener('keydown',escape,true);events?.removeEventListener('blur',panic);visibility?.removeEventListener('visibilitychange',hidden)},
  };
}
