// Parse modules without evaluating user code in the session web view.
import {parse} from '../../src/vendor/acorn.mjs';
export async function validatePieceSource(source, file) {
  if (typeof source !== 'string' || !source.trim()) throw new Error('The piece is empty.');
  if (!String(file).endsWith('.mjs')) return;
  if (source.includes('/* ac-artifact ')) {
    const matches=[...source.matchAll(/\/\* ac-artifact ([\s\S]*?)\*\//g)];
    if(matches.length!==1)throw new Error('Use exactly one ac-artifact descriptor.');
    const json=matches[0][1].trim();let artifact;
    try{artifact=JSON.parse(json);}catch{throw new Error('Artifact descriptor must be valid JSON.');}
    if(json.length>320||/[^\x20-\x7e]/.test(json))throw new Error('Keep the compact artifact descriptor within 320 ASCII bytes.');
    if(artifact.version!==1||artifact.kind!=='shirt-symbol'||!['star','heart','flower','rainbow','custom','drawing'].includes(artifact.shape)||!/^#[0-9a-fA-F]{6}$/.test(artifact.color))throw new Error('Invalid shirt-symbol version, shape, or six-digit hex color.');
    if(artifact.animation!==undefined&&!['spin','pulse','float'].includes(artifact.animation))throw new Error('Animation must be spin, pulse, or float.');
    if(artifact.shape==='drawing'){
      if(!Array.isArray(artifact.draw)||artifact.draw.length<1||artifact.draw.length>12)throw new Error('Drawing needs 1 to 12 commands.');
      for(const [i,c] of artifact.draw.entries()){
        if(!Array.isArray(c)||![0,1,2].includes(c[0])||c.length!==[5,7,8][c[0]])throw new Error(`Drawing command ${i+1} has the wrong number of values. Circle exactly [0,x,y,r,c]; stroke [1,x1,y1,x2,y2,width,c]; triangle [2,x1,y1,x2,y2,x3,y3,c]. Fix the descriptor and write it again.`);
        if(c.some(v=>!Number.isInteger(v)||Math.abs(v)>16)||c.at(-1)<0||c.at(-1)>2||c[0]===0&&(c[3]<1||c[3]>9)||c[0]===1&&(c[5]<1||c[5]>6))throw new Error(`Drawing command ${i+1} has out-of-range values.`);
      }
    }
    if(artifact.shape==='custom'&&(!Array.isArray(artifact.points)||artifact.points.length<3||artifact.points.length>10||artifact.points.some(p=>!Array.isArray(p)||p.length!==2||p.some(v=>!Number.isInteger(v)||Math.abs(v)>9))))throw new Error('Custom outlines need 3 to 10 integer [x,y] points from -9 to 9.');
  }
  try { parse(source, {ecmaVersion:'latest',sourceType:'module'}); }
  catch(error) { throw new Error(`Invalid JavaScript; previous preview kept. ${error.message}`); }
}

// The desktop module also exports `PieceRevisions`, a local snapshot store under
// ~/.local/share/easel/history. The phone keeps its history somewhere else, so
// this is a stub that fails loudly rather than a half-implementation that looks
// like it is saving and is not.
export class PieceRevisions {
  constructor() {
    throw new Error("PieceRevisions is not available in the phone client.");
  }
}
