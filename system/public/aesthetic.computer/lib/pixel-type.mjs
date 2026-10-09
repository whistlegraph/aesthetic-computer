import {glyphRows} from './pixel-font-data.mjs';
// AC's 6×10 glyphs, written directly into RGBA pixels. No browser font rasterizer.
export function pixelWrite(buffer,text,x,y,{color=[240,242,250,255],scale=1}={}) {
 scale=Math.max(1,Math.min(8,Math.floor(scale)));x=Math.floor(x);y=Math.floor(y);const start=x;
 for(const char of String(text)) {
  if(char==='\n'){x=start;y+=12*scale;continue;}
  const rows=glyphRows[char]||glyphRows['?'];
  for(let row=0;row<10;row++)for(let col=0;col<6;col++)if(rows[row]&(1<<col)) {
   for(let dy=0;dy<scale;dy++)for(let dx=0;dx<scale;dx++) {
    const px=x+col*scale+dx,py=y+row*scale+dy;
    if(px>=0&&py>=0&&px<buffer.width&&py<buffer.height)buffer.pixels.set(color,(px+py*buffer.width)*4);
   }
  }
  x+=6*scale;
 }
 return {x,y};
}
export class PixelProfiler {
 constructor(){this.reset();this.hud={width:44,height:14,pixels:new Uint8ClampedArray(44*14*4)};this.version=0;this.label('...fps');}
 reset(now=performance.now()){this.start=now;this.frames=0;this.fps=0;this.work=0;this.cpuMs=0;}
 label(text){text=String(text).slice(0,32);const width=text.length*6+8;if(this.hud?.width!==width)this.hud={width,height:14,pixels:new Uint8ClampedArray(width*14*4)};for(let i=0;i<this.hud.pixels.length;i+=4)this.hud.pixels.set([8,11,23,255],i);pixelWrite(this.hud,text,4,2);this.version++;}
 sample(now=performance.now(),cpuMs=0){this.frames++;this.work+=cpuMs;const elapsed=now-this.start;if(elapsed>=500){this.fps=Math.round(this.frames*1000/elapsed);this.cpuMs=this.work/this.frames;this.label(`${this.fps}fps`);this.start=now;this.frames=this.work=0;return true;}return false;}
 paint(buffer){const {width,height,pixels}=this.hud,x=Math.max(0,buffer.width-width-2),y=Math.max(0,buffer.height-height-2);for(let row=0;row<height&&y+row<buffer.height;row++){const count=Math.min(width,buffer.width-x)*4;buffer.pixels.set(pixels.subarray(row*width*4,row*width*4+count),((y+row)*buffer.width+x)*4);}return buffer;}
}
