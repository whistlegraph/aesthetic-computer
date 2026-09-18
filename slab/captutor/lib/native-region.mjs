// A fixed 16:9 crop in native display pixels. No resampling or display changes.
export function nativeCanvasRegion(display, browser) {
 const sx=display.pixelWidth/display.width, sy=display.pixelHeight/display.height;
 if(!Number.isFinite(sx)||sx<=0||Math.abs(sx-sy)>0.001)throw Error('Unsupported native display scale');
 const r=browser.canvas;
 if(!r||![r.x,r.y,r.width,r.height,browser.screenX,browser.screenY,browser.outerWidth,browser.outerHeight,browser.innerWidth,browser.innerHeight].every(Number.isFinite))throw Error('Canvas geometry unavailable');
 const inset=8;
 const left=browser.screenX+(browser.outerWidth-browser.innerWidth)/2+r.x+inset;
 const top=browser.screenY+(browser.outerHeight-browser.innerHeight)+r.y+inset;
 const width=(r.width-inset*2)*sx,height=(r.height-inset*2)*sy;
 const unit=Math.floor(Math.min(width/32,height/18));
 if(unit<20)throw Error('Canvas is too small for a readable 16:9 recording');
 const w=unit*32,h=unit*18;
 const x=Math.ceil((left*sx+(width-w)/2)/2)*2,y=Math.ceil((top*sy+(height-h)/2)/2)*2;
 const region={x,y,w,h};validateNativeRegion(region,{w:display.pixelWidth,h:display.pixelHeight});return region;
}
export function validateNativeRegion(r,source){
 if(!r||![r.x,r.y,r.w,r.h].every(Number.isInteger)||r.x<0||r.y<0||r.w<=0||r.h<=0||r.w%2||r.h%2||r.w*9!==r.h*16||r.x+r.w>source.w||r.y+r.h>source.h)throw Error('Invalid native 16:9 region or display geometry changed');
 return `crop=${r.w}:${r.h}:${r.x}:${r.y}`;
}
