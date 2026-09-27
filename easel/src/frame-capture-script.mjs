// Reads AC's software canvas or preserved WebGL composite, never app/desktop chrome.
export const CAPTURE_SCRIPT = `(()=>{
 try {
 const root=document.getElementById('aesthetic-computer');
 if(!root)return {pending:true,url:location.href};
 const visible=c=>c&&getComputedStyle(c).display!=='none';
 if(visible(root.querySelector('canvas[data-type="webgpu"]')))throw new Error('WebGPU capture is not supported by this canvas reader');
 const composite=root.querySelector('canvas[data-type="webgl-composite"]');
 const gpu=visible(composite)?composite:null;
 const canvas=gpu||[...root.children].find(c=>c.tagName==='CANVAS'&&!c.dataset.type);
 if(!canvas||!canvas.width||!canvas.height)return {pending:true,url:location.href};
 const width=canvas.width,height=canvas.height;
 if(!width||!height||width>2048||height>2048||width*height>1048576)throw new Error('Canvas exceeds capture limit (1 megapixel)');
 let surface=canvas;
 if(gpu){const gl=gpu.getContext('webgl2')||gpu.getContext('webgl');if(!gl?.getContextAttributes()?.preserveDrawingBuffer)throw new Error('WebGL buffer is not preserved for capture');surface=document.createElement('canvas');surface.width=width;surface.height=height;surface.getContext('2d').drawImage(gpu,0,0);}
 const ctx=surface.getContext('2d');if(!ctx)throw new Error('Pixel buffer unavailable');
 const data=ctx.getImageData(0,0,width,height).data;
 let raw='';for(let at=0;at<data.length;at+=8192)raw+=String.fromCharCode(...data.subarray(at,at+8192));
 return {width,height,rgba:btoa(raw),png:surface.toDataURL('image/png').split(',')[1],source:gpu?'webgl-composite':'software-canvas',renderedRevisionVerified:false,url:location.href};
 }catch(error){return {error:String(error.message||error),url:location.href};}
})()`;
