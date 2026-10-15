// Report the actual picture inside a contained canvas, excluding letterboxes.
// The embedding host uses those empty regions for feed navigation.
export function observeViewport(canvas, report = data => parent.postMessage(data, '*')) {
  let previous;
  const measure = () => {
    const box = canvas.getBoundingClientRect();
    if (!box.width || !box.height || !canvas.width || !canvas.height) return;
    const scale = Math.min(box.width / canvas.width, box.height / canvas.height);
    const width = canvas.width * scale, height = canvas.height * scale;
    const rect = {x: (box.x + (box.width-width)/2)/innerWidth,
      y: (box.y + (box.height-height)/2)/innerHeight, width: width/innerWidth, height: height/innerHeight};
    const key = JSON.stringify(rect);
    if (key !== previous) { previous = key; report({type:'mime:viewport',rect}); }
  };
  new ResizeObserver(measure).observe(canvas);
  new MutationObserver(measure).observe(canvas,{attributes:true,attributeFilter:['width','height']});
  window.addEventListener('resize',measure);
  measure();
}
