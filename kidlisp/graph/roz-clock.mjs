// The simulation runs at 60 Hz even when presentation runs at 30 Hz. Every
// intermediate feedback step executes. Catch-up is bounded after suspension.
export function createRozClock({stepMs=1000/60,maxSteps=4}={}) {
  let previous,accumulator=0;
  return {
    reset(){previous=undefined;accumulator=0;},
    tick(now,{paused=false,blocked=false}={}) {
      if(!Number.isFinite(now))throw new TypeError("Invalid animation time");
      if(previous===undefined)previous=now;
      const elapsed=Math.max(0,now-previous);previous=now;
      if(paused){accumulator=0;return 0;}
      accumulator=Math.min(maxSteps*stepMs,accumulator+elapsed);
      if(blocked)return 0;
      const steps=Math.min(maxSteps,Math.floor((accumulator+1e-6)/stepMs));
      accumulator=Math.max(0,accumulator-steps*stepMs);return steps;
    },
  };
}
