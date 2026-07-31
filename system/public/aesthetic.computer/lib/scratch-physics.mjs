export const SCRATCH_SECONDS_PER_SCREEN = 1.75;

export function scratchProgressDelta(
  deltaX,
  screenWidth,
  tapeDuration,
  secondsPerScreen = SCRATCH_SECONDS_PER_SCREEN,
) {
  if (!(screenWidth > 0) || !(tapeDuration > 0)) return 0;
  return (deltaX / screenWidth) * (secondsPerScreen / tapeDuration);
}

export function scratchRateFromMotion(
  deltaX,
  deltaSeconds,
  screenWidth,
  secondsPerScreen = SCRATCH_SECONDS_PER_SCREEN,
) {
  if (!(deltaSeconds > 0) || !(screenWidth > 0)) return 0;
  return (deltaX / screenWidth) * (secondsPerScreen / deltaSeconds);
}

export function directionalCruiseTarget(rate, motion = 0) {
  const signal = Math.abs(motion) > 0.01 ? motion : rate;
  return signal < 0 ? -1 : 1;
}

export function easeRateToward(rate, target, retention) {
  return target + (rate - target) * retention;
}
