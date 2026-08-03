// onboarding — the shared DOM/bridge contract between Fuser tutorials and Captutor.
//
// Product code owns tutorial IDs, ordering, requirements, and state. Captutor
// discovers that contract from semantic data attributes and the local-only
// `window.__fuserOnboardingAudit` bridge. This keeps screenplays away from
// translated copy, generated class names, portal structure, and Zustand internals.

const ACTIVE_OVERLAY = '[data-onboarding-overlay]';
const ACTIVE_TARGET = '[data-onboarding-active="true"]';
const INITIAL_DIALOG = '[data-onboarding-surface="initial-dialog"]';

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

const visibleAction = action => `js=(() => {
  const roots = [
    document.querySelector(${JSON.stringify(ACTIVE_OVERLAY)}),
    document.querySelector(${JSON.stringify(INITIAL_DIALOG)}),
    document,
  ].filter(Boolean);
  for (const root of roots) {
    const button = [...root.querySelectorAll(
      '[data-onboarding-action=${JSON.stringify(action)}]'
    )].find(element => element.getClientRects().length > 0 && !element.disabled);
    if (button) return button;
  }
  return null;
})()`;

export const onboardingSelectors = Object.freeze({
  activeOverlay: ACTIVE_OVERLAY,
  activeTarget: ACTIVE_TARGET,
  initialDialog: INITIAL_DIALOG,
  replayControl: '[data-neo-anchor="replay-editor-tutorial"]',
  nextAction: visibleAction('next'),
  finishAction: visibleAction('finish'),
  previousAction: visibleAction('previous'),
});

export async function readOnboardingState(cdp) {
  return cdp.eval(`(() => {
    const dialog = document.querySelector(${JSON.stringify(INITIAL_DIALOG)});
    const overlay = document.querySelector(${JSON.stringify(ACTIVE_OVERLAY)});
    const target = document.querySelector(${JSON.stringify(ACTIVE_TARGET)});
    const bridge = window.__fuserOnboardingAudit;
    const store = bridge?.snapshot?.() ?? null;
    const source = overlay || dialog;
    return {
      contractVersion: bridge?.version ?? null,
      engine: overlay?.dataset.onboardingEngine || (dialog ? 'classic-dialog' : 'classic'),
      step: source?.dataset.onboardingOverlay || source?.dataset.onboardingStep ||
        store?.currentStepper || null,
      group: target?.dataset.onboardingGroup || null,
      contentIndex: Number(
        source?.dataset.onboardingContentIndex ?? store?.currentStepperContentIndex ?? 0,
      ),
      requirement: overlay?.dataset.onboardingRequirement || null,
      requirementMet: overlay?.dataset.onboardingRequirementMet === 'true',
      actions: [...document.querySelectorAll('[data-onboarding-action]')]
        .filter(element => element.getClientRects().length > 0 && !element.disabled)
        .map(element => element.dataset.onboardingAction),
      targetAnchor: target?.dataset.neoAnchor || null,
      store,
    };
  })()`);
}

export async function waitForOnboardingStep(
  cdp,
  { step, contentIndex, timeoutMs = 20000 } = {},
) {
  const deadline = Date.now() + timeoutMs;
  let last = null;
  while (Date.now() < deadline) {
    last = await readOnboardingState(cdp);
    const stepMatches = step === undefined || last.step === step;
    const indexMatches =
      contentIndex === undefined || last.contentIndex === contentIndex;
    if (last.step && stepMatches && indexMatches) return last;
    await sleep(100);
  }
  throw new Error(
    `onboarding step timed out: expected ${JSON.stringify({ step, contentIndex })}, ` +
      `last ${JSON.stringify(last)}`,
  );
}

export async function satisfyOnboardingRequirement(cdp, requirement) {
  const result = await cdp.eval(`(() => {
    if (!['localhost', '127.0.0.1'].includes(location.hostname)) {
      return { ok:false, reason:'audit bridge is local-only' };
    }
    const bridge = window.__fuserOnboardingAudit;
    if (!bridge || bridge.version !== 1) {
      return { ok:false, reason:'onboarding audit bridge v1 is unavailable' };
    }
    bridge.completeRequirement(${JSON.stringify(requirement)});
    return { ok:true };
  })()`);
  if (!result?.ok) throw new Error(result?.reason || 'could not satisfy requirement');
  return result;
}

export async function advanceOnboarding(
  cdp,
  { click, satisfyRequirements = false, timeoutMs = 20000 } = {},
) {
  if (typeof click !== 'function') throw new Error('advanceOnboarding needs Captutor click');
  const before = await waitForOnboardingStep(cdp, { timeoutMs });
  const beforeKey = `${before.step}:${before.contentIndex}`;

  if (before.requirement && !before.requirementMet) {
    if (!satisfyRequirements) {
      throw new Error(`onboarding step ${beforeKey} needs ${before.requirement}`);
    }
    await satisfyOnboardingRequirement(cdp, before.requirement);
  } else if (before.actions.includes('next')) {
    await click(onboardingSelectors.nextAction);
  } else if (before.actions.includes('finish')) {
    await click(onboardingSelectors.finishAction);
  } else {
    throw new Error(`onboarding step ${beforeKey} has no forward action`);
  }

  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const after = await readOnboardingState(cdp);
    if (!after.step || `${after.step}:${after.contentIndex}` !== beforeKey) {
      return { before, after };
    }
    await sleep(100);
  }
  throw new Error(`onboarding did not advance from ${beforeKey}`);
}

export async function startOnboardingReplay(cdp) {
  const result = await cdp.eval(`(() => {
    if (!['localhost', '127.0.0.1'].includes(location.hostname)) {
      return { ok:false, reason:'audit bridge is local-only' };
    }
    const bridge = window.__fuserOnboardingAudit;
    if (!bridge || bridge.version !== 1) {
      return { ok:false, reason:'onboarding audit bridge v1 is unavailable' };
    }
    bridge.startReplay();
    return { ok:true };
  })()`);
  if (!result?.ok) throw new Error(result?.reason || 'could not start replay');
  return waitForOnboardingStep(cdp);
}
