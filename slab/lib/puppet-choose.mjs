import { randomUUID } from 'node:crypto';
import { boundedRecentActions, chooseObservedTarget } from './jev-computer-use.mjs';
import { evaluateConfiguredChoices } from './jev-config.mjs';

const selected = node => node.properties?.find(p => p.name === 'selected')?.value?.value;
const selectionState = nodes => JSON.stringify(nodes.filter(n => !n.ignored && typeof selected(n) === 'boolean')
  .map(n => [n.backendDOMNodeId, n.role?.value, selected(n)]).sort((a, b) => a[0] - b[0]));

// Read-only selection. Only the bounded goal, accessible labels, selection state
// and caller-supplied action summaries leave this host. Page/node IDs and locators stay local.
export async function choosePageTarget(page, target, args, options = {}) {
  if (typeof args.goal !== 'string' || !args.goal.trim() || args.goal.length > 500)
    throw new Error('Provide a goal of 1–500 characters');
  const fallback = reason => ({ action: 'observe', reason, target, performed: false });
  if (args.previousOutcome === 'unknown') return fallback('verify_previous_action');
  const recentActions = boundedRecentActions(args.recentActions);
  if (recentActions.some(action => action.outcome === 'unknown')) return fallback('verify_previous_action');
  const observation = { id: randomUUID(), target, capturedAt: new Date().toISOString() };
  const initialURL = page.url();
  const session = await page.context().newCDPSession(page);
  let nodes;
  try { ({ nodes } = await session.send('Accessibility.getFullAXTree')); }
  finally { await session.detach(); }
  const roles = new Set(['button', 'link', 'menuitem', 'tab', 'radio', 'checkbox', 'option']);
  const controls = nodes.filter(n => !n.ignored && roles.has(n.role?.value) &&
    n.name?.value?.length > 0 && n.name.value.length <= 160 &&
    !n.properties?.some(p => p.name === 'disabled' && p.value?.value === true));
  // Never silently omit a possible answer on a dense page.
  if (controls.length > 40) return fallback('too_many_controls');
  const candidates = [];
  for (const n of controls) {
    const locator = { role: n.role.value, name: n.name.value };
    const match = page.getByRole(locator.role, { name: locator.name, exact: true });
    if (await match.count() === 1 && await match.isVisible() && await match.isEnabled())
      candidates.push({ id: `control_${candidates.length}`, label: locator.name,
        role: locator.role, visible: true, locator, node: n.backendDOMNodeId,
        ...(typeof selected(n) === 'boolean' ? { selected: selected(n) } : {}) });
  }
  const decision = await chooseObservedTarget({ goal: args.goal, observation, candidates, recentActions },
    { evaluate: evaluateConfiguredChoices, ...options });
  if (page.isClosed() || page.url() !== initialURL) return fallback('page_changed');
  if (decision.action === 'target') {
    // Re-read identity after inference: a replacement with the same label is
    // still a different observed control. Actual input remains puppet_click.
    const fresh = await page.context().newCDPSession(page);
    let now;
    try { ({ nodes: now } = await fresh.send('Accessibility.getFullAXTree')); }
    finally { await fresh.detach(); }
    if (selectionState(now) !== selectionState(nodes)) return fallback('page_state_changed');
    const c = decision.candidate;
    if (!now.some(n => n.backendDOMNodeId === c.node && !n.ignored &&
        n.role?.value === c.role && n.name?.value === c.label &&
        !n.properties?.some(p => p.name === 'disabled' && p.value?.value === true)))
      return fallback('control_changed');
    const match = page.getByRole(c.locator.role, { name: c.label, exact: true });
    if (await match.count() !== 1 || !await match.isVisible() || !await match.isEnabled())
      return fallback('control_changed');
    delete c.node;
  }
  return decision;
}
