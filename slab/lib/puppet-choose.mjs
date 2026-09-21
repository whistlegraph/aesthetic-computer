import { randomUUID } from 'node:crypto';
import { chooseObservedTarget } from './jev-computer-use.mjs';
import { evaluateConfiguredChoices } from './jev-config.mjs';

// Read-only selection. Only the bounded goal and accessible control labels are
// sent remotely. Exact page IDs, URLs, node IDs and locators stay on this host.
export async function choosePageTarget(page, target, args, options = {}) {
  if (typeof args.goal !== 'string' || !args.goal.trim() || args.goal.length > 500)
    throw new Error('Provide a goal of 1–500 characters');
  const fallback = reason => ({ action: 'observe', reason, target, performed: false });
  if (args.previousOutcome === 'unknown') return fallback('verify_previous_action');
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
        role: locator.role, visible: true, locator, node: n.backendDOMNodeId });
  }
  const decision = await chooseObservedTarget({ goal: args.goal, observation, candidates },
    { evaluate: evaluateConfiguredChoices, ...options });
  if (page.isClosed() || page.url() !== initialURL) return fallback('page_changed');
  if (decision.action === 'target') {
    // Re-read identity after inference: a replacement with the same label is
    // still a different observed control. Actual input remains puppet_click.
    const fresh = await page.context().newCDPSession(page);
    let now;
    try { ({ nodes: now } = await fresh.send('Accessibility.getFullAXTree')); }
    finally { await fresh.detach(); }
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
