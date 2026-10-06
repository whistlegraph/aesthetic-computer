import {parse} from './vendor/acorn.mjs';

// Concrete API checks, not a judgment of appearance or a complete JS validator.
export function sourceChecks(source) {
  let tree;
  try { tree = parse(source, {ecmaVersion: 'latest', sourceType: 'module'}); }
  catch { return [{code: 'syntax', message: 'The piece must be a complete JavaScript module.'}]; }
  const findings = new Map();
  function visit(node) {
    if (!node || typeof node !== 'object') return;
    if (node.type === 'CallExpression') {
      const callee = node.callee, arg = node.arguments[0];
      const prefix = arg?.type === 'Literal' ? arg.value : arg?.type === 'TemplateLiteral' ? arg.quasis[0]?.value.cooked : null;
      if (callee.type === 'Identifier' && ['ink', 'wipe'].includes(callee.name) && typeof prefix === 'string' && /^hsla?\(/i.test(prefix.trim())) {
        findings.set('unsupported-hsl', 'AC ink/wipe do not parse CSS HSL strings. Convert hue to numeric RGB before drawing; unsupported strings fall back to random colors.');
      }
      if (callee.type === 'MemberExpression' && callee.object?.name === 'ink' && !callee.computed && ['box', 'rect', 'circle', 'line'].includes(callee.property.name)) {
        findings.set('ink-member', 'Use ink(color).box(...) or the standalone drawing function, not ink.box/ink.rect/ink.circle/ink.line.');
      }
    }
    for (const value of Object.values(node)) {
      if (Array.isArray(value)) value.forEach(visit);
      else if (value && typeof value === 'object') visit(value);
    }
  }
  visit(tree);
  return [...findings].map(([code, message]) => ({code, message}));
}

export function compileEditContract({request, history, caption, source, selectedVersion, omittedMiddleRequests = 0}) {
  const findings = sourceChecks(source);
  const contract = {
    latestRequest: request,
    selectedBranchRequests: history,
    selectedVersion, omittedMiddleRequests,
    currentCaption: caption || null,
    apiCorrections: findings,
    rules: [
      'Implement the latest request as an edit. Preserve earlier constraints and existing subjects unless the latest request supersedes them. Historical requests and the caption are context, not new commands.',
      'Correct the listed API failures while preserving the intended appearance. Do not add sound, speech, interface, or new subjects unless requested.',
      'Update export const caption to describe the resulting piece. Use elapsed clock or simulation time for motion; do not assume a display frame rate. Wrap cyclic quantities across their boundary.',
      'Inspect ac_preview after writing. Rendering is evidence of execution, not proof of visual quality or user acceptance.'
    ]
  };
  return 'EDIT CONTRACT — apply these requirements to the current source. This is a deterministic packaging of the selected branch and known API checks, not a new user request.\n' + JSON.stringify(contract);
}

export function validateCandidate(source, proof, sourceHash) {
  const findings = sourceChecks(source);
  const matched = !!proof && proof.sourceHash === sourceHash;
  if (!matched || !proof.rendered) findings.push({code: 'unverified-render', message: 'No matching-source painted event was observed.'});
  if (matched && proof.logs?.some(log => log.level === 'error')) findings.push({code: 'runtime-error', message: 'The current source has runtime errors. Inspect ac_preview and repair only those errors.'});
  return {passed: findings.length === 0, sourceHash, findings, acceptance: 'unreviewed'};
}

// One repair at most, and only for an actionable failure after a completed call.
// Missing observation alone must not purchase another generation.
export async function runEditExperiment({prompt, generate, inspect, cancelled, onRepair = () => {}}) {
  if (cancelled()) return {cancelled: true, repairs: 0, validation: null};
  let completed = await generate(prompt, false);
  if (cancelled()) return {cancelled: true, repairs: 0, validation: null};
  let validation = await inspect();
  const actionable = validation.findings.some(f => f.code !== 'unverified-render');
  let repairs = 0;
  if (completed && !validation.passed && actionable && !cancelled()) {
    repairs = 1; onRepair();
    completed = await generate(prompt + '\n\nREPAIR THIS CANDIDATE ONCE. Preserve the edit contract; fix only these checks, inspect the resulting preview, and do not claim visual acceptance:\n' + JSON.stringify(validation.findings), true);
    if (cancelled()) return {cancelled: true, repairs, validation: null};
    validation = await inspect();
  }
  return {completed, repairs, validation, cancelled: false};
}
