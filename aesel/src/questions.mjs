// Questions the model asks mid-turn: Claude's AskUserQuestion and Codex's
// request_user_input. They are answered at the prompt, one question at a time —
// a number picks an option, several numbers pick several, anything else is the
// answer in the user's own words. Approval-shaped questions stay with the
// approval drawer (approvals.mjs); this takes the rest.
import {approvalFor} from './approvals.mjs';

const clean = value => String(value || '').replace(/[\u0000-\u0009\u000b-\u001f\u007f-\u009f]/g, ' ').trim();

export function questionFor(request) {
  const {id, method, params = {}} = request;
  let engine;
  if (method === 'item/tool/askUserQuestion') engine = 'claude';
  else if (method === 'item/tool/requestUserInput' && approvalFor(request)?.kind === 'unsupported') engine = 'codex';
  else return null;
  const raw = Array.isArray(params.questions) ? params.questions : [];
  // A secret is not typed into a transcript; leave those to the approval drawer's dismissal.
  if (!raw.length || raw.some(q => q?.isSecret)) return null;
  const questions = raw.map((q, index) => ({
    id: String(q.id ?? index),
    header: clean(q.header),
    question: clean(q.question),
    multiSelect: !!(q.multiSelect ?? q.multi_select),
    options: (Array.isArray(q.options) ? q.options : []).map(o => ({label: clean(o?.label), description: clean(o?.description)})).filter(o => o.label),
  }));
  return {id, method, engine, questions, index: 0, answers: []};
}

export function questionText(question, number, count) {
  const head = [count > 1 ? `${number}/${count}` : '', question.header].filter(Boolean).join(' · ');
  const lines = [`${head ? head + ' — ' : ''}${question.question}`];
  question.options.forEach((option, index) => lines.push(`  ${index + 1}. ${option.label}${option.description ? ` — ${option.description}` : ''}`));
  lines.push(question.options.length
    ? (question.multiSelect ? '  numbers like 1,3 pick several · or type your own answer' : '  a number picks · or type your own answer')
    : '  type your answer');
  return lines.join('\n');
}

// One typed line → the answer's labels.
export function answerFor(question, text) {
  const typed = String(text || '').trim();
  const picks = typed.split(/[\s,]+/).filter(Boolean);
  if (question.options.length && picks.length && picks.every(p => /^\d+$/.test(p))) {
    const labels = [...new Set(picks.map(Number))].map(n => question.options[n - 1]?.label);
    if (labels.every(Boolean) && (question.multiSelect || labels.length === 1)) return labels;
  }
  return [typed];
}

// Record one answer; returns the engine response once every question has one.
export function answerQuestion(pending, text) {
  pending.answers[pending.index] = answerFor(pending.questions[pending.index], text);
  pending.index += 1;
  if (pending.index < pending.questions.length) return null;
  if (pending.engine === 'claude') {
    return {decision: 'answer', answers: Object.fromEntries(pending.questions.map((q, i) => [q.question, pending.answers[i].join(', ')]))};
  }
  return {answers: Object.fromEntries(pending.questions.map((q, i) => [q.id, {answers: pending.answers[i]}]))};
}

export function declineQuestion(pending) {
  return pending.engine === 'claude' ? {decision: 'decline'} : {answers: {}};
}
