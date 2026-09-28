// OpenRouter alpha Decisions; shared by installed Easel and local experiments.
export async function evaluateChoices({ state, questions }, {
  apiKey = process.env.OPENROUTER_API_KEY, fetchImpl = globalThis.fetch,
  signal = AbortSignal.timeout(1500),
} = {}) {
  if (!apiKey) throw new Error('Set OPENROUTER_API_KEY.');
  if (state == null || !questions || !Object.keys(questions).length ||
      Object.values(questions).some(q => q.type !== 'choice'))
    throw new Error('Provide state and typed choice questions.');
  const response = await fetchImpl('https://openrouter.ai/api/alpha/decisions', {
    method: 'POST', signal, redirect: 'error',
    headers: { Authorization: `Bearer ${apiKey}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ model: '~typesafe/jev-latest', state, questions, provider: { zdr: true } }),
  });
  if (!response.ok) throw new Error(`Jev OpenRouter returned HTTP ${response.status}.`);
  const result = await response.json();
  for (const [id, question] of Object.entries(questions)) {
    const answer = result.answers?.[id];
    if (answer?.type !== 'choice' || !Object.hasOwn(question.criteria, answer.choice))
      throw new Error(`Unknown choice: ${id}`);
    if (!answer.probabilities || !Object.hasOwn(answer.probabilities, answer.choice) ||
        Object.entries(answer.probabilities).some(([key, p]) =>
          !Object.hasOwn(question.criteria, key) || !Number.isFinite(p) || p < 0 || p > 1))
      throw new Error(`Invalid probabilities: ${id}`);
  }
  return result;
}
