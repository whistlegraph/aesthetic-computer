// Jev via the AI Gateway evaluation protocol. Node 22+, no dependencies.
// Protocol source: vercel/ai packages/gateway/src/gateway-evaluation-model.ts
import { pathToFileURL } from 'node:url';

export async function evaluate({ state, questions }, {
  apiKey = process.env.AI_GATEWAY_API_KEY,
  fetchImpl = globalThis.fetch,
  signal = AbortSignal.timeout(10_000),
} = {}) {
  if (!apiKey) throw new Error('Set AI_GATEWAY_API_KEY to a Vercel AI Gateway key.');
  if (!questions || !Object.keys(questions).length || state == null)
    throw new Error('Provide state and at least one typed question.');
  const response = await fetchImpl('https://ai-gateway.vercel.sh/v4/ai/evaluation-model', {
    method: 'POST', signal, redirect: 'error',
    headers: {
      Authorization: `Bearer ${apiKey}`,
      'Content-Type': 'application/json',
      'ai-gateway-protocol-version': '0.0.1',
      'ai-gateway-auth-method': 'api-key',
      'ai-evaluation-model-specification-version': '4',
      'ai-model-id': 'typesafe-ai/jev',
    },
    body: JSON.stringify({ state, questions,
      providerOptions: { gateway: { zeroDataRetention: true } } }),
  });
  // Do not echo upstream error bodies, which may contain submitted state.
  if (!response.ok) throw new Error(`Jev gateway returned HTTP ${response.status}.`);
  const result = await response.json();
  return validateAnswers(result, questions);
}

export function validateAnswers(result, questions) {
  for (const [id, question] of Object.entries(questions)) {
    const answer = result.answers?.[id];
    if (!answer || answer.type !== question.type)
      throw new Error(`Missing or invalid answer: ${id}`);
    if (question.type === 'choice' &&
        !Object.hasOwn(question.criteria, answer.choice))
      throw new Error(`Unknown choice: ${id}`);
    if (question.type === 'boolean' && !probability(answer.probability))
      throw new Error(`Invalid probability: ${id}`);
    if (question.type === 'score' && (!Number.isFinite(answer.score) ||
        answer.score < 0 || answer.score > question.criteria.length - 1))
      throw new Error(`Invalid score: ${id}`);
    if (answer.probabilities && Object.values(answer.probabilities).some(p => !probability(p)))
      throw new Error(`Invalid probabilities: ${id}`);
  }
  return result;
}

const probability = p => Number.isFinite(p) && p >= 0 && p <= 1;

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    let input = '';
    for await (const chunk of process.stdin) {
      input += chunk;
      if (input.length > 256_000) throw new Error('Input exceeds 256 KB.');
    }
    console.log(JSON.stringify(await evaluate(JSON.parse(input)), null, 2));
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
