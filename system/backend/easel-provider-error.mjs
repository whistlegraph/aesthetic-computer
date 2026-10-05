// These are service-provider limits, independent of an AC braincell wallet.
// Only expose recognized reasons; provider bodies can contain private details.
export function inferenceProviderFailure(status, body) {
  let source;
  try { source = JSON.parse(body)?.error?.metadata?.limit_source; } catch {}
  if (status !== 402) return `Inference provider returned ${status}.`;
  if (source === 'openrouter_key_limit') return 'The service’s OpenRouter API key has reached its spending limit. This is not your AC braincell allowance.';
  if (source === 'openrouter_in_flight_budget') return 'OpenRouter’s credit budget is occupied by active or recently completed requests. Wait for them to settle, then retry. This is not your AC braincell allowance.';
  if (source === 'openrouter_credits') return 'OpenRouter’s available credit budget cannot cover this request. The service needs more provider credit to run it at this size. This is not your AC braincell allowance.';
  return 'OpenRouter cannot cover this request with its available credits and in-flight budget. Retry after active requests settle; if it persists, the service needs more provider credit. This is not your AC braincell allowance.';
}
