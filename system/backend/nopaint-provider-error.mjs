// Public errors contain no upstream payloads, keys or provider account details.
export function providerError(response, name = "This model") {
  const limited = response.status === 429;
  const retryAfter = Math.min(
    300,
    Math.max(
      30,
      Number(response.headers?.get("retry-after")) || (limited ? 300 : 60),
    ),
  );
  return Object.assign(
    Error(
      limited
        ? `${name} is temporarily rate-limited by its image provider. Choose another model or try again later.`
        : `${name}'s image provider could not complete this move. Choose another model or try again later.`,
    ),
    {
      status: 502,
      code: limited ? "provider_rate_limited" : "provider_unavailable",
      providerStatus: response.status,
      retryAfter,
    },
  );
}
