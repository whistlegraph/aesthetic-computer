// Read-only provider health. Never submit an image just to check availability.
// The $1 floor was returned by OpenRouter's image-output route on 2026-10-05.
// Keep it configurable independently of per-move prices.
export function createCloudStatus({key, fetch=globalThis.fetch, now=Date.now,
  minimum=1, ttl=60_000}={}) {
  let cached, until=0, pending;
  const failures = new Map();
  async function credit() {
    if (cached && until>now()) return cached;
    if (pending) return pending;
    pending=(async()=>{
      const checked_at=new Date(now()).toISOString();
      try {
        const response=await fetch('https://openrouter.ai/api/v1/credits', {
          headers:{Authorization:`Bearer ${key}`}, redirect:'error', signal:AbortSignal.timeout(5000),
        });
        if (!response.ok) throw Error('Provider status unavailable');
        const {data}=await response.json();
        if (!Number.isFinite(data?.total_credits) || !Number.isFinite(data?.total_usage)) throw Error('Invalid provider balance');
        return {checked_at, balance_usd:Math.max(0, data.total_credits-data.total_usage)};
      } catch { return {checked_at, unknown:true}; }
    })().then(value=>{cached=value;until=now()+ttl;return value;}).finally(()=>{pending=null;});
    return pending;
  }
  const check = async ({enabled=false, offers=[]}={})=>{
    const funding=key ? await credit() : null;
    const result=(code, detail)=>({available:code==='ready', code,
      message:code==='ready'?'AC cloud available':'AC cloud unavailable', detail,
      ...(funding?.checked_at ? {checked_at:funding.checked_at} : {})});
    if (funding && !funding.unknown && funding.balance_usd<minimum && (!offers.length || offers.some(offer=>offer.provider==='openrouter'))) {
      const provider_status={...result('provider_funding', "AC needs to fund its OpenRouter account. Your Braincells remain available. Local models still work."),
        provider:'OpenRouter', balance_usd:funding.balance_usd, minimum_balance_usd:minimum};
      if (enabled && offers.some(offer=>offer.provider!=='openrouter')) return {
        ...result('ready', null), provider_status,
        unavailable_models:offers.filter(offer=>offer.provider==='openrouter').map(offer=>offer.id),
      };
      return provider_status;
    }
    if (!offers.length) return result('not_configured', 'AC has not enabled and priced these image models yet. Your Braincells remain available.');
    if (!enabled) return result('disabled', 'AC has paused cloud image generation. Your Braincells remain available.');
    if (funding?.unknown && offers.every(offer=>offer.provider==='openrouter'))
      return result('status_unavailable', 'AC could not check OpenRouter availability. Refresh to try again.');
    return result('ready', null);
  };
  const status = async (config={}) => {
    const service = await check(config);
    if (!service.available) return service;
    for (const [id, failure] of failures) if (failure.until <= now()) failures.delete(id);
    const issues = Object.fromEntries((config.offers || []).filter(offer=>failures.has(offer.id))
      .map(offer=>[offer.id, failures.get(offer.id)]));
    const unavailable_models = [...new Set([...(service.unavailable_models || []), ...Object.keys(issues)])];
    if (!unavailable_models.length) return service;
    const allUnavailable = config.offers?.length && config.offers.every(offer=>unavailable_models.includes(offer.id));
    const first = Object.values(issues)[0];
    return {...service, unavailable_models, model_issues:issues,
      ...(allUnavailable && first ? {available:false, code:first.code, detail:first.message} : {})};
  };
  status.reportFailure = (offer, error) => {
    if (!error.code?.startsWith('provider_')) return;
    failures.set(offer.id, {code:error.code, message:error.message, until:now()+(error.retryAfter || 60)*1000});
  };
  return status;
}
