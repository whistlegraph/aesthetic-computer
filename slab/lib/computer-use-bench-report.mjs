// Keep receipts and event traces in the report file, not routine tool output.
function timings(values) {
  const sorted = values.filter(Number.isFinite).sort((a, b) => a - b);
  const n = sorted.length;
  if (!n) return { samples: 0 };
  return { samples: n,
    medianMs: +((sorted[Math.floor((n - 1) / 2)] + sorted[Math.floor(n / 2)]) / 2).toFixed(2),
    p95Ms: sorted[Math.ceil(n * 0.95) - 1], maxMs: sorted.at(-1),
    under100ms: sorted.filter(ms => ms < 100).length };
}

function groups(samples, key, verified) {
  const grouped = new Map();
  for (const sample of samples || []) {
    const name = key(sample);
    if (!grouped.has(name)) grouped.set(name, []);
    grouped.get(name).push(sample);
  }
  return Object.fromEntries([...grouped].map(([name, rows]) => [name, {
    attempted: rows.length,
    verified: rows.filter(verified).length,
    // Failed operations have a timeout cost, not a successful action latency.
    successful: timings(rows.filter(verified).map(row => row.ms)),
    failed: timings(rows.filter(row => !verified(row)).map(row => row.ms)),
  }]));
}

export function summarizeBenchmark(report, reportPath) {
  const { rounds, ...game } = report.game || {};
  return {
    at: report.at, ok: report.ok, reportPath,
    ...(report.error ? { error: report.error.slice(0, 600) } : {}),
    scope: 'Tool action and result checks; excludes model inference and fixture startup.',
    installedFrame: report.installedFrame,
    ...(report.game ? { game } : {}),
    ...(report.nativeClickLatency ? { clicks: groups(report.nativeClickLatency,
      row => `hold=${row.holdMs ?? row.nativeInput?.holdMs ?? 'default'},settle=${row.settleMs}`,
      row => Number.isInteger(row.verifiedCount) && row.nativeInput?.verification?.ok !== false) } : {}),
    ...(report.dragSamples ? { dragBatches: report.dragBatches ?? 1,
      drags: groups(report.dragSamples,
        row => `hold=${row.holdMs},path=${row.durationMs},release=${row.releaseMs}`,
        row => row.verified !== false && row.receipt?.verification?.ok === true && row.receipt.releasePosted === true &&
          /^Drops: [1-9]\d*$/.test(row.drops) && row.released === row.drops.replace('Drops:', 'Released:') && row.rejected === 'Rejected: 0') } : {}),
    ...(report.clickLatency ? { browserClicks: (({timesMs, ...summary}) => summary)(report.clickLatency) } : {}),
    checks: { nativeGuard: report.nativeGuard, compactRecovery: report.compactRecovery,
      multiClick: report.multiClickChecks },
    screenshot: report.screenshot,
  };
}
