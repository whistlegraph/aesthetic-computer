import { RECOGNITION_COLLECTION } from "../../system/public/aesthetic.computer/lib/network-recognition-model.mjs";
import { RETENTION_DAYS } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";

export function recognitionReportOptions({ hours = 24 } = {}, now = new Date()) {
  if (!Number.isFinite(hours) || hours <= 0 || hours > RETENTION_DAYS * 24) throw new Error("hours must be > 0 and <= 840");
  return { start: new Date(+now - hours * 3600000), end: now };
}

export async function networkRecognitionReport(db, options = {}) {
  const { start, end } = recognitionReportOptions(options);
  const rows = await db.collection(RECOGNITION_COLLECTION).aggregate([
    { $match: { property: "sotce.net", startedAt: { $gte: start, $lt: end } } },
    { $group: {
      _id: { $dateToString: { format: "%Y-%m-%d", date: "$startedAt", timezone: "+00:00" } },
      visitsChecked: { $sum: 1 },
      acSessionVerified: { $sum: { $cond: [{ $eq: ["$acRecognized", true] }, 1, 0] } },
      bridgeCompleted: { $sum: { $cond: [{ $eq: ["$bridgeCompleted", true] }, 1, 0] } },
    } },
    { $sort: { _id: 1 } },
  ], { maxTimeMS: 10000 }).toArray();
  const totals = rows.reduce((all, row) => {
    for (const key of ["visitsChecked", "acSessionVerified", "bridgeCompleted"]) all[key] += row[key];
    return all;
  }, { visitsChecked: 0, acSessionVerified: 0, bridgeCompleted: 0 });
  const counts = row => ({ ...row, unknown: row.visitsChecked - row.acSessionVerified,
    bridgeIncomplete: row.visitsChecked - row.bridgeCompleted });
  return { start, end, property: "sotce.net", unit: "eligible page visits, not unique people or accounts",
    note: "An existing AC credential was verified for the positive count. Unknown includes absent/expired sessions, partitioned or blocked storage, opt-out during a check, and network failures; it never means no AC account. No account IDs, handles, fingerprints or fishery joins are retained. These checks are a separate instrument from network-visits and must not be added to visit totals.",
    totals: counts(totals), daily: rows.map(({ _id, ...row }) => ({ day: _id, ...counts(row) })) };
}
