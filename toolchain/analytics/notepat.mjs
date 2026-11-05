// Aggregate destination cohorts only. No cross-site visit-ID joins.
export const NOTEPAT_STAGES = Object.freeze({
  ready: "notepatReady", firstNote: "actions.notepat_note_played",
  eightNotes: "actions.notepat_eight_notes", recordingStarted: "actions.notepat_recording_started",
  recordingCompleted: "actions.notepat_recording_completed", playbackRequested: "actions.notepat_playback_requested",
  abletonDownloadRequested: "actions.notepat_amxd_requested",
});
export function notepatPipeline(start, end) {
  return [
    { $match: { property: "notepat.com", automated: false, startedAt: { $gte: start, $lt: end } } },
    { $group: {
      _id: { measured: { $ifNull: ["$notepatVersion", 0] }, source: { $switch: { branches: [
        { case: { $eq: ["$campaign", "nopaint-music"] }, then: "nopaint-music" },
        { case: { $eq: ["$referrerHost", "nopaint.art"] }, then: "nopaint-referrer" },
      ], default: "other" } } },
      visits: { $sum: 1 }, firstVisit: { $min: "$startedAt" },
      ...Object.fromEntries(Object.entries(NOTEPAT_STAGES).map(([name, field]) =>
        [name, { $sum: { $cond: [`$${field}`, 1, 0] } }])),
    } },
  ];
}
const percent = (n, d) => d ? +(100 * n / d).toFixed(2) : null;
export function notepatPathway(rows = [], source = {}) {
  const cohorts = ["nopaint-music", "nopaint-referrer", "other"].map(name => {
    const measured = rows.filter(row => row._id.source === name && row._id.measured === 1);
    const visits = measured.reduce((sum, row) => sum + row.visits, 0);
    const stages = Object.fromEntries(Object.keys(NOTEPAT_STAGES).map(stage => [stage,
      visits ? measured.reduce((sum, row) => sum + (row[stage] || 0), 0) : null]));
    return { source: name, measuredVisits: visits,
      unmeasuredVisits: rows.filter(row => row._id.source === name && row._id.measured !== 1)
        .reduce((sum, row) => sum + row.visits, 0),
      ...stages, readyPercent: percent(stages.ready, visits), firstNotePercent: percent(stages.firstNote, visits),
      eightNotesPerFirstNotePercent: percent(stages.eightNotes, stages.firstNote),
      completedPerRecordingPercent: percent(stages.recordingCompleted, stages.recordingStarted) };
  });
  return {
    source: { shownToInteractingVisits: source.notepatAdShown || 0, clickedAfterShown: source.notepatOpenedAfterShown || 0,
      clickThroughPercent: percent(source.notepatOpenedAfterShown || 0, source.notepatAdShown || 0) },
    cohorts,
    note: "Page visits, not people. Source shown counts only visits that interact after the bumper appears. Destination campaign and referrer cohorts are separate aggregates, never joined identities; shared links, blocked tracking and window boundaries can change the counts. Unmeasured visits are excluded from stage rates; null means no measured arrivals. Ready means instrument boot completed, not audio heard. Eight notes counts accepted manual note starts, including chords, not duration. Recording completed requires a nonempty phrase; playback and Ableton download are requests, not confirmed listening or file saves. Stage flags are once per document and do not assert a strict event order. Bots are excluded."
  };
}
