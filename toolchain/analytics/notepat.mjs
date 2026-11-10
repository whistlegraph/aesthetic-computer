// Aggregate destination cohorts only. No cross-site visit-ID joins.
export const NOTEPAT_STAGES = Object.freeze({
  ready: "notepatReady", firstNote: "actions.notepat_note_played",
  eightNotes: "actions.notepat_eight_notes", recordingStarted: "actions.notepat_recording_started",
  recordingCompleted: "actions.notepat_recording_completed", playbackRequested: "actions.notepat_playback_requested",
  abletonDownloadRequested: "actions.notepat_amxd_requested",
});
const PLAY_DEPTHS = [10, 30, 60, 180, 600];
export function notepatPipeline(start, end) {
  return [
    { $match: { property: "notepat.com", automated: false, startedAt: { $gte: start, $lt: end } } },
    { $group: {
      _id: { measured: { $ifNull: ["$notepatVersion", 0] }, playMeasured: { $ifNull: ["$notepatPlayVersion", 0] }, source: { $switch: { branches: [
        { case: { $eq: ["$campaign", "nopaint-music"] }, then: "nopaint-music" },
        { case: { $eq: ["$referrerHost", "nopaint.art"] }, then: "nopaint-referrer" },
      ], default: "other" } } },
      visits: { $sum: 1 }, firstVisit: { $min: "$startedAt" },
      playSeconds: { $sum: { $ifNull: ["$notepatPlaySeconds", 0] } },
      maxPlaySeconds: { $max: { $ifNull: ["$notepatPlaySeconds", 0] } },
      ...Object.fromEntries(PLAY_DEPTHS.map(seconds => [`played${seconds}`, {
        $sum: { $cond: [{ $gte: ["$notepatPlaySeconds", seconds] }, 1, 0] },
      }])),
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
    const timed = measured.filter(row => row._id.playMeasured === 1);
    const sum = key => timed.reduce((total, row) => total + (row[key] || 0), 0);
    const timedVisits = sum("visits"), players = sum("firstNote");
    const playTime = {
      measuredVisits: timedVisits, unmeasuredVisits: rows.filter(row => row._id.source === name).reduce((total, row) => total + row.visits, 0) - timedVisits,
      players: timedVisits ? players : null,
      totalSecondsLowerBound: timedVisits ? sum("playSeconds") : null,
      averageSecondsLowerBoundPerPlayer: players ? +(sum("playSeconds") / players).toFixed(1) : null,
      maxSecondsLowerBound: timedVisits ? Math.max(0, ...timed.map(row => row.maxPlaySeconds || 0)) : null,
      playersAtLeast: Object.fromEntries(PLAY_DEPTHS.map(seconds => [seconds, timedVisits ? sum(`played${seconds}`) : null])),
    };
    return { source: name, measuredVisits: visits,
      unmeasuredVisits: rows.filter(row => row._id.source === name && row._id.measured !== 1)
        .reduce((sum, row) => sum + row.visits, 0),
      ...stages, playTime, readyPercent: percent(stages.ready, visits), firstNotePercent: percent(stages.firstNote, visits),
      eightNotesPerFirstNotePercent: percent(stages.eightNotes, stages.firstNote),
      completedPerRecordingPercent: percent(stages.recordingCompleted, stages.recordingStarted) };
  });
  return {
    source: { shownToInteractingVisits: source.notepatAdShown || 0, clickedAfterShown: source.notepatOpenedAfterShown || 0,
      clickThroughPercent: percent(source.notepatOpenedAfterShown || 0, source.notepatAdShown || 0) },
    cohorts,
    playTimeDefinition: "Estimated visible time within five seconds of a manual note start, rounded down to five seconds and capped at one hour per visit. Chords share one clock; hidden tabs clear it and suspension gaps are capped. Long held notes are undercounted. Not audible duration or unique people. Missing coverage is unmeasured, not zero; means include only measured visits that played a note.",
    note: "Page visits, not people. Source shown counts only visits that interact after the bumper appears. Destination campaign and referrer cohorts are separate aggregates, never joined identities; shared links, blocked tracking and window boundaries can change the counts. Unmeasured visits are excluded from stage rates; null means no measured arrivals. Ready means instrument boot completed, not audio heard. Eight notes counts accepted manual note starts, including chords, not duration. Recording completed requires a nonempty phrase; playback and Ableton download are requests, not confirmed listening or file saves. Stage flags are once per document and do not assert a strict event order. Bots are excluded."
  };
}
