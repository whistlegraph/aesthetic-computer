# COMPOSE ONE HARNESS FROM THE EVIDENCE

Two candidates were proposed against the base harness and each was run on the
same request. `context/pool/<label>/harness/` is each candidate's harness and
`context/pool/<label>/rollout/trace.md` its complete run;
`context/rollouts/base/trace.md` is the base run. `context/pool/<label>/notes.md` is what each editor said it
changed and why. Deliverables are under `context/deliverables/<label>/`.

Diff each candidate against `context/harness/` (the base) and compare the runs
where they diverge. An edit earns a place when its run behaved better at the
point it targeted; an edit whose run shows no difference there has not earned
one, however sensible it reads. Edits from different candidates that touch
different behaviour can both stay. Where two candidates changed the same
section differently, keep the form whose run did better, or neither.

Write the composed harness into `context/harness/`, editing in place. KEEP_BASE
is a real answer: if neither candidate's run was better where it aimed, leave
the base unchanged.

Reply with two or three sentences: which edits you kept, which you dropped,
and the run evidence for each.
