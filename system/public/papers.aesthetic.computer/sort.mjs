const table = document.getElementById('pubs');
const body = table?.tBodies[0];
let controls = document.getElementById('controls');
if (body && !controls) {
  controls = document.createElement('div');
  controls.id = 'controls';
  controls.className = 'controls';
  const search = document.createElement('input');
  search.type = 'search'; search.placeholder = 'Filter papers…';
  search.setAttribute('aria-label', 'Filter papers');
  search.addEventListener('input', () => {
    const term = search.value.trim().toLowerCase();
    for (const row of body.rows) row.hidden = !row.textContent.toLowerCase().includes(term);
  });
  controls.append(search);
  table.before(controls);
}
if (body && controls) {
  const select = document.createElement('select');
  select.id = 'paper-sort';
  select.setAttribute('aria-label', 'Sort papers');
  const options = [['newest', 'Newest'], ['updated', 'Recently updated'], ['oldest', 'Oldest'], ['submission', 'Submission date'], ['title', 'Title A–Z']];
  for (const [value, text] of options) select.add(new Option(text, value));
  controls.append(select);
  try { select.value = localStorage.getItem('papers-sort') || 'newest'; } catch {}
  if (!select.value) select.value = 'newest';
  const rows = [...body.rows];
  const created = row => row.dataset.created?.slice(0, 10) || '';
  const title = row => (row.querySelector('.paper-title, .ti > a')?.textContent || row.textContent).trim();
  const date = row => select.value === 'updated' ? row.dataset.updated?.slice(0, 10) || '' : select.value === 'submission' ? row.querySelector('.wh')?.textContent.match(/\d{4}-\d{2}-\d{2}/)?.[0] || '' : created(row);
  const labels = new Map();
  const versions = new Map();
  for (const row of rows) {
    const count = Number(row.dataset.revisions);
    if (!Number.isInteger(count) || count < 1) continue;
    const version = document.createElement('span');
    version.className = 'paper-version';
    version.textContent = `v${count}`;
    version.title = `${count} recorded ${count === 1 ? 'revision' : 'revisions'}`;
    version.setAttribute('aria-label', version.title);
    versions.set(row, version);
  }
  for (const row of rows) {
    const parent = row.querySelector('.ti') || row.querySelector('.paper-line');
    if (!parent) continue;
    const label = document.createElement('time');
    label.className = 'paper-date';
    parent.append(label);
    labels.set(row, label);
  }
  function apply() {
    rows.sort((a, b) => {
      if (select.value === 'title') return title(a).localeCompare(title(b));
      const x = date(a), y = date(b);
      if (!x || !y) return x ? -1 : y ? 1 : title(a).localeCompare(title(b));
      return (select.value === 'oldest' ? x.localeCompare(y) : y.localeCompare(x)) || title(a).localeCompare(title(b));
    });
    for (const row of rows) {
      body.append(row);
      const label = labels.get(row);
      if (!label) continue;
      const d = date(row);
      // The compact layout already displays creation dates.
      const existing = row.querySelector('.created');
      const useExisting = !!existing && ['newest', 'oldest', 'title'].includes(select.value);
      if (existing) existing.hidden = !useExisting;
      label.hidden = useExisting || (!d && !versions.has(row));
      label.textContent = `${select.value === 'updated' ? 'Updated ' : select.value === 'submission' ? 'Submission ' : ''}${d}`;
      if (d) label.dateTime = d;
      if (versions.has(row)) (useExisting ? existing : label).append(versions.get(row));
    }
    for (const head of table.querySelectorAll('th[data-dir]')) delete head.dataset.dir;
    try { localStorage.setItem('papers-sort', select.value); } catch {}
  }
  select.addEventListener('change', apply);
  table.tHead?.addEventListener('click', event => {
    if (event.target.closest('th[data-col]')) select.selectedIndex = -1;
  });
  apply();
}
