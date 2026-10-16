const table = document.getElementById('pubs');
const body = table?.tBodies[0];
const controls = document.getElementById('controls');
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
      label.hidden = !d || select.value === 'title' || (!!row.querySelector('.created') && ['newest','oldest'].includes(select.value));
      label.textContent = `${select.value === 'updated' ? 'Updated ' : select.value === 'submission' ? 'Submission ' : ''}${d}`;
      if (d) label.dateTime = d;
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
