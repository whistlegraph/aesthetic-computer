const $ = (id) => document.getElementById(id);
const api = 'https://aesthetic.computer/api';
const params = new URLSearchParams(location.search);
const prefillEmail = params.get('email');
if (prefillEmail) {
  const cleanUrl = new URL(location.href);
  cleanUrl.searchParams.delete('email');
  history.replaceState({}, '', cleanUrl);
}
// Caddy's language aliases rewrite internally; their query is not visible here.
const routeLanguage = { da: 'da', de: 'de', es: 'es', cn: 'zh' }[location.pathname.split('/').filter(Boolean).at(-1)];
const words = {
  en: { purpose: 'Keeping Aesthetic Computer online and free.', frequency: 'Give', monthly: 'Monthly', once: 'Once', amount: 'Amount', tax: 'Not tax deductible.', bills: 'See the bills', manage: 'Manage subscription', other: 'Other ways to give', subscriber: 'monthly subscriber', subscribers: 'monthly subscribers', give: 'Give', month: 'month', terms: 'Renews monthly. Cancel anytime.', loading: 'Opening checkout…', error: 'Could not open checkout. Please try again.', unavailable: 'Subscriber count unavailable', thanks: 'Thank you for giving!', portalError: 'Could not open your subscription. Log in with your checkout email, or contact mail@aesthetic.computer.', portalLoading: 'Opening subscription…' },
  da: { purpose: 'Holder Aesthetic Computer online og gratis.', frequency: 'Giv', monthly: 'Månedligt', once: 'Én gang', amount: 'Beløb', tax: 'Ikke fradragsberettiget.', bills: 'Se regningerne', manage: 'Administrer abonnement', other: 'Andre måder at give', subscriber: 'månedlig abonnent', subscribers: 'månedlige abonnenter', give: 'Giv', month: 'måned', terms: 'Fornyes månedligt. Opsig når som helst.', loading: 'Åbner betaling…', error: 'Kunne ikke åbne betaling. Prøv igen.', unavailable: 'Antal abonnenter utilgængeligt', thanks: 'Tak for dit bidrag!', portalError: 'Kunne ikke åbne dit abonnement. Log ind med din betalingsmail, eller kontakt mail@aesthetic.computer.', portalLoading: 'Åbner abonnement…' },
  de: { purpose: 'Hält Aesthetic Computer online und kostenlos.', frequency: 'Geben', monthly: 'Monatlich', once: 'Einmalig', amount: 'Betrag', tax: 'Nicht steuerlich absetzbar.', bills: 'Kosten ansehen', manage: 'Abo verwalten', other: 'Weitere Zahlungsarten', subscriber: 'monatlicher Unterstützer', subscribers: 'monatliche Unterstützer', give: 'Geben', month: 'Monat', terms: 'Monatliche Verlängerung. Jederzeit kündbar.', loading: 'Zahlung wird geöffnet…', error: 'Zahlung konnte nicht geöffnet werden. Bitte erneut versuchen.', unavailable: 'Anzahl nicht verfügbar', thanks: 'Vielen Dank für deinen Beitrag!', portalError: 'Abo konnte nicht geöffnet werden. Mit der Zahlungs-E-Mail anmelden oder mail@aesthetic.computer kontaktieren.', portalLoading: 'Abo wird geöffnet…' },
  es: { purpose: 'Manteniendo Aesthetic Computer en línea y gratuito.', frequency: 'Dar', monthly: 'Mensual', once: 'Una vez', amount: 'Cantidad', tax: 'No deducible de impuestos.', bills: 'Ver los gastos', manage: 'Gestionar suscripción', other: 'Otras formas de dar', subscriber: 'suscriptor mensual', subscribers: 'suscriptores mensuales', give: 'Dar', month: 'mes', terms: 'Se renueva cada mes. Cancela cuando quieras.', loading: 'Abriendo pago…', error: 'No se pudo abrir el pago. Inténtalo de nuevo.', unavailable: 'Recuento no disponible', thanks: '¡Gracias por tu aportación!', portalError: 'No se pudo abrir tu suscripción. Inicia sesión con tu correo de pago o contacta con mail@aesthetic.computer.', portalLoading: 'Abriendo suscripción…' },
  zh: { purpose: '让 Aesthetic Computer 保持在线和免费。', frequency: '捐赠', monthly: '每月', once: '一次', amount: '金额', tax: '不可抵税。', bills: '查看账单', manage: '管理订阅', other: '其他捐赠方式', subscriber: '位每月订阅者', subscribers: '位每月订阅者', give: '捐赠', month: '月', terms: '每月自动续订，可随时取消。', loading: '正在打开付款页面…', error: '无法打开付款页面，请重试。', unavailable: '订阅人数暂不可用', thanks: '感谢您的捐赠！', portalError: '无法打开订阅。请使用付款邮箱登录，或联系 mail@aesthetic.computer。', portalLoading: '正在打开订阅…' },
};
let lang = params.get('lang') || routeLanguage || navigator.language.split('-')[0];
if (!Object.hasOwn(words, lang)) lang = 'en';
$('language').value = lang;
let count = null;
let countFailed = false;
let busy = false;
const monthly = () => $('form').elements.frequency.value === 'monthly';
const format = (amount) => new Intl.NumberFormat(lang, { style: 'currency', currency: $('currency').value.toUpperCase(), currencyDisplay: 'narrowSymbol', minimumFractionDigits: 0, maximumFractionDigits: 2 }).format(amount);

function render() {
  const t = words[lang];
  document.documentElement.lang = lang;
  document.querySelectorAll('[data-text]').forEach((el) => { el.textContent = t[el.dataset.text]; });
  $('subscriber-label').textContent = countFailed ? t.unavailable : count === 1 ? t.subscriber : t.subscribers;
  $('count').textContent = count === null ? '—' : count.toLocaleString(lang);
  $('count').setAttribute('aria-label', count === null ? t.unavailable : `${count} ${count === 1 ? t.subscriber : t.subscribers}`);
  $('give').textContent = busy ? t.loading : `${t.give} ${format(Number($('amount').value) || 0)}${monthly() ? ` / ${t.month}` : ''}`;
  $('terms').textContent = monthly() ? t.terms : '';
  $('thanks').textContent = t.thanks;
  document.querySelectorAll('[data-amount]').forEach((button) => { button.textContent = format(Number(button.dataset.amount)); });
}

$('language').addEventListener('change', () => { lang = $('language').value; render(); });
$('form').addEventListener('input', render);
function updateCurrency() {
  const dkk = $('currency').value === 'dkk';
  $('amount').min = dkk ? '5' : '1';
  $('amount').max = dkk ? '17500' : '2500';
  $('amount').value = dkk ? '50' : '8';
  document.querySelectorAll('[data-amount]').forEach((button, i) => { button.dataset.amount = (dkk ? [25, 50, 100, 200] : [4, 8, 16, 32])[i]; });
  render();
}
$('currency').addEventListener('change', updateCurrency);
const initialCurrency = params.get('currency') || (lang === 'da' ? 'dkk' : 'usd');
$('currency').value = initialCurrency === 'dkk' ? 'dkk' : 'usd';
updateCurrency();
$('presets').addEventListener('click', (event) => {
  const button = event.target.closest('[data-amount]');
  if (button) { $('amount').value = button.dataset.amount; render(); }
});
$('thanks').hidden = params.get('thanks') !== '1';
render();

fetch(`${api}/gives?summary=subscribers`, { signal: AbortSignal.timeout(8000) })
  .then(async (res) => {
    if (!res.ok) throw new Error('Count unavailable');
    const data = await res.json();
    if (!Number.isSafeInteger(data.activeSubscribers) || data.activeSubscribers < 0) throw new Error('Invalid count');
    count = data.activeSubscribers;
  })
  .catch(() => { countFailed = true; })
  .finally(render);

$('form').addEventListener('submit', async (event) => {
  event.preventDefault();
  if (busy || !$('form').reportValidity()) return;
  busy = true;
  $('give').disabled = true;
  $('error').textContent = '';
  render();
  try {
    const payload = { amount: Math.round(Number($('amount').value) * 100), currency: $('currency').value, recurring: monthly() };
    if (prefillEmail) payload.email = prefillEmail;
    const res = await fetch(`${api}/give`, {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload), signal: AbortSignal.timeout(20000),
    });
    const data = await res.json();
    if (!res.ok || !data.url) throw new Error('Checkout unavailable');
    const url = new URL(data.url);
    if (url.protocol !== 'https:' || url.hostname !== 'checkout.stripe.com') throw new Error('Invalid checkout URL');
    location.assign(url.href);
  } catch {
    $('error').textContent = words[lang].error;
    busy = false;
    $('give').disabled = false;
    render();
  }
});

// Account tools are only loaded when managing an existing subscription.
let authPromise;
function getAuth() {
  if (!authPromise) authPromise = (async () => {
    await new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = 'https://cdn.auth0.com/js/auth0-spa-js/2.0/auth0-spa-js.production.js';
      script.onload = resolve;
      script.onerror = () => { script.remove(); reject(new Error('Could not load login')); };
      document.head.append(script);
    });
    return window.auth0.createAuth0Client({
      domain: 'aesthetic.us.auth0.com', clientId: 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt',
      authorizationParams: { redirect_uri: location.origin + location.pathname, audience: 'https://aesthetic.us.auth0.com/api/v2/' },
      cacheLocation: 'localstorage', useRefreshTokens: true, useRefreshTokensFallback: true,
    });
  })().catch((error) => { authPromise = null; throw error; });
  return authPromise;
}
async function manage(callback = false) {
  $('manage').disabled = true;
  $('error').textContent = '';
  $('status').textContent = words[lang].portalLoading;
  try {
    const auth = await getAuth();
    if (callback) {
      await auth.handleRedirectCallback();
      history.replaceState({}, '', location.pathname);
    }
    if (!await auth.isAuthenticated()) {
      await auth.loginWithRedirect();
      return;
    }
    const token = await auth.getTokenSilently();
    const res = await fetch(`${api}/give-portal`, {
      method: 'POST', headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
      body: '{}', signal: AbortSignal.timeout(20000),
    });
    const data = await res.json();
    if (!res.ok || !data.url) throw new Error('Portal unavailable');
    const url = new URL(data.url);
    if (url.protocol !== 'https:' || url.hostname !== 'billing.stripe.com') throw new Error('Invalid portal URL');
    location.assign(url.href);
  } catch {
    $('error').textContent = words[lang].portalError;
  } finally {
    $('manage').disabled = false;
    $('status').textContent = '';
  }
}
$('manage').addEventListener('click', () => manage());
if (params.has('state') && (params.has('code') || params.has('error'))) manage(true);
