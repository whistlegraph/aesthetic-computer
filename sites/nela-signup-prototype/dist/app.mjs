import {steps, fresh, validate, firstError, issueSignal, confirmSignal, setEmail, emailNeeded, summary} from './flow.mjs';
let state = fresh(new URL(location.href).searchParams.get('from') === 'signal' ? 'signal' : 'web');
let error = '';
const $ = id => document.getElementById(id);
const escape = value => String(value).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const checked = value => value ? ' checked' : '';
const note = text => { $('announcement').textContent = text; };
const field = (id, label, value, help = '', attrs = '') => `<div class="field"><label for="${id}">${label}</label><input id="${id}" value="${escape(value)}" ${attrs}${help ? ` aria-describedby="${id}-help"` : ''}>${help ? `<p class="help" id="${id}-help">${help}</p>` : ''}</div>`;
const nav = (label = 'Continue') => `<p class="error" id="form-error" role="alert">${escape(error)}</p><div class="actions">${state.step > 0 ? '<button type="button" data-back class="text-button">Back</button>' : '<span></span>'}<button type="submit" class="primary">${label}</button></div>`;
const heading = (title, intro = '') => `<h1>${title}</h1>${intro ? `<p class="intro">${intro}</p>` : ''}`;
function signalPanel() {
  if (state.signalStatus === 'verified') return `<div class="signal-panel"><strong class="verified">Signal linked in this demo.</strong><p>${escape(state.signalName)}</p><button type="button" id="unlink-signal" class="text-button">Unlink</button></div>`;
  if (state.signalStatus === 'pending') return `<div class="signal-panel"><p>Confirm this code in Signal.</p><div class="challenge">${escape(state.signalCode.slice(0,3))} ${escape(state.signalCode.slice(3))}</div><p class="help">Request for ${escape(state.signalName)} · <span id="signal-timer"></span></p><div class="inline-actions"><button type="button" id="confirm-signal" class="primary">Simulate confirmation</button><button type="button" id="decline-signal">Simulate decline</button></div><button type="button" id="expire-signal" class="text-button">Try an expired request</button></div>`;
  return `${state.signalStatus === 'expired' ? '<p class="error">That request expired. You can try again.</p>' : state.signalStatus === 'declined' ? '<p class="error">Request declined. No identity was linked.</p>' : ''}${field('signal-name','Signal username',state.signalName,'Use a fictional username, such as river.42.','autocomplete="off" maxlength="64" placeholder="river.42"')}<button type="button" id="request-signal">Preview Signal request</button>`;
}
function contactPanel() {
  const required = emailNeeded(state);
  return `${heading('How can the club reach you?', required ? 'Use email for account access and recovery. Announcements are a separate choice.' : 'Signal is linked. You can also add a recovery email.')}<fieldset><legend>Event announcements</legend><label class="check-row"><input id="email-events" type="checkbox"${checked(state.emailEvents)}><span>Email me<span class="subtext">Upcoming club events.</span></span></label><label class="check-row"><input id="signal-events" type="checkbox"${checked(state.signalEvents)}${state.signalStatus !== 'verified' ? ' disabled' : ''}><span>Notify me on Signal<span class="subtext">${state.signalStatus === 'verified' ? 'Direct notifications; no automatic group join.' : 'Link Signal to choose this channel.'}</span></span></label></fieldset>${state.signalStatus !== 'verified' ? '<button type="button" data-step="1" class="text-button">Link Signal</button>' : ''}${field('email',`Email${required ? '' : ' (optional)'}`,state.email,'Use a fictional address for this prototype.','type="email" autocomplete="off" placeholder="river@example.com" maxlength="254"')}${state.emailVerified ? '<p class="verified">Email verified in this demo.</p>' : state.emailRequestedFor ? `<div class="field"><label for="email-code">Verification code</label><div class="email-code"><input id="email-code" inputmode="numeric" maxlength="6" autocomplete="off" placeholder="6 digits"><button type="button" id="verify-email">Verify demo code</button></div><p class="help">Demo code: 246810. No email is sent.</p></div>` : '<button type="button" id="request-email">Preview email verification</button>'}${nav()}`;
}
function memberPanel() {
  return `${heading('Support the club?', 'Membership support is optional. Choose what works for you.')}<label class="option"><input type="radio" name="support" value="later"${checked(state.support==='later')}><span>Not now<span class="subtext">Continue without a payment.</span></span></label><label class="option"><input type="radio" name="support" value="monthly"${checked(state.support==='monthly')}><span>Monthly support<span class="subtext">Example amounts for discussion.</span></span></label>${state.support === 'monthly' ? `<fieldset><legend>Amount per month · USD</legend><div class="amounts">${[10,20,35].map(n=>`<button type="button" data-amount="${n}" aria-pressed="${state.amount===n}">$${n}</button>`).join('')}</div>${field('amount','Another amount',state.amount,'Whole dollars, $1–$500 in this demo.','type="number" min="1" max="500" step="1"')}</fieldset>${state.payment === 'success' ? `<p class="verified">Demo checkout completed · $${state.amount}/month</p>` : `<button type="button" id="checkout" class="primary">Preview Stripe checkout</button>${state.payment==='failed' ? '<p class="error">The simulated payment failed. Retry or continue without payment.</p>' : state.payment==='cancelled' ? '<p class="help">Checkout cancelled. You can try again or choose “Not now”.</p>' : ''}`}` : ''}${nav()}`;
}
function reviewPanel() {
  const notifications = [state.emailEvents ? 'Email' : '',state.signalEvents ? 'Signal' : ''].filter(Boolean).join(' + ') || 'No event announcements';
  const rows = [ ['Identity', `${state.name}<br><span class="subtext">${escape(state.handle)}.nela.computer</span>`,0,true], ['Signal',state.signalStatus==='verified' ? `${state.signalName} · linked` : 'Not linked',1], ['Community agreement',state.agreed ? 'Berlin Code of Conduct accepted' : 'Not accepted',2], ['Contact',`${state.email || 'Signal only'}<br><span class="subtext">${notifications}</span>`,3,true], ['Membership',state.support==='monthly' ? `$${state.amount}/month · simulated` : 'No payment',4] ];
  // User-entered content is escaped before the small, fixed formatting fragments are inserted.
  rows[0][1] = `${escape(state.name)}<br><span class="subtext">${escape(state.handle)}.nela.computer</span>`;
  rows[3][1] = `${escape(state.email || 'Signal only')}<br><span class="subtext">${notifications}</span>`;
  return `${heading('Everything look right?')}<dl class="summary">${rows.map(([title,value,step,html])=>`<div class="summary-row"><div><dt>${title}</dt><dd>${html ? value : escape(value)}</dd></div><button type="button" class="text-button" data-step="${step}" aria-label="Edit ${title.toLowerCase()}">Edit</button></div>`).join('')}</dl>${nav('Finish prototype')}`;
}
function render(focus = false) {
  $('entry').value = state.origin;
  $('steps').innerHTML = steps.map((title,i)=>`<li><button type="button" data-step="${i}"${i===state.step && !state.complete ? ' aria-current="step"' : ''}${i>state.reached ? ' disabled' : ''} class="${i<state.step || state.complete ? 'complete' : ''}" aria-label="Step ${i+1}: ${title}"><span class="number">${i<state.step || state.complete ? '✓' : String(i+1).padStart(2,'0')}</span><span class="step-name">${title}</span></button></li>`).join('');
  $('identity-label').textContent = `${state.handle || 'your-name'}.nela.computer`;
  if (state.complete) {
    $('main').innerHTML = `<div class="result-mark" aria-hidden="true">✓</div>${heading('You’ve reached the end.','This is where the real flow would create your account and open the club’s tools.')}<p class="success-address">${escape(state.handle)}.nela.computer</p><p class="help">No account, domain or subscription was created.</p><div class="actions"><button type="button" id="review-again" class="text-button">Review choices</button><button type="button" id="done-reset" class="primary">Try another path</button></div>`;
  } else {
    let content;
    if(state.step===0) content = `${heading('Choose your name.', 'A name for people. A handle for the club’s tools.')}${state.origin==='signal' ? '<p class="help">Sample profile from a Signal invite. Ownership is confirmed in the next step.</p>' : ''}${field('name','Display name',state.name,'','autocomplete="off" maxlength="64" placeholder="River"')}${field('handle','Club handle',state.handle,'3–24 lowercase letters, numbers or hyphens.','autocomplete="off" autocapitalize="none" spellcheck="false" maxlength="24" placeholder="river"')}<div class="address" id="address-preview">${escape(state.handle || 'your-name')}.nela.computer</div><p class="help">Illustrative address. Availability is not checked or reserved.</p>${nav()}`;
    if(state.step===1) content = `${heading('Link your Signal.', 'Confirm that the Signal identity belongs to you. You can also skip this and use email.')}${signalPanel()}${nav(state.signalStatus === 'verified' ? 'Continue' : 'Use email instead')}`;
    if(state.step===2) content = `${heading('A shared agreement.')}<div class="policy"><p>NELA uses the Berlin Code of Conduct as a basis for how people treat one another.</p><ul><li>Participate with care and respect.</li><li>Work through disagreements collaboratively.</li><li>Make room for people with different backgrounds and experience.</li><li>Keep the community free from harassment and discrimination.</li></ul><a href="https://berlincodeofconduct.org/en" target="_blank" rel="noopener">Read the full code of conduct</a><p class="help">This summary does not replace the full policy.</p></div><label class="check-row"><input type="checkbox" id="agreed"${checked(state.agreed)}><span>I’ve read and agree to the Berlin Code of Conduct.</span></label>${nav()}`;
    if(state.step===3) content = contactPanel();
    if(state.step===4) content = memberPanel();
    if(state.step===5) content = reviewPanel();
    $('main').innerHTML = `<form id="signup-form" novalidate>${content}</form>`;
  }
  bind(); updateTimer();
  if(focus) $('main').focus();
}
function move(step) { if(!Number.isInteger(step)||step<0||step>5||step>state.reached) throw new Error('Complete the preceding steps first.'); state.step=step;state.complete=false;error='';render(true); }
function advance() {
  error = validate(state,state.step);
  if(error){render();$('form-error')?.scrollIntoView({block:'nearest'});return false;}
  if(state.step===5){const invalid=firstError(state);if(invalid){state.step=invalid.step;error=invalid.error;render(true);return false;}state.complete=true;}else{state.step++;state.reached=Math.max(state.reached,state.step);}
  render(true);return true;
}
function restart(origin = state.origin) { state=fresh(origin);error='';render(true);note('Prototype restarted.'); }
function issue() { try { const random=new Uint32Array(1);crypto.getRandomValues(random);issueSignal(state,String(100000+random[0]%900000));error='';render(); } catch(e){error=e.message;render();} }
function signalResult(result) { try{if(result==='verified') confirmSignal(state,state.signalCode);else{state.signalStatus=result;state.signalCode='';state.signalExpires=0;state.signalEvents=false;}error='';}catch(e){error=e.message;}render(); }
function updateTimer(){if(state.signalStatus!=='pending')return;const seconds=Math.max(0,Math.ceil((state.signalExpires-Date.now())/1000));if(seconds===0){state.signalStatus='expired';state.signalCode='';if(state.step===1)render();return;}$('signal-timer') && ($('signal-timer').textContent=`expires in ${seconds}s`);}
function showError(text){error=text;render();}
function bind() {
  document.querySelectorAll('[data-step]').forEach(button=>button.onclick=()=>move(Number(button.dataset.step)));
  document.querySelector('[data-back]')?.addEventListener('click',()=>move(state.step-1));
  $('signup-form')?.addEventListener('submit',event=>{event.preventDefault();advance();});
  $('name')?.addEventListener('input',event=>{state.name=event.target.value;});
  $('handle')?.addEventListener('input',event=>{state.handle=event.target.value;$('address-preview').textContent=`${state.handle||'your-name'}.nela.computer`;$('identity-label').textContent=`${state.handle||'your-name'}.nela.computer`;});
  $('signal-name')?.addEventListener('input',event=>{state.signalName=event.target.value;});
  $('request-signal')?.addEventListener('click',issue);
  $('confirm-signal')?.addEventListener('click',()=>signalResult('verified'));
  $('decline-signal')?.addEventListener('click',()=>signalResult('declined'));
  $('expire-signal')?.addEventListener('click',()=>signalResult('expired'));
  $('unlink-signal')?.addEventListener('click',()=>signalResult('idle'));
  $('agreed')?.addEventListener('change',event=>{state.agreed=event.target.checked;});
  $('email-events')?.addEventListener('change',event=>{state.emailEvents=event.target.checked;render();});
  $('signal-events')?.addEventListener('change',event=>{state.signalEvents=event.target.checked;});
  $('email')?.addEventListener('input',event=>{const changed=state.emailRequestedFor||state.emailVerified;setEmail(state,event.target.value);if(changed){render();$('email').focus();}});
  $('request-email')?.addEventListener('click',()=>{if(!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(state.email)){showError('Enter an email address before previewing verification.');return;}state.emailRequestedFor=state.email;error='';render();$('email-code').focus();});
  $('verify-email')?.addEventListener('click',()=>{if($('email-code').value!=='246810'){showError('Use the demo code 246810.');return;}state.emailVerified=true;error='';render();});
  document.querySelectorAll('[name="support"]').forEach(input=>input.onchange=()=>{state.support=input.value;if(state.support==='later')state.payment='none';error='';render();});
  document.querySelectorAll('[data-amount]').forEach(button=>button.onclick=()=>{state.amount=Number(button.dataset.amount);state.payment='none';render();});
  $('amount')?.addEventListener('input',event=>{const wasPaid=state.payment==='success';state.amount=Number(event.target.value);state.payment='none';if(wasPaid){render();$('amount').focus();}else{document.querySelectorAll('[data-amount]').forEach(button=>button.setAttribute('aria-pressed',String(Number(button.dataset.amount)===state.amount)));}});
  $('checkout')?.addEventListener('click',()=>{if(!Number.isInteger(state.amount)||state.amount<1||state.amount>500){showError('Choose a whole-dollar amount from $1 to $500.');return;}$('checkout-amount').textContent=`$${state.amount} / month`;$('checkout-dialog').showModal();});
  $('review-again')?.addEventListener('click',()=>move(5));
  $('done-reset')?.addEventListener('click',()=>restart());
}
$('entry').addEventListener('change',event=>restart(event.target.value));
$('reset').addEventListener('click',()=>restart());
$('questions').addEventListener('click',()=>$('questions-dialog').showModal());
document.querySelectorAll('[data-close]').forEach(button=>button.onclick=()=>$(button.dataset.close).close());
for(const [id,result] of [['pay-success','success'],['pay-failure','failed'],['pay-cancel','cancelled']])$(id).onclick=()=>{state.payment=result;$('checkout-dialog').close();error='';render();note(`Demo payment ${result}.`);};
$('checkout-dialog').addEventListener('cancel',()=>{state.payment='cancelled';render();});
setInterval(updateTimer,1000);
render();
// Optional browser tools share the same transitions and never call external services.
const context=document.modelContext;
if(context?.registerTool){const lifecycle=new AbortController();for(const tool of [
 {name:'read_signup_prototype',description:'Read the current simulated signup choices and step. No account or payment exists.',inputSchema:{type:'object',properties:{},additionalProperties:false},annotations:{readOnlyHint:true,untrustedContentHint:true},execute:()=>({step:steps[state.step],...summary(state)})},
 {name:'navigate_signup_prototype',description:'Navigate to a previously reached signup step; does not complete signup.',inputSchema:{type:'object',properties:{step:{type:'integer',minimum:0,maximum:5}},required:['step'],additionalProperties:false},annotations:{readOnlyHint:false},execute:input=>{if(!input||typeof input!=='object')throw new Error('A step is required.');move(input.step);return {step:steps[state.step]};}}
]){try{Promise.resolve(context.registerTool(tool,{signal:lifecycle.signal})).catch(()=>{});}catch{}}window.addEventListener('pagehide',()=>lifecycle.abort(),{once:true});}
