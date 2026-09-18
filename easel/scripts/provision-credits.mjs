// Run on Lith with its existing environment. Never prints API keys or secrets.
import Stripe from '../../system/node_modules/stripe/esm/stripe.esm.node.js';
import {existsSync,mkdirSync,writeFileSync} from 'node:fs';
const envFile='/etc/aesthetic-computer/ac-credits.env';
if(existsSync(envFile))throw Error('Credit environment already exists; inspect before reprovisioning');
if(process.env.CONTEXT==='dev')throw Error('Refusing live provisioning from a dev environment');
const stripe=new Stripe(process.env.STRIPE_API_PRIV_KEY);
const endpoint=await stripe.webhookEndpoints.create({url:'https://aesthetic.computer/api/easel-checkout',enabled_events:['checkout.session.completed','checkout.session.async_payment_succeeded','charge.refunded'],description:'AC braincells'});
mkdirSync('/etc/aesthetic-computer',{recursive:true});
writeFileSync(envFile,`AC_CREDITS_CHECKOUT_ENABLED=true\nSTRIPE_AC_CREDITS_WEBHOOK_SECRET=${endpoint.secret}\n`,{mode:0o600});
mkdirSync('/etc/systemd/system/lith.service.d',{recursive:true});
writeFileSync('/etc/systemd/system/lith.service.d/ac-credits.conf',`[Service]\nEnvironmentFile=${envFile}\n`);
console.log(JSON.stringify({endpoint:endpoint.id,url:endpoint.url,livemode:endpoint.livemode}));
