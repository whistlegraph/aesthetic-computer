#!/usr/bin/env node
// check-auth0-webhook-config.mjs
// Check if Auth0 Log Stream is properly configured and receiving events

import { config } from 'dotenv';

config();

console.log('\n🔍 Checking Auth0 Log Stream Configuration\n');
console.log('════════════════════════════════════════════════════════════════\n');

// Check environment variables
console.log('📋 Environment Variables:\n');
console.log(`AUTH0_LOG_TOKEN: ${process.env.AUTH0_LOG_TOKEN ? '✅ Set' : '❌ Missing'}`);
console.log(`AUTH0_M2M_CLIENT_ID: ${process.env.AUTH0_M2M_CLIENT_ID ? '✅ Set' : '❌ Missing'}`);
console.log(`AUTH0_M2M_SECRET: ${process.env.AUTH0_M2M_SECRET ? '✅ Set' : '❌ Missing'}`);

console.log('\n════════════════════════════════════════════════════════════════\n');
console.log('📍 Expected Auth0 Log Stream Configuration:\n');
console.log('   Dashboard: https://manage.auth0.com/dashboard/us/aesthetic/log-streams');
console.log('   Endpoint: https://aesthetic.computer/.netlify/functions/auth0-events');
console.log('   Authorization Header: Should match AUTH0_LOG_TOKEN');
console.log('   Event Types to Stream:');
console.log('      - ss (Success Signup)');
console.log('      - sv (Success Verification Email)');

console.log('\n════════════════════════════════════════════════════════════════\n');
console.log('💡 Troubleshooting Steps:\n');
console.log('   1. Verify Log Stream is active in Auth0 dashboard');
console.log('   2. Check that webhook URL is correct');
console.log('   3. Verify AUTH0_LOG_TOKEN matches in both Auth0 and .env');
console.log('   4. Check Netlify function logs for incoming webhook calls');
console.log('   5. Test webhook manually with curl/Postman\n');

console.log('🔗 To check Netlify function logs, run:');
console.log('   netlify functions:log auth0-events\n');

console.log('🔗 Or view in Netlify dashboard:');
console.log('   https://app.netlify.com/sites/aesthetic-computer/logs/functions\n');
