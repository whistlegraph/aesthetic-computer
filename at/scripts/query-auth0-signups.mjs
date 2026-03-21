#!/usr/bin/env node
// Query Auth0 for recent user signups with created_at timestamps

import { shell } from '/workspaces/aesthetic-computer/system/backend/shell.mjs';
import { config } from 'dotenv';

config();

async function getAuth0RecentUsers(tenant = 'aesthetic') {
  try {
    const { got } = await import('got');
    
    const clientId = tenant === 'aesthetic' 
      ? process.env.AUTH0_M2M_CLIENTID 
      : process.env.SOTCE_AUTH0_M2M_CLIENTID;
    const clientSecret = tenant === 'aesthetic'
      ? process.env.AUTH0_M2M_CLIENT_SECRET
      : process.env.SOTCE_AUTH0_M2M_CLIENT_SECRET;
    const baseURI = tenant === 'aesthetic'
      ? 'https://aesthetic.us.auth0.com'
      : 'https://sotce.us.auth0.com';

    // Get access token
    const tokenResponse = await got.post(`${baseURI}/oauth/token`, {
      json: {
        client_id: clientId,
        client_secret: clientSecret,
        audience: `${baseURI}/api/v2/`,
        grant_type: 'client_credentials',
      },
      responseType: 'json',
    });

    const token = tokenResponse.body.access_token;

    // Query users sorted by created_at descending
    const response = await got(`${baseURI}/api/v2/users`, {
      searchParams: {
        sort: 'created_at:-1', // Most recent first
        per_page: 20,
        page: 0,
        fields: 'user_id,email,created_at',
        include_fields: true,
      },
      headers: { Authorization: `Bearer ${token}` },
      responseType: 'json',
    });

    return response.body;
  } catch (error) {
    shell.error(`Error querying Auth0: ${error.message}`);
    return null;
  }
}

console.log('\n🔍 Querying Auth0 for recent user signups...\n');

// Query both tenants
const aestheticUsers = await getAuth0RecentUsers('aesthetic');
const sotceUsers = await getAuth0RecentUsers('sotce');

if (aestheticUsers) {
  console.log('📅 Recent AESTHETIC tenant signups:\n');
  aestheticUsers.forEach((user, i) => {
    const createdAt = new Date(user.created_at);
    const hoursAgo = (Date.now() - createdAt.getTime()) / (1000 * 60 * 60);
    console.log(`${i + 1}. ${user.user_id}`);
    console.log(`   Email: ${user.email || 'N/A'}`);
    console.log(`   Created: ${createdAt.toISOString()}`);
    console.log(`   ${hoursAgo.toFixed(1)} hours ago (${(hoursAgo / 24).toFixed(1)} days)`);
    console.log('');
  });
}

if (sotceUsers) {
  console.log('\n📅 Recent SOTCE tenant signups:\n');
  sotceUsers.forEach((user, i) => {
    const createdAt = new Date(user.created_at);
    const hoursAgo = (Date.now() - createdAt.getTime()) / (1000 * 60 * 60);
    console.log(`${i + 1}. ${user.user_id}`);
    console.log(`   Email: ${user.email || 'N/A'}`);
    console.log(`   Created: ${createdAt.toISOString()}`);
    console.log(`   ${hoursAgo.toFixed(1)} hours ago (${(hoursAgo / 24).toFixed(1)} days)`);
    console.log('');
  });
}

console.log('\n⏰ Current time:', new Date().toISOString());
