// Billing Aggregation, 25.12.30
// 🧾 Aggregates billing data from AC's SaaS providers
// Credentials stored in MongoDB to avoid Netlify's 4KB env var limit
// Endpoint: /api/billing

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";

const dev = process.env.CONTEXT === "dev";

// Cache credentials in memory for warm function invocations
let cachedCredentials = null;

async function getBillingCredentials() {
  if (cachedCredentials) return cachedCredentials;
  
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "billing" });
  
  if (!secrets) {
    console.log("Billing credentials not found in database - using static estimates only");
    return null;
  }
  
  cachedCredentials = {
    digitalocean: secrets.digitalocean, // { token }
    cloudflare: secrets.cloudflare,     // { email, apiKey, accountId }
    pinata: secrets.pinata,             // { apiKey, apiSecret }
  };
  
  return cachedCredentials;
}

// Provider configurations
const PROVIDERS = {
  digitalocean: {
    name: "DigitalOcean",
    description: "Servers, databases, spaces",
    endpoints: {
      balance: "https://api.digitalocean.com/v2/customers/my/balance",
      billing_history: "https://api.digitalocean.com/v2/customers/my/billing_history",
    },
  },
  stripe: {
    name: "Stripe",
    description: "Payment processing",
    // Stripe billing is for incoming payments, not outgoing costs
    // We track fees separately
  },
  netlify: {
    name: "Netlify",
    description: "Hosting, functions, edge",
    // Netlify billing API is limited - we'll estimate based on usage
  },
  cloudflare: {
    name: "Cloudflare",
    description: "DNS, CDN, tunnels",
    endpoints: {
      billing: (accountId) => `https://api.cloudflare.com/client/v4/accounts/${accountId}/billing/profile`,
    },
  },
  mongodb: {
    name: "MongoDB Atlas",
    description: "Database",
    // Atlas billing requires org-level API access
  },
  pinata: {
    name: "Pinata",
    description: "IPFS pinning",
    endpoints: {
      usage: "https://api.pinata.cloud/data/userPinnedDataTotal",
    },
  },
  jamsocket: {
    name: "Jamsocket",
    description: "Session server",
    // Would need to check their API
  },
  vercel: {
    name: "Vercel", 
    description: "Edge functions (backup)",
    // Minimal usage currently
  },
  openai: {
    name: "OpenAI",
    description: "AI/LLM APIs",
    endpoints: {
      usage: "https://api.openai.com/v1/dashboard/billing/usage",
    },
  },
  anthropic: {
    name: "Anthropic",
    description: "Claude AI",
    // Check for billing API
  },
};

/**
 * Fetch DigitalOcean billing data
 */
async function fetchDigitalOcean(credentials) {
  const token = credentials?.digitalocean?.token;
  if (!token) return { provider: "digitalocean", name: PROVIDERS.digitalocean.name, skipped: true, reason: "No credentials configured" };

  try {
    const headers = { Authorization: `Bearer ${token}` };
    
    const [balanceRes, historyRes] = await Promise.all([
      fetch(PROVIDERS.digitalocean.endpoints.balance, { headers }),
      fetch(PROVIDERS.digitalocean.endpoints.billing_history, { headers }),
    ]);

    const balance = await balanceRes.json();
    const history = await historyRes.json();

    return {
      provider: "digitalocean",
      name: PROVIDERS.digitalocean.name,
      description: PROVIDERS.digitalocean.description,
      balance: {
        monthToDate: balance.month_to_date_balance,
        accountBalance: balance.account_balance,
        monthToDateUsage: balance.month_to_date_usage,
        generatedAt: balance.generated_at,
      },
      recentHistory: history.billing_history?.slice(0, 5).map(h => ({
        date: h.date,
        type: h.type,
        description: h.description,
        amount: h.amount,
      })),
    };
  } catch (error) {
    return { provider: "digitalocean", error: error.message };
  }
}

/**
 * Fetch Cloudflare billing data
 */
async function fetchCloudflare(credentials) {
  const email = credentials?.cloudflare?.email;
  const apiKey = credentials?.cloudflare?.apiKey;
  const accountId = credentials?.cloudflare?.accountId;
  
  if (!email || !apiKey || !accountId) {
    return { provider: "cloudflare", name: PROVIDERS.cloudflare.name, skipped: true, reason: "Cloudflare credentials not configured" };
  }

  try {
    const headers = {
      'X-Auth-Email': email,
      'X-Auth-Key': apiKey,
      'Content-Type': 'application/json',
    };

    const res = await fetch(PROVIDERS.cloudflare.endpoints.billing(accountId), { headers });
    const data = await res.json();

    if (!data.success) {
      return { provider: "cloudflare", error: data.errors?.[0]?.message || "Unknown error" };
    }

    return {
      provider: "cloudflare",
      name: PROVIDERS.cloudflare.name,
      description: PROVIDERS.cloudflare.description,
      plan: data.result?.plan?.name,
      // Most CF features are free tier for AC
    };
  } catch (error) {
    return { provider: "cloudflare", error: error.message };
  }
}

/**
 * Fetch Pinata usage data
 */
async function fetchPinata(credentials) {
  const apiKey = credentials?.pinata?.apiKey;
  const apiSecret = credentials?.pinata?.apiSecret;
  
  if (!apiKey || !apiSecret) {
    return { provider: "pinata", name: PROVIDERS.pinata.name, skipped: true, reason: "Pinata credentials not configured" };
  }

  try {
    const res = await fetch(PROVIDERS.pinata.endpoints.usage, {
      headers: {
        pinata_api_key: apiKey,
        pinata_secret_api_key: apiSecret,
      },
    });
    const data = await res.json();

    return {
      provider: "pinata",
      name: PROVIDERS.pinata.name,
      description: PROVIDERS.pinata.description,
      usage: {
        pinnedCount: data.pin_count,
        pinnedSizeBytes: data.pin_size_total,
        pinnedSizeGB: (data.pin_size_total / (1024 * 1024 * 1024)).toFixed(2),
      },
    };
  } catch (error) {
    return { provider: "pinata", error: error.message };
  }
}

/**
 * Fetch OpenAI usage (if available)
 */
async function fetchOpenAI() {
  const apiKey = process.env.OPENAI_API_KEY;
  if (!apiKey) return { provider: "openai", name: PROVIDERS.openai.name, skipped: true, reason: "No OPENAI_API_KEY configured" };

  // Note: OpenAI billing API requires organization-level access
  // This is a placeholder - actual implementation may vary
  return {
    provider: "openai",
    name: PROVIDERS.openai.name,
    description: PROVIDERS.openai.description,
    note: "Check dashboard.openai.com for usage",
  };
}

/**
 * Static/estimated costs for providers without APIs
 */
function getStaticEstimates() {
  return [
    {
      provider: "netlify",
      name: PROVIDERS.netlify.name,
      description: PROVIDERS.netlify.description,
      estimated: true,
      monthlyEstimate: 19, // Pro plan
      note: "Pro plan - check netlify.com/billing",
    },
    {
      provider: "jamsocket",
      name: PROVIDERS.jamsocket.name,
      description: PROVIDERS.jamsocket.description,
      estimated: true,
      monthlyEstimate: 0, // Currently on free/early tier?
      note: "Session server hosting",
    },
    {
      provider: "mongodb",
      name: PROVIDERS.mongodb.name,
      description: PROVIDERS.mongodb.description,
      estimated: true,
      monthlyEstimate: 0, // M0 free tier
      note: "Atlas M0 free tier",
    },
    {
      provider: "anthropic",
      name: PROVIDERS.anthropic.name,
      description: PROVIDERS.anthropic.description,
      estimated: true,
      note: "Usage-based - check console.anthropic.com",
    },
  ];
}

export async function handler(event, context) {
  // Only allow GET
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // Sensitive endpoint: require @jeffrey admin auth.
  const user = await authorize(event.headers || {});
  if (!user) {
    return respond(401, { error: "Unauthorized" });
  }
  const isAdmin = await hasAdmin(user);
  if (!isAdmin) {
    return respond(403, { error: "Admin access required" });
  }

  const query = event.queryStringParameters || {};
  const provider = query.provider; // Optional: filter by provider

  try {
    // Get credentials from MongoDB
    const credentials = await getBillingCredentials();
    
    // Fetch from all providers in parallel
    const [digitalocean, cloudflare, pinata, openai] = await Promise.all([
      fetchDigitalOcean(credentials),
      fetchCloudflare(credentials),
      fetchPinata(credentials),
      fetchOpenAI(),
    ]);

    const dynamicProviders = [digitalocean, cloudflare, pinata, openai];
    const staticProviders = getStaticEstimates();
    
    let allProviders = [...dynamicProviders, ...staticProviders];

    // Filter by provider if requested
    if (provider) {
      allProviders = allProviders.filter(p => p.provider === provider);
    }

    // Calculate totals where possible
    const monthlyEstimateTotal = staticProviders
      .filter(p => p.monthlyEstimate)
      .reduce((sum, p) => sum + p.monthlyEstimate, 0);

    const doBalance = digitalocean.balance?.monthToDateUsage 
      ? parseFloat(digitalocean.balance.monthToDateUsage.replace('$', ''))
      : 0;

    return respond(200, {
      generated: new Date().toISOString(),
      summary: {
        monthlyEstimate: `$${(monthlyEstimateTotal + doBalance).toFixed(2)}`,
        note: "Partial data - some providers require manual checking",
      },
      providers: allProviders,
    });
  } catch (error) {
    console.error("Billing aggregation error:", error);
    return respond(500, { error: "Failed to aggregate billing data" });
  }
}
