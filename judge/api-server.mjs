import http from 'http';
import { URL } from 'url';
import { WebSocketServer } from 'ws';
import { MongoClient } from 'mongodb';
import dotenv from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

// Load environment variables from vault
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const vaultEnvPath = join(__dirname, '../aesthetic-computer-vault/censor/.env');
dotenv.config({ path: vaultEnvPath });

const OLLAMA_API = 'http://localhost:11434/api/generate';
const MODEL = 'gemma2:2b';
const PORT = 3000;

// MongoDB connection
const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING || 'mongodb://localhost:27017';
const MONGODB_NAME = process.env.MONGODB_NAME || 'aesthetic';
const COLLECTION_NAME = process.env.COLLECTION_NAME || 'chat-system';
let mongoClient = null;
let chatCollection = null;

async function connectMongo() {
  if (!mongoClient) {
    mongoClient = new MongoClient(MONGODB_CONNECTION_STRING, {
      serverSelectionTimeoutMS: 2000, // Timeout after 2 seconds
      connectTimeoutMS: 2000
    });
    await mongoClient.connect();
    chatCollection = mongoClient.db(MONGODB_NAME).collection(COLLECTION_NAME);
    console.log('📦 Connected to MongoDB');
  }
  return chatCollection;
}

// PG-13 content filter prompt
const systemPrompt = `You are a PG-13 content filter for a chat room. Respond in s-expression format.

ALWAYS reply with: (yes) (why "short lowercase explanation") or (no) (why "short lowercase explanation")

Reply (yes) for URLs (http://, https://, www., or domain.tld patterns).

Reply (no) if the message contains:
- Sexual content or innuendo
- Body functions (bathroom humor, gross-out content)
- Profanity or explicit language
- Violence or threats
- Drug references
- Hate speech or slurs

Reply (yes) for:
- Normal conversation
- URLs and links
- Questions and discussions
- Greetings and casual chat

Example responses:
(yes) (why "normal greeting")
(no) (why "contains profanity")
(yes) (why "url allowed")

Keep all explanations lowercase and brief. Reply only in this s-expression format.`;

async function filterMessage(message, onChunk = null) {
  const prompt = `${systemPrompt}\n\nMessage: "${message}"\n\nReply (t/f):`;
  
  const response = await fetch(OLLAMA_API, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      model: MODEL,
      prompt: prompt,
      stream: true, // Enable streaming
    }),
  });

  if (!response.ok) {
    throw new Error(`Ollama API error: ${response.status}`);
  }

  let fullResponse = '';
  let totalDuration = 0;
  
  // Stream the response
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    
    const chunk = decoder.decode(value);
    const lines = chunk.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        const data = JSON.parse(line);
        if (data.response) {
          fullResponse += data.response;
          if (onChunk) {
            onChunk({ chunk: data.response, full: fullResponse });
          }
        }
        if (data.total_duration) {
          totalDuration = data.total_duration;
        }
      } catch (e) {
        // Skip invalid JSON lines
      }
    }
  }
  
  const finalResponse = fullResponse.trim();
  
  // Parse s-expression: (yes) or (no) and extract why
  const decision = finalResponse.toLowerCase().includes('(yes)') ? 't' : 'f';
  
  // Extract (why "...") - proper s-expression format
  let reason = '';
  const reasonMatch = finalResponse.match(/\(why\s+"((?:[^"\\]|\\.)*)"\)/is);
  if (reasonMatch) {
    reason = reasonMatch[1].toLowerCase().trim(); // Force lowercase and trim
  }
  
  return {
    decision: decision,
    sentiment: finalResponse,
    reason: reason,
    responseTime: totalDuration / 1e9, // Convert to seconds
  };
}

const server = http.createServer(async (req, res) => {
  // CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  if (req.method === 'GET' && req.url.startsWith('/api/chat-messages')) {
    // Fetch random sample of chat messages for auto-testing
    try {
      const collection = await connectMongo();
      const messages = await collection
        .aggregate([
          { $match: { content: { $exists: true, $ne: '' } } },
          { $sample: { size: 100 } },
          { $project: { content: 1, _id: 0 } }
        ])
        .toArray();
      
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(messages.map(m => m.content)));
    } catch (error) {
      console.error('MongoDB error:', error);
      // Fallback to test messages if MongoDB unavailable
      const testMessages = [
        "hello world",
        "how are you doing today?",
        "fuck you",
        "shit this sucks",
        "nice painting!",
        "what time is it?",
        "https://example.com/image.png",
        "i hate you so much",
        "beautiful colors",
        "damn that's cool",
        "check out my site www.example.com",
        "you're an idiot",
        "love this piece",
        "kill yourself",
        "great work!",
        "this is garbage",
        "can you help me?",
        "stupid ass program",
        "amazing art",
        "go to hell"
      ];
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(testMessages));
    }
  } else if (req.method === 'POST' && req.url === '/api/filter') {
    let body = '';
    
    req.on('data', chunk => {
      body += chunk.toString();
    });

    req.on('end', async () => {
      try {
        const { message } = JSON.parse(body);
        
        if (!message || typeof message !== 'string') {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Invalid message' }));
          return;
        }

        console.log(`[${new Date().toISOString()}] Testing: "${message.substring(0, 50)}${message.length > 50 ? '...' : ''}"`);
        
        const result = await filterMessage(message);
        
        console.log(`  → Decision: ${result.decision === 't' ? 'PASS' : 'FAIL'} (${(result.responseTime * 1000).toFixed(0)}ms)`);

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify(result));
      } catch (error) {
        console.error('Error:', error);
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
  } else {
    res.writeHead(404, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ error: 'Not found' }));
  }
});

// Track warmup state
let isWarmedUp = false;
let warmupPromise = null;

async function warmupModel() {
  if (isWarmedUp) return;
  if (warmupPromise) return warmupPromise;
  
  warmupPromise = (async () => {
    console.log(`⏳ Warming up model...`);
    try {
      await filterMessage('hello');
      isWarmedUp = true;
      console.log(`✅ Model warmed up and ready!\n`);
    } catch (error) {
      console.log(`⚠️  Model warmup failed: ${error.message}`);
      console.log(`   First request may be slower.\n`);
    }
  })();
  
  return warmupPromise;
}

server.listen(PORT, async () => {
  console.log(`🛡️  Content Filter API running on http://localhost:${PORT}`);
  console.log(`📊 Dashboard available via Caddy on http://localhost:8080`);
  console.log(`🤖 Using model: ${MODEL}\n`);
  
  // Start warmup (don't block server startup)
  warmupModel().catch(err => console.error('❌ Warmup error:', err));
});

// Handle uncaught errors
process.on('uncaughtException', (err) => {
  console.error('❌ Uncaught exception:', err);
});

process.on('unhandledRejection', (err) => {
  console.error('❌ Unhandled rejection:', err);
});

// WebSocket server for live streaming
const wss = new WebSocketServer({ server, path: '/ws' });

wss.on('connection', (ws) => {
  console.log('📡 WebSocket client connected');
  
  ws.on('message', async (data) => {
    try {
      const { message } = JSON.parse(data);
      
      if (!message || typeof message !== 'string') {
        ws.send(JSON.stringify({ error: 'Invalid message' }));
        return;
      }
      
      // Wait for warmup to complete before processing
      if (!isWarmedUp) {
        ws.send(JSON.stringify({ type: 'warming', message: 'Model warming up...' }));
        await warmupPromise;
      }
      
      console.log(`[${new Date().toISOString()}] Streaming test: "${message.substring(0, 50)}${message.length > 50 ? '...' : ''}"`);
      
      const startTime = Date.now();
      
      // Send start event
      ws.send(JSON.stringify({ type: 'start' }));
      
      // Filter with streaming
      const result = await filterMessage(message, (update) => {
        ws.send(JSON.stringify({ 
          type: 'chunk', 
          chunk: update.chunk,
          full: update.full 
        }));
      });
      
      const responseTime = Date.now() - startTime;
      
      console.log(`  → Decision: ${result.decision === 't' ? 'PASS' : 'FAIL'} (${responseTime}ms)`);
      
      // Send final result
      ws.send(JSON.stringify({ 
        type: 'complete',
        decision: result.decision,
        sentiment: result.sentiment,
        reason: result.reason,
        responseTime: responseTime
      }));
      
    } catch (error) {
      console.error('WebSocket error:', error);
      ws.send(JSON.stringify({ type: 'error', error: error.message }));
    }
  });
  
  ws.on('close', () => {
    console.log('📡 WebSocket client disconnected');
  });
});
