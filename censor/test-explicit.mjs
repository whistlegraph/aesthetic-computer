#!/usr/bin/env node

// Quick test of explicit content handling

const testMessages = [
  { text: "hello friend", shouldPass: true },
  { text: "https://example.com/cool-site", shouldPass: true },
  { text: "lets meet at the park", shouldPass: true },
  { text: "fuck you asshole", shouldPass: false },
  { text: "suck my dick", shouldPass: false },
  { text: "lick my poophole", shouldPass: false },
  { text: "comin out all drippin wet\nsuck it all up Lick my poophole and eat it\ndiarrhea is good for you", shouldPass: false },
  { text: "i hate you stupid bitch", shouldPass: false },
  { text: "lets smoke some weed", shouldPass: false },
  { text: "kill yourself", shouldPass: false },
];

const MODERATION_PROMPT = `Rate this chat message as PG-13 appropriate or not.

ALWAYS REPLY 't' FOR URLS AND LINKS (even if they contain words like "live", "lyt", etc.)

Block if message contains: sexual content, body functions, profanity, violence, drugs, hate speech.
Allow: URLs (https://, http://), links, normal conversation.

Reply with one letter: t (appropriate) or f (inappropriate)

Message: "`;

async function testMessage(message) {
  const response = await fetch('http://localhost:11434/api/generate', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      model: 'gemma2:2b',
      prompt: MODERATION_PROMPT + message + '"',
      stream: false,
      options: {
        num_ctx: 256,
        temperature: 0,
        num_predict: 5,
      }
    })
  });

  const data = await response.json();
  const responseText = data.response.toLowerCase().trim();
  
  // Extract decision
  let decision = '';
  if (responseText.startsWith('t')) {
    decision = 't';
  } else if (responseText.startsWith('f')) {
    decision = 'f';
  }
  
  return { decision, raw: responseText };
}

async function main() {
  console.log('🧪 Testing Explicit Content Detection with gemma2:2b\n');
  
  let correct = 0;
  let total = testMessages.length;
  
  for (const test of testMessages) {
    const result = await testMessage(test.text);
    const passed = (result.decision === 't') === test.shouldPass;
    const emoji = passed ? '✅' : '❌';
    const expected = test.shouldPass ? 't' : 'f';
    
    const displayText = test.text.length > 50 
      ? test.text.substring(0, 50) + '...' 
      : test.text;
    
    console.log(`${emoji} Expected: ${expected}, Got: ${result.decision} - "${displayText}"`);
    
    if (passed) correct++;
  }
  
  console.log(`\n📊 Results: ${correct}/${total} correct (${Math.round(correct/total*100)}%)`);
}

main();
