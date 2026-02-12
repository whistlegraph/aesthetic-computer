#!/usr/bin/env node

import { MongoClient } from 'mongodb';

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || 'aesthetic';

async function main() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  
  try {
    await client.connect();
    console.log('Connected to MongoDB\n');
    
    const db = client.db(MONGODB_NAME);
    
    // List all collections
    console.log('Collections in database:');
    const collections = await db.listCollections().toArray();
    for (const coll of collections) {
      console.log(`  - ${coll.name}`);
    }
    
    // Check for chat-related collections
    console.log('\n='.repeat(80));
    const chatCollections = collections.filter(c => 
      c.name.toLowerCase().includes('chat') || 
      c.name.toLowerCase().includes('message')
    );
    
    if (chatCollections.length > 0) {
      console.log('\nChat-related collections found:');
      
      for (const coll of chatCollections) {
        console.log(`\n${coll.name}:`);
        const collection = db.collection(coll.name);
        const count = await collection.countDocuments();
        console.log(`  Total documents: ${count}`);
        
        if (count > 0) {
          const sample = await collection.findOne();
          console.log(`  Sample document:`);
          console.log(JSON.stringify(sample, null, 2));
        }
      }
    } else {
      console.log('\nNo chat-related collections found. Checking all collections for messages...\n');
      
      for (const coll of collections) {
        const collection = db.collection(coll.name);
        const count = await collection.countDocuments();
        if (count > 0) {
          const sample = await collection.findOne();
          console.log(`\n${coll.name} (${count} documents):`);
          console.log(JSON.stringify(sample, null, 2).substring(0, 500) + '...');
        }
      }
    }
    
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  } finally {
    await client.close();
  }
}

main();
