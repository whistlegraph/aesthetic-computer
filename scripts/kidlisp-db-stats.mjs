// MongoDB stats for kidlisp ecosystem report
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { MongoClient } = require('/workspaces/aesthetic-computer/system/node_modules/mongodb');

const uri = process.env.MONGODB_CONNECTION_STRING;
const dbName = 'aesthetic';

async function main() {
  const client = new MongoClient(uri);
  try {
    await client.connect();
    console.log('Connected to MongoDB Atlas');
    
    const db = client.db(dbName);
    
    // List all collections
    const collections = await db.listCollections().toArray();
    console.log('\n📊 Collections:');
    collections.forEach(c => console.log('  -', c.name));
    
    // Count documents in kidlisp collection
    const kidlispCount = await db.collection('kidlisp').countDocuments();
    console.log('\n📝 KidLisp documents:', kidlispCount);
    
    // Get some stats on kidlisp
    const kidlispStats = await db.collection('kidlisp').aggregate([
      { $group: {
        _id: null,
        totalDocs: { $sum: 1 },
        withUser: { $sum: { $cond: [{ $ifNull: ['$user', false] }, 1, 0] } },
        avgHits: { $avg: { $ifNull: ['$hits', 0] } },
        maxHits: { $max: { $ifNull: ['$hits', 0] } }
      }}
    ]).toArray();
    
    if (kidlispStats[0]) {
      console.log('📈 Stats:');
      console.log('  - Total:', kidlispStats[0].totalDocs);
      console.log('  - With user:', kidlispStats[0].withUser);
      console.log('  - Anonymous:', kidlispStats[0].totalDocs - kidlispStats[0].withUser);
      console.log('  - Avg hits:', Math.round(kidlispStats[0].avgHits * 100) / 100);
      console.log('  - Max hits:', kidlispStats[0].maxHits);
    }
    
    // Get sample of recent kidlisp
    const samples = await db.collection('kidlisp')
      .find({})
      .sort({ when: -1 })
      .limit(5)
      .project({ code: 1, when: 1, hits: 1, user: 1, source: 1 })
      .toArray();
    
    console.log('\n🔖 Recent kidlisp samples:');
    samples.forEach(s => {
      const sourcePreview = s.source?.slice(0, 60).replace(/\n/g, ' ') + (s.source?.length > 60 ? '...' : '');
      console.log('  - [' + s.code + '] hits:', s.hits || 0, 'user:', s.user ? 'yes' : 'anon');
      console.log('    ', sourcePreview);
    });
    
    // Top hits
    const topHits = await db.collection('kidlisp')
      .find({})
      .sort({ hits: -1 })
      .limit(5)
      .project({ code: 1, hits: 1, source: 1 })
      .toArray();
    
    console.log('\n🔥 Top hit kidlisp:');
    topHits.forEach(s => {
      const sourcePreview = s.source?.slice(0, 50).replace(/\n/g, ' ') + (s.source?.length > 50 ? '...' : '');
      console.log('  - [' + s.code + '] hits:', s.hits || 0);
      console.log('    ', sourcePreview);
    });
    
    // Other relevant collections  
    console.log('\n📦 Other counts:');
    const paintings = await db.collection('paintings').countDocuments();
    console.log('  - Paintings:', paintings);
    
    const handles = await db.collection('@handles').countDocuments();
    console.log('  - Handles:', handles);
    
    const moods = await db.collection('moods').countDocuments();
    console.log('  - Moods:', moods);
    
  } catch (err) {
    console.error('Error:', err.message);
  } finally {
    await client.close();
  }
}

main();
