// Quick script to query pieces from MongoDB
import { connect } from './system/backend/database.mjs';

const slug = process.argv[2];

try {
  const { db, disconnect } = await connect();
  const collection = db.collection('pieces');
  
  if (slug) {
    const piece = await collection.findOne({ slug });
    
    if (piece) {
      console.log('Found piece:');
      console.log(JSON.stringify(piece, null, 2));
      console.log('\nKeys in document:', Object.keys(piece));
    } else {
      console.log(`No piece found with slug: ${slug}`);
    }
  } else {
    // Show a few recent pieces with atproto field to see the structure
    console.log('Fetching recent pieces with atproto field...\n');
    const pieces = await collection
      .find({ 'atproto.rkey': { $exists: true } })
      .sort({ when: -1 })
      .limit(3)
      .toArray();
    
    pieces.forEach((piece, i) => {
      console.log(`\n=== Piece ${i + 1} ===`);
      console.log(JSON.stringify(piece, null, 2));
    });
  }
  
  await disconnect();
} catch (error) {
  console.error('Error querying piece:', error);
  process.exit(1);
}
