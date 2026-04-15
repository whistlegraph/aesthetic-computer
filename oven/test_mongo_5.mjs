import { MongoClient } from 'mongodb';

const mongoUri = "mongodb://aesthetic_app:EcNT2vYjtFWoCCh4rB5IulTympiXBfdN@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0";
const mongoDb = "aesthetic";

const client = new MongoClient(mongoUri);
try {
  await client.connect();
  const db = client.db(mongoDb);
  
  // Find by code field
  const kidlispCollection = db.collection('kidlisp');
  const rozPiece = await kidlispCollection.findOne({ code: 'roz' });
  
  if (rozPiece) {
    console.log('Found roz piece!');
    console.log(JSON.stringify({ code: rozPiece.code, source: rozPiece.source, hash: rozPiece.hash }, null, 2));
  } else {
    console.log('roz not found by code field');
    // Search for pieces with "roz" in code
    const matches = await kidlispCollection.find({ code: /roz/i }).limit(3).toArray();
    console.log('Pieces with roz in code:', matches.map(p => p.code));
  }
} finally {
  await client.close();
}
