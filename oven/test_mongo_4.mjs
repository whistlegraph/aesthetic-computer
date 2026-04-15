import { MongoClient } from 'mongodb';

const mongoUri = "mongodb://aesthetic_app:EcNT2vYjtFWoCCh4rB5IulTympiXBfdN@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0";
const mongoDb = "aesthetic";

const client = new MongoClient(mongoUri);
try {
  await client.connect();
  const db = client.db(mongoDb);
  
  // Check kidlisp collection fields
  const kidlispCollection = db.collection('kidlisp');
  const sample = await kidlispCollection.findOne();
  console.log('Sample kidlisp piece structure:');
  console.log(JSON.stringify(sample, null, 2).substring(0, 500));
  
  // Check if roz exists by looking for it in author info
  const rozPiece = await kidlispCollection.findOne({ $or: [
    { 'author': 'roz' },
    { 'name': 'roz' },
    { 'handle': 'roz' }
  ]});
  
  if (rozPiece) {
    console.log('\nFound roz piece:', { _id: rozPiece._id, fields: Object.keys(rozPiece) });
  } else {
    console.log('\nroz not found by author/name/handle');
  }
} finally {
  await client.close();
}
