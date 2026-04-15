import { MongoClient } from 'mongodb';

const mongoUri = "mongodb://aesthetic_app:EcNT2vYjtFWoCCh4rB5IulTympiXBfdN@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0";
const mongoDb = "aesthetic";

const client = new MongoClient(mongoUri);
try {
  await client.connect();
  const db = client.db(mongoDb);
  
  // Check pieces collection structure
  const piecesCollection = db.collection('pieces');
  const samples = await piecesCollection.find().limit(5).toArray();
  console.log('Sample pieces:');
  samples.forEach(p => {
    console.log(JSON.stringify({ _id: p._id, name: p.name, hasSource: !!p.source, code: p.code }, null, 2));
  });
} finally {
  await client.close();
}
