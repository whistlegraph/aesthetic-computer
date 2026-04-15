import { MongoClient } from 'mongodb';

const mongoUri = "mongodb://aesthetic_app:EcNT2vYjtFWoCCh4rB5IulTympiXBfdN@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0";
const mongoDb = "aesthetic";

const client = new MongoClient(mongoUri);
try {
  await client.connect();
  const db = client.db(mongoDb);
  
  // List collections
  const collections = await db.listCollections().toArray();
  console.log('Collections:', collections.map(c => c.name));
  
  // Check pieces collection
  const piecesCollection = db.collection('pieces');
  const count = await piecesCollection.countDocuments();
  console.log('Total pieces:', count);
  
  // Find $roz
  const roz = await piecesCollection.findOne({
    $or: [
      { name: 'roz' },
      { name: '$roz' },
      { _id: 'roz' },
      { _id: '$roz' }
    ]
  });
  
  if (roz) {
    console.log('Found piece:', { name: roz.name, _id: roz._id, hasSource: !!roz.source });
  } else {
    console.log('Piece not found, searching for pieces with roz in name...');
    const samples = await piecesCollection.find({ name: /roz/i }).limit(5).toArray();
    console.log('Samples:', samples.map(p => ({ name: p.name, _id: p._id })));
  }
} finally {
  await client.close();
}
