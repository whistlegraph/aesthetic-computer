import { MongoClient } from 'mongodb';

const mongoUri = "mongodb://aesthetic_app:EcNT2vYjtFWoCCh4rB5IulTympiXBfdN@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0";
const mongoDb = "aesthetic";

const client = new MongoClient(mongoUri);
try {
  await client.connect();
  const db = client.db(mongoDb);
  
  // Check kidlisp collection
  const kidlispCollection = db.collection('kidlisp');
  const count = await kidlispCollection.countDocuments();
  console.log('Total kidlisp pieces:', count);
  
  const sample = await kidlispCollection.findOne({ _id: 'roz' });
  if (sample) {
    console.log('Found $roz in kidlisp:', { _id: sample._id, hasSource: !!sample.source, sourceLength: sample.source?.length });
  } else {
    console.log('$roz not in kidlisp, checking samples...');
    const samples = await kidlispCollection.find().limit(3).toArray();
    console.log('Samples:', samples.map(p => ({ _id: p._id, hasSource: !!p.source })));
  }
} finally {
  await client.close();
}
