// Database, 23.07.09.17.33
// Manages database connections to MongoDB.
// And has application-specific queries.

import { MongoClient } from "mongodb";
const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
const mongoDBName = process.env.MONGODB_NAME;

let client;

async function connect() {
  client = await MongoClient.connect(mongoDBConnectionString, {
    useUnifiedTopology: true,
  });
  const db = client.db(mongoDBName);
  return { db, disconnect };
}

async function disconnect() {
  await client?.close();
}

// Re-usable calls to the application.
async function moodFor(sub, database) {
  const collection = database.db.collection("moods");
  const record = (
    await collection.find({ user: sub }).sort({ when: -1 }).limit(1).toArray()
  )[0]; // Most recent mood or `undefined` if none are found.
  if (record?.mood) {
    record.mood = record.mood.replaceAll("\\n", "\n"); // Replace \\n -> \n.
  }
  return record;
}

export { connect, moodFor };