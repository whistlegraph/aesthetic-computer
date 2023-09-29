// Database, 23.07.09.17.33
// Manages database connections to MongoDB.
// And has application-specific queries.

import { MongoClient, ServerApiVersion } from "mongodb";
const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
const mongoDBName = process.env.MONGODB_NAME;

let client;

async function connect() {
  client = await MongoClient.connect(mongoDBConnectionString, {
    serverApi: {
      version: ServerApiVersion.v1,
      strict: true,
      deprecationErrors: true,
    },
  });
  const db = client.db(mongoDBName);
  return { db, disconnect };
}

async function disconnect() {
  await client?.close?.();
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

// Get all moods with handles included.
// 🗒️ There will be no `\\n` filtering here, so it should happen on client rendering.
async function allMoods(database) {
  const collection = database.db.collection("moods");
  const pipeline = [
    {
      $match: { deleted: { $ne: true } },
    },
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo",
      },
    },
    { $unwind: "$handleInfo" },
    {
      $project: {
        _id: 0,
        mood: 1,
        when: 1,
        handle: { $concat: ["@", "$handleInfo.handle"] },
      },
    },
    { $sort: { when: -1 } },
  ];

  const records = await collection.aggregate(pipeline).toArray();
  return records;
}

export { connect, moodFor, allMoods };

// Demo code from MongoDB's connection page: (23.08.15.19.59)

// Create a MongoClient with a MongoClientOptions object to set the Stable API version
// const client = new MongoClient(uri, {
//   serverApi: {
//     version: ServerApiVersion.v1,
//     strict: true,
//     deprecationErrors: true,
//   }
// });

// async function run() {
//   try {
//     // Connect the client to the server	(optional starting in v4.7)
//     await client.connect();
//     // Send a ping to confirm a successful connection
//     await client.db("admin").command({ ping: 1 });
//     console.log("Pinged your deployment. You successfully connected to MongoDB!");
//   } finally {
//     // Ensures that the client will close when you finish/error
//     await client.close();
//   }
// }
// run().catch(console.dir);
