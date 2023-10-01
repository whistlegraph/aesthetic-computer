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
  if (client) await client.close?.();
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
async function allMoods(database, handle = null) {
  const collection = database.db.collection("moods");
  const matchStage = { deleted: { $ne: true } };

  if (handle) {
    matchStage["handleInfo.handle"] = handle.replace(/^@/, "");
  }

  const pipeline = [
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
      $match: matchStage,
    },
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

// 🧻🖌️ Paintings media migrations:

import { S3Client, ListObjectsV2Command } from "@aws-sdk/client-s3";

async function listAndSavePaintings() {
  const s3User = new S3Client({
    endpoint: "https://" + process.env.USER_ENDPOINT,
    credentials: {
      accessKeyId: process.env.ART_KEY,
      secretAccessKey: process.env.ART_SECRET,
    },
  });

  const { db, disconnect } = await connect();
  const collection = db.collection("paintings");
  await collection.createIndex({ user: 1 });
  await collection.createIndex({ when: 1 });
  await collection.createIndex({ slug: 1 });
  await collection.createIndex({ slug: 1, user: 1 }, { unique: true });

  // Iterate through each user's top-level directory
  const userDirectories = await listDirectories({
    s3: s3User,
    bucket: process.env.USER_SPACE_NAME,
  });

  for (const dir of userDirectories) {
    // Check for `auth0` prefix and list its subdirectories
    if (dir.startsWith("auth0")) {
      console.log("Dir:", dir);

      const subDirectories = await listDirectories(
        { s3: s3User, bucket: process.env.USER_SPACE_NAME },
        dir,
      );

      // console.log("Subs:", subDirectories);

      // Check if the `painting` subdirectory exists
      if (subDirectories.includes(dir + "painting/")) {
        const files = await listFiles(
          { s3: s3User, bucket: process.env.USER_SPACE_NAME },
          dir + "painting/",
        );

        // console.log("Found paintings in:", dir);

        for (const file of files) {
          // Ignore the .zips.
          if (file.endsWith(".png")) {
            const slug = file.split("/").pop().replace(".png", "");
            const user = dir.split("/")[0];
            const existingRecord = await collection.findOne({ slug, user });
            if (!existingRecord) {
              const record = {
                slug,
                user,
                when: new Date(),
              };
              await collection.insertOne(record);
              console.log("✅ Added painting entry for:", slug);
            } else {
              console.log("⚠️ Painting already exists for:", slug);
            }
          }
        }
      }
    }
  }

  disconnect();
}

// Helper function to list directories for a given S3 client
async function listDirectories(client, prefix = "") {
  const params = { Bucket: client.bucket, Delimiter: "/", Prefix: prefix };
  const response = await client.s3.send(new ListObjectsV2Command(params));
  return response.CommonPrefixes
    ? response.CommonPrefixes.map((prefix) => prefix.Prefix)
    : [];
}

// Helper function to list files for a given S3 client and prefix
async function listFiles(client, prefix) {
  const params = { Bucket: client.bucket, Prefix: prefix };
  const response = await client.s3.send(new ListObjectsV2Command(params));
  return response.Contents.map((file) => file.Key);
}

export { listAndSavePaintings };
