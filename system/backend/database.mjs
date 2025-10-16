// Database, 23.07.09.17.33
// Manages database connections to MongoDB.
// And has application-specific queries.

import { MongoClient, ObjectId, ServerApiVersion } from "mongodb";

let client;

async function connect() {
  // Read environment variables at runtime, not at module load time
  const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
  const mongoDBName = process.env.MONGODB_NAME;
  
  // Validate environment variables
  if (!mongoDBConnectionString) {
    throw new Error('MONGODB_CONNECTION_STRING environment variable is not set');
  }
  if (!mongoDBName) {
    throw new Error('MONGODB_NAME environment variable is not set');
  }

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
// 🗒️ There will be no `\n` filtering here, so it should happen on client rendering.
async function allMoods(database, handles = null) {
  const collection = database.db.collection("moods");
  const matchStage = { deleted: { $ne: true } };

  // Refactored to support comma-separated list of handles
  if (handles) {
    const handleList = handles
      .split(",")
      .map((h) => h.trim().replace(/^@/, ""))
      .filter(Boolean);
    
    matchStage["handleInfo.handle"] = { $in: handleList };
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
        atproto: 1,
      },
    },
    { $sort: { when: -1 } },
  ];

  const records = await collection.aggregate(pipeline).toArray();
  return records;
}


export { connect, ObjectId, moodFor, allMoods };

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

async function listAndSaveMedia(mediaType) {
  if (!["painting", "piece"].includes(mediaType)) {
    throw new Error(
      'Invalid media type provided. Expected "painting" or "piece"',
    );
  }

  const s3User = new S3Client({
    endpoint: "https://" + process.env.USER_ENDPOINT,
    credentials: {
      accessKeyId: process.env.ART_KEY,
      secretAccessKey: process.env.ART_SECRET,
    },
  });

  const { db, disconnect } = await connect();
  const collection = db.collection(mediaType + "s"); // Adjust collection name based on media type
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

      // Check if the media type subdirectory exists
      if (subDirectories.includes(dir + mediaType + "/")) {
        const files = await listFiles(
          { s3: s3User, bucket: process.env.USER_SPACE_NAME },
          dir + mediaType + "/",
        );

        for (const file of files) {
          const extension = mediaType === "painting" ? ".png" : ".mjs";
          // TODO: Eventually add other media types...

          if (file.endsWith(extension)) {
            const slug = file.split("/").pop().replace(extension, "");
            const user = dir.split("/")[0];
            const existingRecord = await collection.findOne({ slug, user });
            if (!existingRecord) {
              const record = {
                slug,
                user,
                when: new Date(),
              };
              await collection.insertOne(record);
              console.log(`✅ Added ${mediaType} entry for:`, slug);
            } else {
              console.log(`⚠️ ${mediaType} already exists for:`, slug);
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

export { listAndSaveMedia };
