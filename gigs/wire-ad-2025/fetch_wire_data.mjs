import { connect } from "./system/backend/database.mjs";
import fs from "fs";

async function main() {
  console.log("🔌 Connecting to database...");
  const { db, disconnect } = await connect();

  try {
    const data = {
      chats: [],
      handles: [],
      moods: [],
      paintings: []
    };

    // 1. Fetch Handles (Map user sub -> handle)
    console.log("👤 Fetching handles...");
    const handlesCollection = db.collection("@handles");
    const allHandlesDocs = await handlesCollection.find({}).toArray();
    
    const userToHandle = {};
    allHandlesDocs.forEach(doc => {
      if (doc.handle) {
        userToHandle[doc._id] = "@" + doc.handle;
        data.handles.push("@" + doc.handle);
      }
    });
    console.log(`   Found ${data.handles.length} handles.`);

    // Helper to resolve user
    const resolveUser = (userSub) => userToHandle[userSub] || "anon";

    // 2. Fetch Chat Messages (Clock)
    console.log("🕰️ Fetching Clock chat...");
    const clockCollection = db.collection("chat-clock");
    const clockMessages = await clockCollection
      .find({})
      .sort({ when: -1 })
      .limit(50)
      .toArray();
    
    clockMessages.forEach(msg => {
      data.chats.push({
        from: resolveUser(msg.user),
        text: msg.text,
        when: msg.when,
        source: "clock"
      });
    });

    // 3. Fetch Chat Messages (System)
    console.log("💬 Fetching System chat...");
    const systemCollection = db.collection("chat-system");
    const systemMessages = await systemCollection
      .find({})
      .sort({ when: -1 })
      .limit(50)
      .toArray();

    systemMessages.forEach(msg => {
      data.chats.push({
        from: resolveUser(msg.user),
        text: msg.text,
        when: msg.when,
        source: "system"
      });
    });
    console.log(`   Found ${data.chats.length} total chat messages.`);

    // 4. Fetch Moods
    console.log("😊 Fetching Moods...");
    const moodsCollection = db.collection("moods");
    const recentMoods = await moodsCollection
      .find({ deleted: { $ne: true } })
      .sort({ when: -1 })
      .limit(50)
      .toArray();

    recentMoods.forEach(mood => {
      data.moods.push({
        handle: resolveUser(mood.user),
        text: mood.mood,
        when: mood.when
      });
    });
    console.log(`   Found ${data.moods.length} moods.`);

    // 5. Fetch Paintings
    console.log("🎨 Fetching Paintings...");
    const paintingsCollection = db.collection("paintings");
    const recentPaintings = await paintingsCollection
      .find({})
      .sort({ when: -1 })
      .limit(50)
      .toArray();

    recentPaintings.forEach(p => {
      data.paintings.push({
        code: p.slug, // Assuming slug is the code
        user: resolveUser(p.user),
        when: p.when
      });
    });
    console.log(`   Found ${data.paintings.length} paintings.`);

    // Save to file
    fs.writeFileSync("wire_data.json", JSON.stringify(data, null, 2));
    console.log("💾 Saved data to wire_data.json");

  } catch (err) {
    console.error("❌ Error:", err);
  } finally {
    await disconnect();
    console.log("🔌 Disconnected.");
  }
}

main();
