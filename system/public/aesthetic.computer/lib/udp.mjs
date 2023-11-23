import Geckos from "../dep/geckos.io-client.2.3.2.min.js";

/* #region 🏁 todo
  - [] Production ICE / TURN Servers.
  - [] Set up room system.
#endregion */

export const UDP = {
  connect: (port = 8889) => {
    const channel = Geckos({ port }); // default port is 9208

    channel.onConnect((error) => {
      if (error) {
        console.error("🩰", error.message);
        return;
      }

      console.log("🩰 Connected to UDP!");

      channel.on("chat message", (data) => {
        console.log(`🩰 You got the message: ${data}`);
      });

      channel.emit("chat message", "a short message sent to the server");
    });
  },
};
