// Dashboard, 24.03.11.14.58
// A dashboard that could potentially run decoupled from
// `session.mjs`.

let termkit;

// Dynamically import the `terminal-kit` library.
try {
  termkit = (await import("terminal-kit")).default;
} catch (err) {
  error("Failed to load terminal-kit", error);
}

if (termkit) {
  const term = termkit.terminal;

  const doc = term.createDocument({
    palette: new termkit.Palette(),
  });

  // Create left (log) and right (client list) columns
  const leftColumn = new termkit.Container({
    parent: doc,
    x: 0,
    width: "100%",
    height: "100%",
  });

  // const rightColumn = new termkit.Container({
  //   parent: doc,
  //   x: "70%",
  //   width: "30%",
  //   height: "100%",
  // });

  term.grabInput();

  // console.log("grabbed input");

  term.on("key", function (name, matches, data) {
    console.log("'key' event:", name);

    // Detect CTRL-C and exit 'manually'
    if (name === "CTRL_C") {
      process.exit();
    }
  });

  term.on("mouse", function (name, data) {
    console.log("'mouse' event:", name, data);
  });

  // Log box in the left column
  const logBox = new termkit.TextBox({
    parent: leftColumn,
    content: "Your logs will appear here...\n",
    scrollable: true,
    vScrollBar: true,
    x: 0,
    y: 0,
    width: "100%",
    height: "100%",
    mouse: true, // to allow mouse interactions if needed
  });

  // Static list box in the right column
  // const clientList = new termkit.TextBox({
  //   parent: rightColumn,
  //   content: "Client List:\n",
  //   x: 0,
  //   y: 0,
  //   width: "100%",
  //   height: "100%",
  // });

  // Example functions to update contents
  // function addLog(message) {
  //   logBox.setContent(logBox.getContent() + message + "\n");
  //   // logBox.scrollBottom();
  //   doc.draw();
  // }

  // function updateClientList(clients) {
  //   clientList.setContent("Client List:\n" + clients.join("\n"));
  //   doc.draw();
  // }

  // Example usage
  // addLog("Server started...");
  // updateClientList(["Client1", "Client2"]);

  doc.draw();
}
