// Shell, 24.09.02.18.46
// Log backend lines to the shell with a filename trace.

export const shell = {
  log(...args) {
    print("log", ...args);
  },

  warn(...args) {
    print("warn", ...args);
  },

  error(...args) {
    print("error", ...args);
  },
};

// function print(type, ...args) {
//   const stack = new Error().stack;
//   const stackLine = stack.split("\n")[3]; // Get the line with the file and line number
//   const fileDetails = stackLine.match(/\/([^\/]+):(\d+):\d+/); // Extract file name and line number

//   if (fileDetails) {
//     const fileName = fileDetails[1];
//     const lineNumber = fileDetails[2];
//     console[type](`🟪 ${fileName}:${lineNumber} -`, ...args);
//   } else {
//     console[type](...args);
//   }
// }
function print(type, ...args) {
  const stack = new Error().stack;
  const stackLine = stack.split("\n")[3]; // Get the line with the file and line number
  const fileDetails = stackLine.match(/\/([^\/]+):(\d+):(\d+)/); // Extract file name, line number, and column number
  const fullPathMatch = stackLine.match(/\((.*):(\d+):\d+\)/); // Extract full path for the file URL

  if (fileDetails && fullPathMatch) {
    const fullPath = fullPathMatch[1];
    const lineNumber = fileDetails[2];
    const fileUrl = `${fileDetails[1]}:${lineNumber}`;
    console[type](`🟪 ${fileUrl} -`, ...args);
  } else {
    console[type](...args);
  }
}
