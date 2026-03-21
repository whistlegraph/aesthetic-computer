/** @type import('hardhat/config').HardhatUserConfig */
require("@nomicfoundation/hardhat-toolbox");

const { task } = require("hardhat/config");
const { infuraApiKey, etherscanApiKey, mnemonic } = require("./secrets.json");

module.exports = {
  solidity: "0.8.17",
  etherscan: {
    apiKey: etherscanApiKey,
  },
  networks: {
    rinkeby: {
      url: `https://rinkeby.infura.io/v3/${infuraApiKey}`,
      accounts: { mnemonic },
    },
    ropsten: {
      url: `https://ropsten.infura.io/v3/${infuraApiKey}`,
      accounts: { mnemonic },
    },
    mainnet: {
      url: `https://mainnet.infura.io/v3/${infuraApiKey}`,
      accounts: { mnemonic },
      gasPrice: "auto"
    },
  },
};

const name = "AstheticComputerPaintings";

task("acp:balance", "Gets the balance of the active account.").setAction(
  async (taskArgs, hre) => {
    const [account] = await hre.ethers.getSigners();
    console.log(
      "💸 Current balance:",
      (await account.getBalance()).toString()
    );
  }
);

task("acp:deploy", "Deploys the contract.").setAction(
  async (taskArgs, hre) => {
    // 1. Get some information about the deployer.
    const [deployer] = await hre.ethers.getSigners();
    console.log(`🔶 Deploying ${name} from:`, deployer.address);
    console.log(
      "💸 Current balance:",
      (await deployer.getBalance()).toString()
    );

    // 2. Deploy the contract.
    const Contract = await hre.ethers.getContractFactory(name);
    const contract = await Contract.deploy();
    await contract.deployed();
    console.log("💚 Deployed to:", contract.address);
  }
);

task("acp:mint", "Mint a token for the deployed contract.")
  .addParam("from", "Contract address.")
  .addOptionalParam("to", "Receiver address.")
  .setAction(async (taskArgs, hre) => {
    const [defaultToAccount] = await hre.ethers.getSigners();

    const from = taskArgs.from;
    const to = taskArgs.to || defaultToAccount.address;

    const Contract = await ethers.getContractFactory(name);
    const contract = await Contract.attach(from);

    try {
      const mint = await contract.safeMint(to);
      console.log("🎈 Minting one token...");
      console.log("🏹 From:", from);
      console.log("📩 To:", to);
      console.log("---");
      console.log(mint);
      console.log("---");
      const minted = await mint.wait();
      console.log(minted);
      console.log("🌠 Minted!");
    } catch (e) {
      console.log(e);
      console.log("🚨 Failed to mint!");
    }
  });
