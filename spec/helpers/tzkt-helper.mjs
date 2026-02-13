/**
 * TzKT API Helper
 *
 * Utilities for querying the TzKT indexer API for Tezos blockchain data.
 * Used for read-only verification of contract state without making transactions.
 */

// TzKT API base URLs
const TZKT_API = {
  mainnet: 'https://api.tzkt.io/v1',
  ghostnet: 'https://api.ghostnet.tzkt.io/v1'
};

/**
 * Fetch data from TzKT API with retry logic
 * @param {string} endpoint - API endpoint (e.g., '/contracts/KT1.../storage')
 * @param {string} network - Network name ('mainnet' or 'ghostnet')
 * @param {number} retries - Number of retries on failure
 * @returns {Promise<any>} JSON response
 */
export async function getTzKTData(endpoint, network = 'mainnet', retries = 3) {
  const baseUrl = TZKT_API[network];
  if (!baseUrl) {
    throw new Error(`Unknown network: ${network}`);
  }

  const url = `${baseUrl}${endpoint}`;
  let lastError;

  for (let attempt = 1; attempt <= retries; attempt++) {
    try {
      console.log(`🔍 TzKT API: ${endpoint} (attempt ${attempt}/${retries})`);
      const response = await fetch(url);

      if (!response.ok) {
        // Handle rate limiting with exponential backoff
        if (response.status === 429) {
          const waitTime = Math.pow(2, attempt) * 1000;
          console.warn(`⚠️  Rate limited, waiting ${waitTime}ms before retry...`);
          await sleep(waitTime);
          continue;
        }

        throw new Error(`TzKT API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data;
    } catch (error) {
      lastError = error;
      if (attempt < retries) {
        const waitTime = Math.pow(2, attempt) * 500;
        console.warn(`⚠️  Request failed, waiting ${waitTime}ms before retry...`);
        await sleep(waitTime);
      }
    }
  }

  throw new Error(`TzKT API request failed after ${retries} attempts: ${lastError.message}`);
}

/**
 * Get contract storage
 * @param {string} contractAddress - Contract address (KT1...)
 * @param {string} network - Network name
 * @returns {Promise<object>} Contract storage object
 */
export async function getContractStorage(contractAddress, network = 'mainnet') {
  return getTzKTData(`/contracts/${contractAddress}/storage`, network);
}

/**
 * Get contract entrypoints
 * @param {string} contractAddress - Contract address
 * @param {string} network - Network name
 * @returns {Promise<array>} Array of entrypoint definitions
 */
export async function getContractEntrypoints(contractAddress, network = 'mainnet') {
  return getTzKTData(`/contracts/${contractAddress}/entrypoints`, network);
}

/**
 * Get bigmap value by key
 * @param {number} bigMapId - BigMap ID
 * @param {string} key - Key (hex bytes or string)
 * @param {string} network - Network name
 * @returns {Promise<object>} BigMap entry
 */
export async function getBigMapValue(bigMapId, key, network = 'mainnet') {
  return getTzKTData(`/bigmaps/${bigMapId}/keys/${key}`, network);
}

/**
 * Get all keys from a bigmap (with pagination)
 * @param {number} bigMapId - BigMap ID
 * @param {string} network - Network name
 * @param {number} limit - Max number of keys to fetch
 * @returns {Promise<array>} Array of bigmap entries
 */
export async function getBigMapKeys(bigMapId, network = 'mainnet', limit = 1000) {
  return getTzKTData(`/bigmaps/${bigMapId}/keys?limit=${limit}`, network);
}

/**
 * Get token metadata from TzKT
 * @param {string} contractAddress - Contract address
 * @param {number} tokenId - Token ID
 * @param {string} network - Network name
 * @returns {Promise<array>} Array of token records (usually 1)
 */
export async function getTokenMetadata(contractAddress, tokenId, network = 'mainnet') {
  return getTzKTData(`/tokens?contract=${contractAddress}&tokenId=${tokenId}`, network);
}

/**
 * Get all tokens for a contract
 * @param {string} contractAddress - Contract address
 * @param {string} network - Network name
 * @param {number} limit - Max tokens to fetch
 * @returns {Promise<array>} Array of tokens
 */
export async function getAllTokens(contractAddress, network = 'mainnet', limit = 100) {
  return getTzKTData(`/tokens?contract=${contractAddress}&limit=${limit}`, network);
}

/**
 * Get operations for a contract entrypoint
 * @param {string} contractAddress - Contract address
 * @param {string} entrypoint - Entrypoint name (e.g., 'keep', 'pause')
 * @param {string} network - Network name
 * @param {number} limit - Max operations to fetch
 * @returns {Promise<array>} Array of operations
 */
export async function getOperations(contractAddress, entrypoint, network = 'mainnet', limit = 10) {
  return getTzKTData(
    `/operations/transactions?target=${contractAddress}&entrypoint=${entrypoint}&limit=${limit}`,
    network
  );
}

/**
 * Get all operations for a contract
 * @param {string} contractAddress - Contract address
 * @param {string} network - Network name
 * @param {number} limit - Max operations to fetch
 * @returns {Promise<array>} Array of operations
 */
export async function getAllOperations(contractAddress, network = 'mainnet', limit = 100) {
  return getTzKTData(
    `/operations/transactions?target=${contractAddress}&limit=${limit}`,
    network
  );
}

/**
 * Get token balances for an address
 * @param {string} address - Wallet address
 * @param {string} contractAddress - Contract address (optional filter)
 * @param {string} network - Network name
 * @returns {Promise<array>} Array of balance records
 */
export async function getBalances(address, contractAddress = null, network = 'mainnet') {
  const filter = contractAddress ? `&token.contract=${contractAddress}` : '';
  return getTzKTData(`/tokens/balances?account=${address}${filter}`, network);
}

/**
 * Check if an operation was successful
 * @param {string} operationHash - Operation hash (op...)
 * @param {string} network - Network name
 * @returns {Promise<object>} Operation details
 */
export async function getOperation(operationHash, network = 'mainnet') {
  return getTzKTData(`/operations/${operationHash}`, network);
}

/**
 * Sleep utility for rate limiting
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
