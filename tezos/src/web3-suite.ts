/**
 * Tezos Web3 Integration Suite
 * Comprehensive JavaScript/TypeScript API integrations for Tezos ecosystem
 */

import { TezosToolkit } from '@taquito/taquito';
import fetch from 'node-fetch';

export class TezosWeb3Suite {
  private tezos: TezosToolkit;
  private network: 'mainnet' | 'ghostnet';
  private apis: {
    tzkt: string;
    objkt: string;
    betterCallDev: string;
    dipdup: string;
    tezosDomains: string;
  };

  constructor(rpcUrl: string, network: 'mainnet' | 'ghostnet' = 'ghostnet') {
    this.tezos = new TezosToolkit(rpcUrl);
    this.network = network;
    
    this.apis = {
      tzkt: network === 'mainnet' ? 'https://api.tzkt.io' : 'https://api.ghostnet.tzkt.io',
      objkt: 'https://data.objkt.com/v3/graphql',
      betterCallDev: 'https://api.better-call.dev/v1',
      dipdup: 'https://metadata.dipdup.net',
      tezosDomains: 'https://api.tezos.domains/graphql'
    };
  }

  // TzKT API Integration
  async getAccount(address: string) {
    const response = await fetch(`${this.apis.tzkt}/v1/accounts/${address}`);
    return await response.json();
  }

  async getContract(address: string) {
    const response = await fetch(`${this.apis.tzkt}/v1/contracts/${address}`);
    return await response.json();
  }

  async getTokenBalances(address: string, limit: number = 20) {
    const response = await fetch(`${this.apis.tzkt}/v1/tokens/balances?account=${address}&limit=${limit}`);
    return await response.json();
  }

  async getOperations(address: string, limit: number = 10) {
    const response = await fetch(`${this.apis.tzkt}/v1/accounts/${address}/operations?limit=${limit}`);
    return await response.json();
  }

  async searchAccounts(query: string) {
    const response = await fetch(`${this.apis.tzkt}/v1/suggest/accounts/${query}`);
    return await response.json();
  }

  async getBigMapKeys(bigMapId: number, limit: number = 100) {
    const response = await fetch(`${this.apis.tzkt}/v1/bigmaps/${bigMapId}/keys?limit=${limit}`);
    return await response.json();
  }

  async getContractStorage(address: string) {
    const response = await fetch(`${this.apis.tzkt}/v1/contracts/${address}/storage`);
    return await response.json();
  }

  // Better Call Dev API Integration
  async getContractAnalysis(address: string) {
    const response = await fetch(`${this.apis.betterCallDev}/contract/${this.network}/${address}`);
    return await response.json();
  }

  async getContractEntrypoints(address: string) {
    const response = await fetch(`${this.apis.betterCallDev}/contract/${this.network}/${address}/entrypoints`);
    return await response.json();
  }

  async getContractOperations(address: string, limit: number = 50) {
    const response = await fetch(`${this.apis.betterCallDev}/contract/${this.network}/${address}/operations?size=${limit}`);
    return await response.json();
  }

  // Objkt API Integration (GraphQL)
  async graphqlQuery(query: string, variables?: any) {
    const response = await fetch(this.apis.objkt, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query, variables })
    });
    const result = await response.json();
    return result.data;
  }

  async getToken(tokenId: string) {
    const query = `
      query GetToken($tokenId: String!) {
        token(where: {token_id: {_eq: $tokenId}}) {
          token_id
          name
          description
          artifact_uri
          display_uri
          thumbnail_uri
          creators {
            creator_address
          }
          fa2_address
          supply
          royalties
        }
      }
    `;
    return await this.graphqlQuery(query, { tokenId });
  }

  async getCreatorTokens(address: string, limit: number = 20) {
    const query = `
      query GetCreatorTokens($address: String!, $limit: Int!) {
        token(where: {creators: {creator_address: {_eq: $address}}}, limit: $limit) {
          token_id
          name
          artifact_uri
          display_uri
          fa2_address
          supply
        }
      }
    `;
    return await this.graphqlQuery(query, { address, limit });
  }

  async getCollection(contractAddress: string) {
    const query = `
      query GetCollection($address: String!) {
        fa2(where: {contract: {_eq: $address}}) {
          contract
          name
          description
          tokens_aggregate {
            aggregate {
              count
            }
          }
          tokens(limit: 10) {
            token_id
            name
            artifact_uri
          }
        }
      }
    `;
    return await this.graphqlQuery(query, { address: contractAddress });
  }

  // Tezos Domains Integration
  async resolveDomain(domain: string) {
    const query = `
      query ResolveDomain($domain: String!) {
        domain(name: $domain) {
          name
          address
          owner
          expiry
        }
      }
    `;
    const response = await fetch(this.apis.tezosDomains, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query, variables: { domain } })
    });
    const result = await response.json();
    return result.data?.domain;
  }

  async reverseLookup(address: string) {
    const query = `
      query ReverseLookup($address: String!) {
        reverseRecord(address: $address) {
          domain {
            name
          }
        }
      }
    `;
    const response = await fetch(this.apis.tezosDomains, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query, variables: { address } })
    });
    const result = await response.json();
    return result.data?.reverseRecord?.domain?.name;
  }

  // FA2 Analysis Tools
  async analyzeFA2Contract(address: string) {
    const [contract, storage, entrypoints, tokens] = await Promise.all([
      this.getContract(address),
      this.getContractStorage(address),
      this.getContractEntrypoints(address),
      this.getCollection(address)
    ]);

    return {
      contract,
      storage,
      entrypoints,
      collection: tokens?.fa2?.[0],
      isFA2: entrypoints?.some((ep: any) => 
        ['transfer', 'balance_of', 'update_operators'].includes(ep.name)
      )
    };
  }

  // KidLisp Specific Tools
  async findKidLispContracts() {
    // Search for contracts with KidLisp-related metadata
    const contracts = await fetch(`${this.apis.tzkt}/v1/contracts?kind=smart_contract&limit=100`);
    const contractsData = await contracts.json();
    
    // Filter for potential KidLisp contracts
    const kidlispContracts = contractsData.filter((contract: any) => 
      contract.alias?.toLowerCase().includes('kidlisp') ||
      contract.alias?.toLowerCase().includes('meme') ||
      contract.alias?.toLowerCase().includes('aesthetic')
    );

    return kidlispContracts;
  }

  async getNetworkStats() {
    const head = await fetch(`${this.apis.tzkt}/v1/head`);
    const headData = await head.json();
    
    const stats = await fetch(`${this.apis.betterCallDev}/stats/${this.network}`);
    const statsData = await stats.json();

    return {
      network: this.network,
      currentLevel: headData.level,
      timestamp: headData.timestamp,
      contracts: statsData.contracts,
      operations: statsData.operations,
      apis: this.apis
    };
  }

  // Utility Methods
  getTezosToolkit() {
    return this.tezos;
  }

  getAPIs() {
    return this.apis;
  }

  getNetwork() {
    return this.network;
  }
}

// Export a default instance for quick use
export const tezosWeb3 = (network: 'mainnet' | 'ghostnet' = 'ghostnet') => {
  const rpcUrl = network === 'mainnet' 
    ? process.env.TEZOS_RPC_URL_MAINNET || 'https://mainnet.api.tez.ie'
    : process.env.TEZOS_RPC_URL_GHOSTNET || 'https://ghostnet.ecadinfra.com';
  
  return new TezosWeb3Suite(rpcUrl, network);
};

export default TezosWeb3Suite;
