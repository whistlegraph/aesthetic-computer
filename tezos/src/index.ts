/**
 * KidLisp - Tezos Integration Library
 * 
 * This library provides TypeScript/JavaScript functions to interact with
 * the KidLisp FA2 contract on Tezos.
 */

import { TezosToolkit } from '@taquito/taquito';
import { BeaconWallet } from '@taquito/beacon-wallet';
import { MichelCodecPacker } from '@taquito/taquito';
import { NetworkType } from '@airgap/beacon-dapp';
import { char2Bytes } from '@taquito/utils';
import crypto from 'crypto';

export interface KidLispTokenMetadata {
  name: string;
  description: string;
  kidlisp_code: string;
  code_hash: string;
  creator: string;
  metadata_uri?: string;
}

export interface MintParams {
  code_hash: string;
  kidlisp_code: string;
  creator: string;
  amount: number;
  metadata_uri?: string;
}

export interface TokenInfo {
  token_id: number;
  creator: string;
  code_hash: string;
  balance: number;
  metadata: KidLispTokenMetadata;
}

export class KidLispClient {
  private tezos: TezosToolkit;
  private contractAddress: string;
  private wallet?: BeaconWallet;

  constructor(rpcUrl: string, contractAddress: string) {
    this.tezos = new TezosToolkit(rpcUrl);
    this.contractAddress = contractAddress;
    
    // Set up packer for better performance
    this.tezos.setPackerProvider(new MichelCodecPacker());
  }

  /**
   * Initialize wallet connection for user interactions
   */
  async initWallet(appName: string = 'KidLisp Meme Coin'): Promise<void> {
    this.wallet = new BeaconWallet({
      name: appName,
      preferredNetwork: NetworkType.MAINNET, // or NetworkType.GHOSTNET for testing
    });

    this.tezos.setWalletProvider(this.wallet);

    // Request permissions (no additional params needed for basic permissions)
    await this.wallet.requestPermissions();
  }

  /**
   * Generate a deterministic Tezos wallet from user handle and sub
   * NOTE: In production, this should use more secure key derivation
   */
  static generateWalletFromHandle(handle: string, userSub: string): string {
    // Create a deterministic seed from handle + userSub
    const seed = crypto.createHash('sha256')
      .update(`${handle}:${userSub}:aesthetic.computer`)
      .digest();
    
    // In production, use proper HD wallet derivation
    // This is a simplified version for demonstration
    const privateKey = seed.toString('hex').substring(0, 64);
    
    // TODO: Convert to proper Tezos private key format
    // TODO: Derive public key and address
    
    return privateKey; // Placeholder
  }

  /**
   * Create SHA-256 hash of KidLisp code for token identification
   */
  static hashKidLispCode(code: string): string {
    return crypto.createHash('sha256')
      .update(code.trim())
      .digest('hex');
  }

  /**
   * Mint a new KidLisp token or add to existing supply
   */
  async mintKidLispToken(params: MintParams): Promise<string> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      
      const operation = await contract.methods.mint_kidlisp_token({
        code_hash: params.code_hash,
        kidlisp_code: params.kidlisp_code,
        creator: params.creator,
        amount: params.amount,
        metadata_uri: params.metadata_uri || `https://aesthetic.computer/kidlisp/metadata/${params.code_hash}`
      }).send({
        amount: 0.1, // Minting fee in tez
      });

      await operation.confirmation();
      return operation.hash;
    } catch (error) {
      console.error('Error minting KidLisp token:', error);
      throw error;
    }
  }

  /**
   * Get token ID for a given KidLisp code hash
   */
  async getTokenForHash(codeHash: string): Promise<number | null> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      const result = await contract.views.get_token_for_hash(codeHash).read();
      
      return result || null;
    } catch (error) {
      console.error('Error getting token for hash:', error);
      return null;
    }
  }

  /**
   * Get creator address for a token
   */
  async getCreator(tokenId: number): Promise<string | null> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      const result = await contract.views.get_creator(tokenId).read();
      
      return result || null;
    } catch (error) {
      console.error('Error getting creator:', error);
      return null;
    }
  }

  /**
   * Get code hash for a token
   */
  async getCodeHash(tokenId: number): Promise<string | null> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      const result = await contract.views.get_code_hash(tokenId).read();
      
      return result || null;
    } catch (error) {
      console.error('Error getting code hash:', error);
      return null;
    }
  }

  /**
   * Get token balance for an address
   */
  async getBalance(owner: string, tokenId: number): Promise<number> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      const result = await contract.views.balance_of([{
        owner,
        token_id: tokenId
      }]).read();
      
      return result[0]?.balance || 0;
    } catch (error) {
      console.error('Error getting balance:', error);
      return 0;
    }
  }

  /**
   * Transfer tokens between addresses
   */
  async transfer(from: string, to: string, tokenId: number, amount: number): Promise<string> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      
      const operation = await contract.methods.transfer([{
        from_: from,
        txs: [{
          to_: to,
          token_id: tokenId,
          amount: amount
        }]
      }]).send();

      await operation.confirmation();
      return operation.hash;
    } catch (error) {
      console.error('Error transferring tokens:', error);
      throw error;
    }
  }

  /**
   * Get complete token information including metadata
   */
  async getTokenInfo(tokenId: number, owner?: string): Promise<TokenInfo | null> {
    try {
      const [creator, codeHash, metadata] = await Promise.all([
        this.getCreator(tokenId),
        this.getCodeHash(tokenId),
        this.getTokenMetadata(tokenId)
      ]);

      if (!creator || !codeHash || !metadata) {
        return null;
      }

      const balance = owner ? await this.getBalance(owner, tokenId) : 0;

      return {
        token_id: tokenId,
        creator,
        code_hash: codeHash,
        balance,
        metadata
      };
    } catch (error) {
      console.error('Error getting token info:', error);
      return null;
    }
  }

  /**
   * Get token metadata from contract storage
   */
  private async getTokenMetadata(tokenId: number): Promise<KidLispTokenMetadata | null> {
    try {
      const contract = await this.tezos.contract.at(this.contractAddress);
      const storage: any = await contract.storage();
      
      const metadata = storage.token_metadata.get(tokenId.toString());
      if (!metadata) return null;

      // Convert bytes to strings
      const convertMetadata = (meta: any): KidLispTokenMetadata => ({
        name: this.bytesToString(meta.name),
        description: this.bytesToString(meta.description),
        kidlisp_code: this.bytesToString(meta.kidlisp_code),
        code_hash: this.bytesToString(meta.code_hash),
        creator: this.bytesToString(meta.creator),
        metadata_uri: meta.metadata_uri ? this.bytesToString(meta.metadata_uri) : undefined
      });

      return convertMetadata(metadata);
    } catch (error) {
      console.error('Error getting token metadata:', error);
      return null;
    }
  }

  /**
   * Convert Tezos bytes to string
   */
  private bytesToString(bytes: string): string {
    try {
      return Buffer.from(bytes, 'hex').toString('utf8');
    } catch {
      return bytes; // Return as-is if conversion fails
    }
  }

  /**
   * Get current wallet address
   */
  async getWalletAddress(): Promise<string | null> {
    try {
      if (!this.wallet) {
        throw new Error('Wallet not initialized');
      }
      
      return await this.wallet.getPKH();
    } catch (error) {
      console.error('Error getting wallet address:', error);
      return null;
    }
  }

  /**
   * Disconnect wallet
   */
  async disconnectWallet(): Promise<void> {
    if (this.wallet) {
      await this.wallet.clearActiveAccount();
    }
  }
}

/**
 * Helper functions for integration with aesthetic.computer
 */
export class AestheticComputerIntegration {
  /**
   * Generate Tezos wallet address for a user handle
   */
  static async generateUserWallet(handle: string, userSub: string): Promise<{
    address: string;
    privateKey: string;
  }> {
    // TODO: Implement proper key derivation
    // This should use BIP39/BIP44 or similar secure methods
    
    const privateKey = KidLispClient.generateWalletFromHandle(handle, userSub);
    
    // TODO: Derive Tezos address from private key
    const address = 'tz1...'; // Placeholder
    
    return { address, privateKey };
  }

  /**
   * Create metadata URI for KidLisp token
   */
  static createMetadataUri(codeHash: string, code: string, creator: string): string {
    const metadata = {
      name: `KidLisp Code #${codeHash.substring(0, 8)}`,
      description: `A unique piece of KidLisp code by ${creator}`,
      image: `https://aesthetic.computer/kidlisp/preview/${codeHash}`,
      animation_url: `https://aesthetic.computer/kidlisp/run/${codeHash}`,
      external_url: `https://aesthetic.computer/$${codeHash}`,
      attributes: [
        {
          trait_type: "Language",
          value: "KidLisp"
        },
        {
          trait_type: "Creator",
          value: creator
        },
        {
          trait_type: "Code Hash",
          value: codeHash
        },
        {
          trait_type: "Code Length",
          value: code.length
        }
      ],
      properties: {
        kidlisp_code: code,
        code_hash: codeHash,
        platform: "aesthetic.computer"
      }
    };

    // In practice, this would be uploaded to IPFS
    return `https://aesthetic.computer/api/kidlisp/metadata/${codeHash}`;
  }

  /**
   * Check if a KidLisp code should generate a meme coin
   */
  static shouldMintToken(code: string): boolean {
    // Add criteria for when to mint tokens
    // For example: minimum code length, complexity, etc.
    return code.trim().length >= 10; // Simple criterion
  }
}

export default KidLispClient;
