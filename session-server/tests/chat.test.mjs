// Chat Tests, 25.11.28
// Tests for the ChatManager multi-instance chat system

import { describe, it, expect, beforeAll, afterAll, vi } from "vitest";
import { WebSocket } from "ws";
import { ChatManager, chatInstances } from "../chat-manager.mjs";

describe("ChatManager", () => {
  let chatManager;

  beforeAll(async () => {
    // Create ChatManager without MongoDB for testing
    chatManager = new ChatManager({
      dev: true,
      mongoConnectionString: null, // Explicitly disable MongoDB
    });
    // Don't call init() - it would try to connect to MongoDB
  });

  describe("Instance Configuration", () => {
    it("should have three chat instances configured", () => {
      const instances = Object.keys(chatInstances);
      expect(instances).toContain("chat-system.aesthetic.computer");
      expect(instances).toContain("chat-clock.aesthetic.computer");
      expect(instances).toContain("chat.sotce.net");
    });

    it("should identify chat hosts correctly", () => {
      expect(chatManager.isChatHost("chat-system.aesthetic.computer")).toBe(true);
      expect(chatManager.isChatHost("chat-clock.aesthetic.computer")).toBe(true);
      expect(chatManager.isChatHost("chat.sotce.net")).toBe(true);
      expect(chatManager.isChatHost("session-server.aesthetic.computer")).toBe(false);
      expect(chatManager.isChatHost("random.com")).toBe(false);
    });

    it("should get correct instance for host", () => {
      const instance = chatManager.getInstance("chat-system.aesthetic.computer");
      expect(instance).toBeDefined();
      expect(instance.config.name).toBe("chat-system");
    });
  });

  describe("Message Packing", () => {
    it("should pack messages with type and content", () => {
      const packed = chatManager.pack("test", { hello: "world" }, 1);
      const parsed = JSON.parse(packed);
      expect(parsed.type).toBe("test");
      expect(parsed.id).toBe(1);
      expect(JSON.parse(parsed.content)).toEqual({ hello: "world" });
    });

    it("should handle string content", () => {
      const packed = chatManager.pack("test", "hello", 1);
      const parsed = JSON.parse(packed);
      expect(parsed.content).toBe("hello");
    });
  });

  describe("Chat Status", () => {
    it("should return status for all instances", () => {
      const status = chatManager.getStatus();
      expect(Array.isArray(status)).toBe(true);
      expect(status.length).toBe(3);
      
      const names = status.map(s => s.name);
      expect(names).toContain("chat-system");
      expect(names).toContain("chat-clock");
      expect(names).toContain("chat-sotce");
    });

    it("should return status for specific host", () => {
      const status = chatManager.getStatus("chat-system.aesthetic.computer");
      expect(status).toBeDefined();
      expect(status.name).toBe("chat-system");
      expect(status.connections).toBe(0);
      expect(status.messages).toBe(0);
    });
  });
});

describe("Filter Integration", () => {
  it("should filter profanity", async () => {
    const { filter } = await import("../filter.mjs");
    
    // Clean text should pass through
    expect(filter("hello world")).toBe("hello world");
    
    // Profanity should be filtered (replaced with underscores)
    // Note: actual profanity filtering is tested in the obscenity library
  });
});

describe("Redact Module", () => {
  it("should redact message text", async () => {
    const { redact, unredact } = await import("../redact.mjs");
    
    const msg = { text: "hello world", from: "@user" };
    redact(msg);
    
    expect(msg.text).toBe("_____ _____");
    expect(msg.redactedText).toBe("hello world");
  });

  it("should unredact message text", async () => {
    const { redact, unredact } = await import("../redact.mjs");
    
    const msg = { text: "hello world", from: "@user" };
    redact(msg);
    unredact(msg);
    
    expect(msg.text).toBe("hello world");
  });
});
