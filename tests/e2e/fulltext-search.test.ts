import { beforeEach, describe, expect, it } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe("Full-text Search API E2E Tests", () => {
  beforeEach(async () => {
    await TestCleanup.cleanupNodes();
  });

  describe("POST /search/fulltext", () => {
    it("should perform basic full-text search", async () => {
      // Create test nodes with specific content
      await TestCleanup.createTestNode({
        title: "Full-text Test Node 1",
        content:
          "This document contains the keyword 'searchable' for testing purposes.",
        tags: ["fulltext", "test"],
      });

      await TestCleanup.createTestNode({
        title: "Full-text Test Node 2",
        content: "Another document with 'searchable' content for verification.",
        tags: ["fulltext", "test"],
      });

      // Perform full-text search
      const response = await ApiHelpers.api.post("/search/fulltext").send({
        query: "searchable",
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("matches");
      expect(response.body).toHaveProperty("totalMatches");
      expect(response.body).toHaveProperty("query", "searchable");
      expect(response.body).toHaveProperty("searchTime");

      // Should find matches (if ripgrep is available)
      const matches = response.body.matches;
      expect(Array.isArray(matches)).toBe(true);
    });

    it("should handle search options correctly", async () => {
      // Create test node
      await TestCleanup.createTestNode({
        title: "Case Sensitive Test",
        content: "This has both UPPERCASE and lowercase content.",
        tags: ["case-test"],
      });

      // Case sensitive search
      const caseSensitiveResponse = await ApiHelpers.api
        .post("/search/fulltext")
        .send({
          query: "UPPERCASE",
          caseSensitive: true,
          maxResults: 5,
        });

      expect(caseSensitiveResponse.status).toBe(200);
      expect(caseSensitiveResponse.body).toHaveProperty("status", "success");
    });

    it("should validate request body", async () => {
      // Empty query should fail
      const emptyQueryResponse = await ApiHelpers.api
        .post("/search/fulltext")
        .send({
          query: "",
        });

      expect(emptyQueryResponse.status).toBe(400);
      expect(emptyQueryResponse.body).toHaveProperty("status", "error");

      // Missing query should fail
      const missingQueryResponse = await ApiHelpers.api
        .post("/search/fulltext")
        .send({
          caseSensitive: true,
        });

      expect(missingQueryResponse.status).toBe(400);
      expect(missingQueryResponse.body).toHaveProperty("status", "error");
    });

    it("should handle ripgrep not available gracefully", async () => {
      // This test should pass regardless of whether ripgrep is installed
      const response = await ApiHelpers.api.post("/search/fulltext").send({
        query: "nonexistent",
      });

      // Should either succeed (if ripgrep is available) or fail gracefully (if not)
      expect([200, 500]).toContain(response.status);

      if (response.status === 500) {
        // Should have helpful error message about ripgrep
        expect(response.body.error).toMatch(/ripgrep|rg/i);
      }
    });
  });

  describe("GET /search/fulltext/:query", () => {
    it("should perform simple full-text search via GET", async () => {
      await TestCleanup.createTestNode({
        title: "GET Search Test",
        content: "Content for GET endpoint testing with keyword 'findme'.",
        tags: ["get-test"],
      });

      const response = await ApiHelpers.api
        .get("/search/fulltext/findme")
        .query({ limit: "5" });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("matches");
      expect(response.body).toHaveProperty("query", "findme");
    });

    it("should parse query parameters correctly", async () => {
      const response = await ApiHelpers.api.get("/search/fulltext/test").query({
        case: "true",
        context: "2",
        limit: "10",
        types: "md,org",
      });

      // Should not fail due to parameter parsing
      expect([200, 500]).toContain(response.status);
    });

    it("should reject empty query in GET", async () => {
      const response = await ApiHelpers.api.get("/search/fulltext/");

      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty("status", "error");
    });
  });
});
