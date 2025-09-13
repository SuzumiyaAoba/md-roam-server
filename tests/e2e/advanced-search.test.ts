import { beforeEach, describe, expect, it } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe("Advanced Search API E2E Tests", () => {
  beforeEach(async () => {
    await TestCleanup.cleanupNodes();
  });

  describe("POST /search/fuzzy", () => {
    it("should perform fuzzy search with Levenshtein distance", async () => {
      // Create test nodes with variations of a keyword
      await TestCleanup.createTestNode({
        title: "Machine Learning Basics",
        content: "This document explains machine learning concepts.",
        tags: ["ai", "learning"],
      });

      await TestCleanup.createTestNode({
        title: "Maschine Learning Advanced", // Intentional typo
        content: "Advanced topics in machine learning algorithms.",
        tags: ["ai", "advanced"],
      });

      const response = await ApiHelpers.api.post("/search/fuzzy").send({
        query: "Machine Learning",
        threshold: 0.6,
        maxResults: 10,
        fields: ["title", "content"],
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("results");
      expect(response.body).toHaveProperty("totalResults");
      expect(response.body).toHaveProperty("query", "Machine Learning");
      expect(response.body).toHaveProperty("threshold", 0.6);
      expect(response.body).toHaveProperty("searchTime");

      const results = response.body.results;
      expect(Array.isArray(results)).toBe(true);

      if (results.length > 0) {
        const result = results[0];
        expect(result).toHaveProperty("id");
        expect(result).toHaveProperty("title");
        expect(result).toHaveProperty("file");
        expect(result).toHaveProperty("score");
        expect(result).toHaveProperty("matches");
        expect(Array.isArray(result.matches)).toBe(true);
      }
    });

    it("should handle Japanese text fuzzy search", async () => {
      await TestCleanup.createTestNode({
        title: "機械学習の基礎",
        content: "機械学習の基本的な概念を説明します。",
        tags: ["AI", "学習"],
      });

      await TestCleanup.createTestNode({
        title: "機会学習の応用", // Intentional similar character
        content: "機械学習の応用事例を紹介します。",
        tags: ["AI", "応用"],
      });

      const response = await ApiHelpers.api.post("/search/fuzzy").send({
        query: "機械学習",
        threshold: 0.7,
        maxResults: 10,
        fields: ["title", "content"],
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("results");
      expect(Array.isArray(response.body.results)).toBe(true);
    });

    it("should validate request parameters", async () => {
      const emptyQueryResponse = await ApiHelpers.api
        .post("/search/fuzzy")
        .send({
          query: "",
        });

      expect(emptyQueryResponse.status).toBe(400);
      expect(emptyQueryResponse.body).toHaveProperty("status", "error");

      const missingQueryResponse = await ApiHelpers.api
        .post("/search/fuzzy")
        .send({
          threshold: 0.5,
        });

      expect(missingQueryResponse.status).toBe(400);
      expect(missingQueryResponse.body).toHaveProperty("status", "error");
    });
  });

  describe("POST /search/phrase", () => {
    it("should perform exact phrase search", async () => {
      await TestCleanup.createTestNode({
        title: "Deep Learning Tutorial",
        content: "This is a complete guide to deep learning techniques.",
        tags: ["tutorial", "deep-learning"],
      });

      await TestCleanup.createTestNode({
        title: "Machine Learning Guide",
        content: "Learn about machine learning and deep learning applications.",
        tags: ["guide", "ml"],
      });

      const response = await ApiHelpers.api.post("/search/phrase").send({
        phrase: "deep learning",
        caseSensitive: false,
        fields: ["title", "content"],
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("results");
      expect(response.body).toHaveProperty("phrase", "deep learning");

      const results = response.body.results;
      expect(Array.isArray(results)).toBe(true);

      if (results.length > 0) {
        const result = results[0];
        expect(result).toHaveProperty("matches");
        expect(Array.isArray(result.matches)).toBe(true);

        if (result.matches.length > 0) {
          const match = result.matches[0];
          expect(match).toHaveProperty("field");
          expect(match).toHaveProperty("snippet");
          expect(match).toHaveProperty("position");
        }
      }
    });

    it("should handle case-sensitive phrase search", async () => {
      await TestCleanup.createTestNode({
        title: "AI and AI Applications",
        content:
          "Artificial Intelligence (AI) and ai applications in business.",
        tags: ["AI", "business"],
      });

      const caseSensitiveResponse = await ApiHelpers.api
        .post("/search/phrase")
        .send({
          phrase: "AI",
          caseSensitive: true,
          fields: ["title", "content"],
          maxResults: 10,
        });

      expect(caseSensitiveResponse.status).toBe(200);
      expect(caseSensitiveResponse.body).toHaveProperty("status", "success");

      const caseInsensitiveResponse = await ApiHelpers.api
        .post("/search/phrase")
        .send({
          phrase: "AI",
          caseSensitive: false,
          fields: ["title", "content"],
          maxResults: 10,
        });

      expect(caseInsensitiveResponse.status).toBe(200);
      expect(caseInsensitiveResponse.body).toHaveProperty("status", "success");
    });
  });

  describe("POST /search/field", () => {
    it("should search within specific fields", async () => {
      await TestCleanup.createTestNode({
        title: "Python Programming",
        content: "Learn JavaScript and other programming languages.",
        tags: ["python", "programming"],
        category: "tutorial",
      });

      // Search in title field
      const titleResponse = await ApiHelpers.api.post("/search/field").send({
        query: "python",
        field: "title",
        caseSensitive: false,
        exact: false,
        maxResults: 10,
      });

      expect(titleResponse.status).toBe(200);
      expect(titleResponse.body).toHaveProperty("field", "title");

      // Search in content field (should not match Python in title)
      const contentResponse = await ApiHelpers.api.post("/search/field").send({
        query: "python",
        field: "content",
        caseSensitive: false,
        exact: false,
        maxResults: 10,
      });

      expect(contentResponse.status).toBe(200);
      expect(contentResponse.body).toHaveProperty("field", "content");

      // Search in tags field
      const tagsResponse = await ApiHelpers.api.post("/search/field").send({
        query: "python",
        field: "tags",
        caseSensitive: false,
        exact: false,
        maxResults: 10,
      });

      expect(tagsResponse.status).toBe(200);
      expect(tagsResponse.body).toHaveProperty("field", "tags");
    });

    it("should handle exact field search", async () => {
      await TestCleanup.createTestNode({
        title: "Test",
        content: "This is a test document for testing purposes.",
        tags: ["test", "testing"],
      });

      const exactResponse = await ApiHelpers.api.post("/search/field").send({
        query: "test",
        field: "tags",
        exact: true,
        maxResults: 10,
      });

      expect(exactResponse.status).toBe(200);
      expect(exactResponse.body).toHaveProperty("status", "success");

      const results = exactResponse.body.results;
      if (results.length > 0) {
        expect(results[0]).toHaveProperty("matchType");
      }
    });
  });

  describe("POST /search/suggestions", () => {
    it("should generate search suggestions", async () => {
      await TestCleanup.createTestNode({
        title: "Machine Learning Fundamentals",
        content: "Understanding machine learning algorithms and applications.",
        tags: ["machine-learning", "algorithms"],
      });

      await TestCleanup.createTestNode({
        title: "Data Science Methods",
        content: "Data science techniques for machine learning projects.",
        tags: ["data-science", "methods"],
      });

      const response = await ApiHelpers.api.post("/search/suggestions").send({
        query: "mach",
        field: "title",
        maxSuggestions: 5,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("suggestions");
      expect(response.body).toHaveProperty("query", "mach");
      expect(response.body).toHaveProperty("field", "title");

      const suggestions = response.body.suggestions;
      expect(Array.isArray(suggestions)).toBe(true);

      if (suggestions.length > 0) {
        const suggestion = suggestions[0];
        expect(suggestion).toHaveProperty("text");
        expect(suggestion).toHaveProperty("type");
        expect(suggestion).toHaveProperty("nodeId");
      }
    });

    it("should generate tag suggestions", async () => {
      await TestCleanup.createTestNode({
        title: "Programming Tutorial",
        content: "Learn programming concepts.",
        tags: ["programming", "tutorial", "coding"],
      });

      const response = await ApiHelpers.api.post("/search/suggestions").send({
        query: "prog",
        field: "tags",
        maxSuggestions: 5,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("suggestions");
      expect(Array.isArray(response.body.suggestions)).toBe(true);
    });
  });

  describe("GET /search/suggestions/:query", () => {
    it("should provide simple suggestion endpoint", async () => {
      await TestCleanup.createTestNode({
        title: "React Development",
        content: "Building React applications with modern tools.",
        tags: ["react", "development"],
      });

      const response = await ApiHelpers.api
        .get("/search/suggestions/react")
        .query({ field: "title", limit: "3" });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("suggestions");
      expect(response.body).toHaveProperty("query", "react");
      expect(response.body).toHaveProperty("field", "title");
    });

    it("should reject empty query in suggestions", async () => {
      const response = await ApiHelpers.api.get("/search/suggestions/");

      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty("status", "error");
    });
  });

  describe("POST /search/highlight", () => {
    it("should provide search results with highlights", async () => {
      await TestCleanup.createTestNode({
        title: "TypeScript Advanced Features",
        content:
          "TypeScript provides type safety and modern JavaScript features for large-scale applications.",
        tags: ["typescript", "javascript"],
      });

      const response = await ApiHelpers.api.post("/search/highlight").send({
        query: "typescript",
        caseSensitive: false,
        highlightTag: "mark",
        maxResults: 10,
        snippetLength: 150,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("results");
      expect(response.body).toHaveProperty("query", "typescript");

      const results = response.body.results;
      expect(Array.isArray(results)).toBe(true);

      if (results.length > 0) {
        const result = results[0];
        expect(result).toHaveProperty("snippet");
        expect(result).toHaveProperty("highlights");
        expect(Array.isArray(result.highlights)).toBe(true);

        if (result.highlights.length > 0) {
          const highlight = result.highlights[0];
          expect(highlight).toHaveProperty("field");
          expect(highlight).toHaveProperty("text");
          expect(highlight).toHaveProperty("positions");
          expect(Array.isArray(highlight.positions)).toBe(true);

          if (highlight.positions.length > 0) {
            const position = highlight.positions[0];
            expect(position).toHaveProperty("start");
            expect(position).toHaveProperty("end");
          }
        }
      }
    });

    it("should handle multiple highlight positions", async () => {
      await TestCleanup.createTestNode({
        title: "Java Programming Java Tutorial",
        content:
          "Java is a popular programming language. Java applications run on the JVM.",
        tags: ["java", "programming"],
      });

      const response = await ApiHelpers.api.post("/search/highlight").send({
        query: "java",
        caseSensitive: false,
        maxResults: 10,
        snippetLength: 200,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");

      const results = response.body.results;
      if (results.length > 0 && results[0].highlights.length > 0) {
        const highlight = results[0].highlights[0];
        // Should find multiple positions of "java" in the text
        expect(highlight.positions.length).toBeGreaterThan(0);
      }
    });
  });

  describe("Japanese Language Support", () => {
    it("should handle Japanese fuzzy search with character normalization", async () => {
      await TestCleanup.createTestNode({
        title: "プログラミング入門",
        content:
          "プログラミングの基礎を学びます。コードを書く練習をしましょう。",
        tags: ["プログラミング", "入門"],
      });

      await TestCleanup.createTestNode({
        title: "プログラム設計",
        content: "効率的なプログラム設計の方法論について説明します。",
        tags: ["プログラム", "設計"],
      });

      // Test with slight variation (should match due to normalization)
      const response = await ApiHelpers.api.post("/search/fuzzy").send({
        query: "プログラミング",
        threshold: 0.7,
        fields: ["title", "content", "tags"],
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(Array.isArray(response.body.results)).toBe(true);
    });

    it("should handle Japanese phrase search", async () => {
      await TestCleanup.createTestNode({
        title: "人工知能の応用",
        content:
          "人工知能技術は様々な分野で応用されています。機械学習や深層学習が注目されています。",
        tags: ["人工知能", "応用"],
      });

      const response = await ApiHelpers.api.post("/search/phrase").send({
        phrase: "人工知能",
        caseSensitive: false,
        fields: ["title", "content"],
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");

      const results = response.body.results;
      expect(Array.isArray(results)).toBe(true);
    });

    it("should provide Japanese suggestions", async () => {
      await TestCleanup.createTestNode({
        title: "データサイエンス入門",
        content: "データ分析の基本から応用まで学習します。",
        tags: ["データサイエンス", "分析"],
      });

      const response = await ApiHelpers.api.post("/search/suggestions").send({
        query: "データ",
        field: "title",
        maxSuggestions: 5,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(Array.isArray(response.body.suggestions)).toBe(true);
    });
  });

  describe("Performance and Edge Cases", () => {
    it("should handle large result sets with limits", async () => {
      // Create multiple test nodes
      for (let i = 0; i < 5; i++) {
        await TestCleanup.createTestNode({
          title: `Test Document ${i}`,
          content: `This is test content for document number ${i} with test keywords.`,
          tags: ["test", `doc-${i}`],
        });
      }

      const response = await ApiHelpers.api.post("/search/fuzzy").send({
        query: "test",
        threshold: 0.5,
        maxResults: 3,
        fields: ["title", "content"],
      });

      expect(response.status).toBe(200);
      expect(response.body.results.length).toBeLessThanOrEqual(3);
      expect(response.body).toHaveProperty("totalResults");
      expect(response.body).toHaveProperty("searchTime");
    });

    it("should handle special characters in search queries", async () => {
      await TestCleanup.createTestNode({
        title: "C++ Programming Guide",
        content: "Learn C++ with examples: int main() { return 0; }",
        tags: ["c++", "programming"],
      });

      const response = await ApiHelpers.api.post("/search/phrase").send({
        phrase: "C++",
        fields: ["title", "content"],
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
    });

    it("should handle empty result sets gracefully", async () => {
      const response = await ApiHelpers.api.post("/search/fuzzy").send({
        query: "nonexistentqueryterms",
        threshold: 0.8,
        fields: ["title", "content"],
        maxResults: 10,
      });

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body.results).toEqual([]);
      expect(response.body.totalResults).toBe(0);
    });
  });
});
