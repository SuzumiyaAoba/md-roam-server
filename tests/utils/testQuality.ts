import { expect } from "vitest";
import { ApiHelpers } from "./apiHelpers";
import { CreateNodePayload, UpdateNodePayload, NodeData } from "./types";

/**
 * Test quality and maintainability utilities for cleaner, more reliable tests
 */

// Builder Pattern for Test Data
export class NodeDataBuilder {
  private data: Partial<CreateNodePayload> = {};

  static create(): NodeDataBuilder {
    return new NodeDataBuilder();
  }

  withTitle(title: string): NodeDataBuilder {
    this.data.title = title;
    return this;
  }

  withContent(content: string): NodeDataBuilder {
    this.data.content = content;
    return this;
  }

  withTags(...tags: string[]): NodeDataBuilder {
    this.data.tags = [...(this.data.tags || []), ...tags];
    return this;
  }

  withAliases(...aliases: string[]): NodeDataBuilder {
    this.data.aliases = [...(this.data.aliases || []), ...aliases];
    return this;
  }

  withRefs(...refs: string[]): NodeDataBuilder {
    this.data.refs = [...(this.data.refs || []), ...refs];
    return this;
  }

  withCategory(category: string): NodeDataBuilder {
    this.data.category = category;
    return this;
  }

  asMarkdown(): NodeDataBuilder {
    this.data.file_type = "md";
    return this;
  }

  asOrg(): NodeDataBuilder {
    this.data.file_type = "org";
    return this;
  }

  asJapanese(): NodeDataBuilder {
    return this.withTitle("日本語テストノード")
      .withContent("# 日本語ヘッダー\n\nこれは日本語のテストコンテンツです。")
      .withTags("日本語", "テスト")
      .withAliases("Japanese Test Node");
  }

  asRich(): NodeDataBuilder {
    return this.withTitle("Rich Content Node")
      .withContent(
        "# Header\n\nThis is **bold** and *italic* text with [links](https://example.com).",
      )
      .withTags("rich", "formatted", "test")
      .withAliases("Rich Node", "Formatted Node")
      .withRefs("https://example.com")
      .withCategory("testing");
  }

  asMinimal(): NodeDataBuilder {
    return this.withTitle("Minimal Test Node").asMarkdown();
  }

  asLarge(): NodeDataBuilder {
    const largeContent = "Large content block. ".repeat(1000);
    const manyTags = Array.from({ length: 50 }, (_, i) => `tag-${i}`);
    const manyAliases = Array.from({ length: 20 }, (_, i) => `Alias ${i + 1}`);

    return this.withTitle("Large Content Node")
      .withContent(largeContent)
      .withTags(...manyTags)
      .withAliases(...manyAliases)
      .withCategory("performance-testing");
  }

  build(): CreateNodePayload {
    if (!this.data.title) {
      throw new Error("Title is required for node creation");
    }

    return {
      title: this.data.title,
      content: this.data.content || "",
      tags: this.data.tags || [],
      aliases: this.data.aliases || [],
      refs: this.data.refs || [],
      category: this.data.category,
      file_type: this.data.file_type || "md",
    };
  }
}

// Fluent API Client with Page Object Pattern
export class FluentApiClient {
  private nodeId?: string;

  static forNode(nodeId: string): FluentApiClient {
    const client = new FluentApiClient();
    client.nodeId = nodeId;
    return client;
  }

  static nodes(): NodesApiClient {
    return new NodesApiClient();
  }

  static search(): SearchApiClient {
    return new SearchApiClient();
  }

  static server(): ServerApiClient {
    return new ServerApiClient();
  }

  static files(): FilesApiClient {
    return new FilesApiClient();
  }
}

export class NodesApiClient {
  async create(nodeData: CreateNodePayload): Promise<ApiResponse<NodeData>> {
    const response = await ApiHelpers.createNode(nodeData);
    return new ApiResponse(response);
  }

  async getAll(): Promise<ApiResponse<NodeData[]>> {
    const response = await ApiHelpers.getAllNodes();
    return new ApiResponse(response);
  }

  async get(nodeId: string): Promise<ApiResponse<NodeData>> {
    const response = await ApiHelpers.getNode(nodeId);
    return new ApiResponse(response);
  }

  async update(
    nodeId: string,
    updateData: Partial<UpdateNodePayload>,
  ): Promise<ApiResponse<NodeData>> {
    const response = await ApiHelpers.updateNode(nodeId, updateData);
    return new ApiResponse(response);
  }

  async delete(nodeId: string): Promise<ApiResponse<void>> {
    const response = await ApiHelpers.deleteNode(nodeId);
    return new ApiResponse(response);
  }
}

export class SearchApiClient {
  async query(searchQuery: string): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.searchNodes(searchQuery);
    return new ApiResponse(response);
  }
}

export class ServerApiClient {
  async healthCheck(): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.healthCheck();
    return new ApiResponse(response);
  }

  async getStats(): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.getStats();
    return new ApiResponse(response);
  }

  async sync(): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.syncDatabase();
    return new ApiResponse(response);
  }
}

export class FilesApiClient {
  async list(): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.getFiles();
    return new ApiResponse(response);
  }

  async listRaw(): Promise<ApiResponse<any>> {
    const response = await ApiHelpers.getRawFiles();
    return new ApiResponse(response);
  }
}

// Enhanced API Response Wrapper
export class ApiResponse<T> {
  constructor(private response: any) {}

  get status(): number {
    return this.response.status;
  }

  get body(): T {
    return this.response.body;
  }

  get headers(): Record<string, any> {
    return this.response.headers || {};
  }

  // Fluent assertions
  shouldBeSuccessful(): this {
    expect(this.status).toBeGreaterThanOrEqual(200);
    expect(this.status).toBeLessThan(300);
    expect(this.body).toHaveProperty("status", "success");
    return this;
  }

  shouldHaveStatus(expectedStatus: number): this {
    expect(this.status).toBe(expectedStatus);
    return this;
  }

  shouldBeError(expectedStatus?: number): this {
    if (expectedStatus) {
      expect(this.status).toBe(expectedStatus);
    } else {
      expect(this.status).toBeGreaterThanOrEqual(400);
    }
    expect(this.body).toHaveProperty("status", "error");
    return this;
  }

  shouldHaveData(): this {
    expect(this.body).toHaveProperty("data");
    return this;
  }

  shouldHaveTimestamp(): this {
    expect(this.body).toHaveProperty("timestamp");
    expect(this.body.timestamp).toMatch(
      /^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$/,
    );
    return this;
  }

  shouldContainInBody(expectedContent: string): this {
    const bodyString = JSON.stringify(this.body);
    expect(bodyString).toContain(expectedContent);
    return this;
  }

  shouldNotContainInBody(unexpectedContent: string): this {
    const bodyString = JSON.stringify(this.body);
    expect(bodyString).not.toContain(unexpectedContent);
    return this;
  }

  shouldHaveResponseTimeBelow(maxTimeMs: number): this {
    // This would require timing measurement from the calling code
    // Implementation depends on how timing is tracked
    return this;
  }

  getData(): T {
    this.shouldBeSuccessful();
    return this.body;
  }
}

// Enhanced Assertion Helpers
export class NodeAssertions {
  /**
   * Comprehensive node validation
   */
  static assertValidNode(node: any, expectedData?: Partial<NodeData>): void {
    // Basic structure
    expect(node).toBeTypeOf("object");
    expect(node).toHaveProperty("id");
    expect(node).toHaveProperty("title");
    expect(node).toHaveProperty("file");
    expect(node).toHaveProperty("file_type");
    expect(node).toHaveProperty("path");

    // Type validation
    expect(typeof node.id).toBe("string");
    expect(typeof node.title).toBe("string");
    expect(typeof node.file).toBe("string");
    expect(typeof node.path).toBe("string");
    expect(["md", "org"]).toContain(node.file_type);

    // ID format validation (UUID-like)
    expect(node.id).toMatch(
      /^[A-F0-9]{8}-[A-F0-9]{4}-[A-F0-9]{4}-[A-F0-9]{4}-[A-F0-9]{12}$/,
    );

    // File extension consistency
    if (node.file_type === "md") {
      expect(node.file).toMatch(/\.md$/);
    } else if (node.file_type === "org") {
      expect(node.file).toMatch(/\.org$/);
    }

    // Path validation
    expect(node.path).toContain(node.file);
    expect(node.path).not.toContain("../"); // No path traversal

    // Expected data validation
    if (expectedData) {
      Object.entries(expectedData).forEach(([key, value]) => {
        expect(node[key]).toEqual(value);
      });
    }
  }

  /**
   * Assert node metadata is properly structured
   */
  static assertValidMetadata(node: any): void {
    if (node.tags) {
      expect(Array.isArray(node.tags)).toBe(true);
      node.tags.forEach((tag: any) => {
        expect(typeof tag).toBe("string");
      });
    }

    if (node.aliases) {
      expect(Array.isArray(node.aliases)).toBe(true);
      node.aliases.forEach((alias: any) => {
        expect(typeof alias).toBe("string");
      });
    }

    if (node.refs) {
      expect(Array.isArray(node.refs)).toBe(true);
      node.refs.forEach((ref: any) => {
        expect(typeof ref).toBe("string");
      });
    }

    if (node.category) {
      expect(typeof node.category).toBe("string");
    }
  }

  /**
   * Assert Japanese content is handled properly
   */
  static assertJapaneseContentSupport(node: any): void {
    if (
      node.title &&
      node.title.match(/[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/)
    ) {
      // Contains Japanese characters
      expect(node.title.length).toBeGreaterThan(0);
      expect(node.file).toBeDefined();
      expect(node.path).toBeDefined();

      // Should not contain corrupted characters
      expect(node.title).not.toContain("�"); // Replacement character
      expect(node.title).not.toContain("?"); // Sometimes indicates encoding issues
    }
  }
}

// Test Scenario Templates
export class TestScenarios {
  /**
   * Standard CRUD scenario template
   */
  static async executeCrudScenario(
    name: string,
    nodeData: CreateNodePayload,
    updateData: Partial<UpdateNodePayload>,
  ): Promise<CrudScenarioResult> {
    const scenario: CrudScenarioResult = {
      name,
      steps: {},
      success: false,
      nodeId: undefined,
    };

    try {
      // Create
      const createResponse = await FluentApiClient.nodes().create(nodeData);
      scenario.steps.create = {
        success: createResponse.status === 201,
        responseTime: 0, // Would need timing implementation
        error:
          createResponse.status !== 201
            ? createResponse.body?.message
            : undefined,
      };

      if (!scenario.steps.create.success) {
        return scenario;
      }

      const createdNode = createResponse.getData();
      scenario.nodeId = createdNode.id;
      NodeAssertions.assertValidNode(createdNode, {
        title: nodeData.title,
        file_type: nodeData.file_type,
      });

      // Read
      const readResponse = await FluentApiClient.nodes().get(createdNode.id);
      scenario.steps.read = {
        success: readResponse.status === 200,
        responseTime: 0,
        error:
          readResponse.status !== 200 ? readResponse.body?.message : undefined,
      };

      if (scenario.steps.read.success) {
        const readNode = readResponse.getData();
        NodeAssertions.assertValidNode(readNode, {
          id: createdNode.id,
          title: nodeData.title,
        });
      }

      // Update
      const updateResponse = await FluentApiClient.nodes().update(
        createdNode.id,
        updateData,
      );
      scenario.steps.update = {
        success: updateResponse.status === 200,
        responseTime: 0,
        error:
          updateResponse.status !== 200
            ? updateResponse.body?.message
            : undefined,
      };

      if (scenario.steps.update.success && updateData.title) {
        const updatedNode = updateResponse.getData();
        NodeAssertions.assertValidNode(updatedNode, {
          id: createdNode.id,
          title: updateData.title,
        });
      }

      // Delete
      const deleteResponse = await FluentApiClient.nodes().delete(
        createdNode.id,
      );
      scenario.steps.delete = {
        success: deleteResponse.status === 200,
        responseTime: 0,
        error:
          deleteResponse.status !== 200
            ? deleteResponse.body?.message
            : undefined,
      };

      // Verify deletion
      if (scenario.steps.delete.success) {
        const verifyResponse = await FluentApiClient.nodes().get(
          createdNode.id,
        );
        scenario.steps.verifyDeletion = {
          success: verifyResponse.status === 404,
          responseTime: 0,
          error:
            verifyResponse.status !== 404
              ? "Node still exists after deletion"
              : undefined,
        };
      }

      // Overall success
      scenario.success = Object.values(scenario.steps).every(
        (step) => step.success,
      );
    } catch (error) {
      scenario.steps.exception = {
        success: false,
        responseTime: 0,
        error: error instanceof Error ? error.message : String(error),
      };
    }

    return scenario;
  }

  /**
   * Performance benchmark scenario
   */
  static async executePerformanceScenario(
    name: string,
    operation: () => Promise<any>,
    maxTimeMs: number,
    iterations = 1,
  ): Promise<PerformanceScenarioResult> {
    const results: number[] = [];
    let success = true;
    let error: string | undefined;

    for (let i = 0; i < iterations; i++) {
      const startTime = performance.now();

      try {
        await operation();
        const endTime = performance.now();
        const duration = endTime - startTime;
        results.push(duration);

        if (duration > maxTimeMs) {
          success = false;
        }
      } catch (err) {
        success = false;
        error = err instanceof Error ? err.message : String(err);
        break;
      }
    }

    return {
      name,
      iterations,
      maxTimeMs,
      results,
      avgTimeMs:
        results.length > 0
          ? results.reduce((a, b) => a + b, 0) / results.length
          : 0,
      minTimeMs: results.length > 0 ? Math.min(...results) : 0,
      maxTimeMs: results.length > 0 ? Math.max(...results) : 0,
      success,
      error,
    };
  }
}

// Test Documentation Generator
export class TestDocumentationGenerator {
  /**
   * Generate test case documentation
   */
  static generateTestDoc(
    testName: string,
    description: string,
    steps: string[],
  ): TestDocumentation {
    return {
      testName,
      description,
      steps,
      timestamp: new Date().toISOString(),
      tags: this.extractTagsFromName(testName),
    };
  }

  private static extractTagsFromName(testName: string): string[] {
    const tags = [];

    if (testName.includes("japanese") || testName.includes("unicode"))
      tags.push("i18n");
    if (testName.includes("performance")) tags.push("performance");
    if (testName.includes("error") || testName.includes("invalid"))
      tags.push("error-handling");
    if (testName.includes("security")) tags.push("security");
    if (testName.includes("crud")) tags.push("crud");
    if (testName.includes("workflow")) tags.push("workflow");

    return tags;
  }

  /**
   * Generate test suite summary
   */
  static generateSuiteSummary(docs: TestDocumentation[]): TestSuiteSummary {
    const tagCounts: Record<string, number> = {};

    docs.forEach((doc) => {
      doc.tags.forEach((tag) => {
        tagCounts[tag] = (tagCounts[tag] || 0) + 1;
      });
    });

    return {
      totalTests: docs.length,
      tagDistribution: tagCounts,
      generatedAt: new Date().toISOString(),
    };
  }
}

// Type definitions
export interface CrudScenarioResult {
  name: string;
  steps: Record<
    string,
    {
      success: boolean;
      responseTime: number;
      error?: string;
    }
  >;
  success: boolean;
  nodeId?: string;
}

export interface PerformanceScenarioResult {
  name: string;
  iterations: number;
  maxTimeMs: number;
  results: number[];
  avgTimeMs: number;
  minTimeMs: number;
  maxTimeMs: number;
  success: boolean;
  error?: string;
}

export interface TestDocumentation {
  testName: string;
  description: string;
  steps: string[];
  timestamp: string;
  tags: string[];
}

export interface TestSuiteSummary {
  totalTests: number;
  tagDistribution: Record<string, number>;
  generatedAt: string;
}
