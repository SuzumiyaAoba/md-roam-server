import supertest from "supertest";
import { TEST_CONFIG } from "./testSetup";

// API client setup
export const api = supertest(TEST_CONFIG.SERVER_URL);

// Type definitions for API responses
export interface ApiResponse<T = unknown> {
  status: "success" | "error";
  message: string;
  timestamp: string;
  data?: T;
  [key: string]: unknown;
}

export interface NodeData {
  id: string;
  title: string;
  file: string;
  file_type: "md" | "org";
  path: string;
}

export interface CreateNodeRequest {
  title: string;
  content?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
  category?: string;
  file_type?: "md" | "org";
}

export interface FileListItem {
  id: string;
  file: string;
  title: string;
  mtime: string;
}

// API helper functions - Node operations
export async function createNode(
  nodeData: CreateNodeRequest,
): Promise<supertest.Response> {
  return api.post("/nodes").send(nodeData).expect("Content-Type", /json/);
}

export async function getNode(nodeId: string): Promise<supertest.Response> {
  return api.get(`/nodes/${nodeId}`).expect("Content-Type", /json/);
}

export async function updateNode(
  nodeId: string,
  updateData: Partial<CreateNodeRequest>,
): Promise<supertest.Response> {
  return api
    .put(`/nodes/${nodeId}`)
    .send(updateData)
    .expect("Content-Type", /json/);
}

export async function deleteNode(nodeId: string): Promise<supertest.Response> {
  return api.delete(`/nodes/${nodeId}`).expect("Content-Type", /json/);
}

export async function getAllNodes(): Promise<supertest.Response> {
  return api.get("/nodes").expect("Content-Type", /json/);
}

export async function getNodeContent(
  nodeId: string,
): Promise<supertest.Response> {
  return api.get(`/nodes/${nodeId}/content`).expect("Content-Type", /json/);
}

// File operations
export async function getFiles(): Promise<supertest.Response> {
  return api.get("/files").expect("Content-Type", /json/);
}

export async function getRawFiles(): Promise<supertest.Response> {
  return api.get("/files/raw").expect("Content-Type", /json/);
}

// Search operations
export async function searchNodes(query: string): Promise<supertest.Response> {
  return api
    .get(`/search/${encodeURIComponent(query)}`)
    .expect("Content-Type", /json/);
}

export async function getStats(): Promise<supertest.Response> {
  return api.get("/stats").expect("Content-Type", /json/);
}

export async function getTags(): Promise<supertest.Response> {
  return api.get("/tags").expect("Content-Type", /json/);
}

// Server health
export async function healthCheck(): Promise<supertest.Response> {
  return api.get("/").expect("Content-Type", /json/);
}

export async function syncDatabase(): Promise<supertest.Response> {
  return api.post("/sync").expect("Content-Type", /json/);
}

// Utility methods
export function expectSuccessResponse(response: supertest.Response): void {
  expect(response.status).toBeGreaterThanOrEqual(200);
  expect(response.status).toBeLessThan(300);
  expect(response.body).toHaveProperty("status", "success");
  expect(response.body).toHaveProperty("message");
  expect(response.body).toHaveProperty("timestamp");
}

export function expectErrorResponse(
  response: supertest.Response,
  expectedStatus?: number,
): void {
  if (expectedStatus) {
    expect(response.status).toBe(expectedStatus);
  } else {
    expect(response.status).toBeGreaterThanOrEqual(400);
  }
  expect(response.body).toHaveProperty("status", "error");
  expect(response.body).toHaveProperty("message");
}

export function expectNodeResponse(
  response: supertest.Response,
  expectedData?: Partial<NodeData>,
): NodeData {
  expectSuccessResponse(response);

  const nodeData = response.body as NodeData;
  expect(nodeData).toHaveProperty("id");
  expect(nodeData).toHaveProperty("title");
  expect(nodeData).toHaveProperty("file");
  expect(nodeData).toHaveProperty("file_type");
  expect(nodeData).toHaveProperty("path");

  if (expectedData) {
    Object.entries(expectedData).forEach(([key, value]) => {
      expect(nodeData[key as keyof NodeData]).toBe(value);
    });
  }

  return nodeData;
}

export function generateTestNode(
  overrides: Partial<CreateNodeRequest> = {},
): CreateNodeRequest {
  const timestamp = Date.now();
  return {
    title: `Test Node ${timestamp}`,
    content: `Test content created at ${new Date().toISOString()}`,
    tags: ["test", "automated"],
    category: "testing",
    ...overrides,
  };
}

export function generateUniqueTitle(prefix: string = "Test"): string {
  return `${prefix} ${Date.now()}-${Math.random().toString(36).substr(2, 5)}`;
}

// Legacy compatibility - ApiHelpers class
export const ApiHelpers = {
  createNode,
  getNode,
  updateNode,
  deleteNode,
  getAllNodes,
  getNodeContent,
  getFiles,
  getRawFiles,
  searchNodes,
  getStats,
  getTags,
  healthCheck,
  syncDatabase,
  expectSuccessResponse,
  expectErrorResponse,
  expectNodeResponse,
  generateTestNode,
  generateUniqueTitle,
};

// Test cleanup utilities
let createdNodeIds: string[] = [];

export function trackNode(nodeId: string): void {
  createdNodeIds.push(nodeId);
}

export async function cleanupNodes(): Promise<void> {
  if (createdNodeIds.length === 0) return;

  console.log(`🧹 Cleaning up ${createdNodeIds.length} test nodes...`);

  const cleanupWithRetry = async (
    nodeId: string,
    retries = 2,
  ): Promise<void> => {
    for (let attempt = 0; attempt <= retries; attempt++) {
      try {
        const response = await ApiHelpers.deleteNode(nodeId);
        if (response.status === 200 || response.status === 404) {
          return; // Success or already deleted
        }
      } catch (error: unknown) {
        // Check if this is a connection error
        const errorObj = error as { code?: string; message?: string };
        if (errorObj.code === "ECONNREFUSED" || errorObj.code === "ETIMEDOUT") {
          if (attempt < retries) {
            console.warn(
              `Cleanup retry ${attempt + 1}/${retries + 1} for node ${nodeId}`,
            );
            await new Promise((resolve) =>
              setTimeout(resolve, 1000 * (attempt + 1)),
            ); // Exponential backoff
            continue;
          } else {
            console.warn(
              `Failed to cleanup node ${nodeId} after ${retries + 1} attempts: Connection issue`,
            );
            return; // Give up gracefully
          }
        }

        if (attempt === retries) {
          console.warn(
            `Failed to cleanup node ${nodeId}:`,
            errorObj.message || error,
          );
        }
      }
    }
  };

  // Process cleanup in smaller batches to reduce server load
  const batchSize = 5;
  for (let i = 0; i < createdNodeIds.length; i += batchSize) {
    const batch = createdNodeIds.slice(i, i + batchSize);
    const batchPromises = batch.map((nodeId) => cleanupWithRetry(nodeId));
    await Promise.allSettled(batchPromises);

    // Small delay between batches
    if (i + batchSize < createdNodeIds.length) {
      await new Promise((resolve) => setTimeout(resolve, 200));
    }
  }

  createdNodeIds = [];
}

export async function createTestNode(
  nodeData?: Partial<CreateNodeRequest>,
): Promise<NodeData> {
  const testNodeData = ApiHelpers.generateTestNode(nodeData);
  const response = await ApiHelpers.createNode(testNodeData);

  const nodeResponse = ApiHelpers.expectNodeResponse(response);
  trackNode(nodeResponse.id);

  return nodeResponse;
}

// Legacy compatibility - TestCleanup class
export const TestCleanup = {
  trackNode,
  cleanupNodes,
  createTestNode,
};

// Global cleanup after each test suite
afterEach(async () => {
  await cleanupNodes();
});
