import supertest from 'supertest';
import { TEST_CONFIG } from './testSetup';

// API client setup
export const api = supertest(TEST_CONFIG.SERVER_URL);

// Type definitions for API responses
export interface ApiResponse<T = any> {
  status: 'success' | 'error';
  message: string;
  timestamp: string;
  data?: T;
  [key: string]: any;
}

export interface NodeData {
  id: string;
  title: string;
  file: string;
  file_type: 'md' | 'org';
  path: string;
}

export interface CreateNodeRequest {
  title: string;
  content?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
  category?: string;
  file_type?: 'md' | 'org';
}

export interface FileListItem {
  id: string;
  file: string;
  title: string;
  mtime: string;
}

// API helper functions
export class ApiHelpers {
  
  // Node operations
  static async createNode(nodeData: CreateNodeRequest): Promise<supertest.Response> {
    return api
      .post('/nodes')
      .send(nodeData)
      .expect('Content-Type', /json/);
  }

  static async getNode(nodeId: string): Promise<supertest.Response> {
    return api
      .get(`/nodes/${nodeId}`)
      .expect('Content-Type', /json/);
  }

  static async updateNode(nodeId: string, updateData: Partial<CreateNodeRequest>): Promise<supertest.Response> {
    return api
      .put(`/nodes/${nodeId}`)
      .send(updateData)
      .expect('Content-Type', /json/);
  }

  static async deleteNode(nodeId: string): Promise<supertest.Response> {
    return api
      .delete(`/nodes/${nodeId}`)
      .expect('Content-Type', /json/);
  }

  static async getAllNodes(): Promise<supertest.Response> {
    return api
      .get('/nodes')
      .expect('Content-Type', /json/);
  }

  static async getNodeContent(nodeId: string): Promise<supertest.Response> {
    return api
      .get(`/nodes/${nodeId}/content`)
      .expect('Content-Type', /json/);
  }

  // File operations
  static async getFiles(): Promise<supertest.Response> {
    return api
      .get('/files')
      .expect('Content-Type', /json/);
  }

  static async getRawFiles(): Promise<supertest.Response> {
    return api
      .get('/files/raw')
      .expect('Content-Type', /json/);
  }

  // Search operations
  static async searchNodes(query: string): Promise<supertest.Response> {
    return api
      .get(`/search/${encodeURIComponent(query)}`)
      .expect('Content-Type', /json/);
  }

  static async getStats(): Promise<supertest.Response> {
    return api
      .get('/stats')
      .expect('Content-Type', /json/);
  }

  static async getTags(): Promise<supertest.Response> {
    return api
      .get('/tags')
      .expect('Content-Type', /json/);
  }

  // Server health
  static async healthCheck(): Promise<supertest.Response> {
    return api
      .get('/')
      .expect('Content-Type', /json/);
  }

  static async syncDatabase(): Promise<supertest.Response> {
    return api
      .post('/sync')
      .expect('Content-Type', /json/);
  }

  // Utility methods
  static expectSuccessResponse(response: supertest.Response): void {
    expect(response.status).toBeGreaterThanOrEqual(200);
    expect(response.status).toBeLessThan(300);
    expect(response.body).toHaveProperty('status', 'success');
    expect(response.body).toHaveProperty('message');
    expect(response.body).toHaveProperty('timestamp');
  }

  static expectErrorResponse(response: supertest.Response, expectedStatus?: number): void {
    if (expectedStatus) {
      expect(response.status).toBe(expectedStatus);
    } else {
      expect(response.status).toBeGreaterThanOrEqual(400);
    }
    expect(response.body).toHaveProperty('status', 'error');
    expect(response.body).toHaveProperty('message');
  }

  static expectNodeResponse(response: supertest.Response, expectedData?: Partial<NodeData>): NodeData {
    ApiHelpers.expectSuccessResponse(response);
    
    const nodeData = response.body as NodeData;
    expect(nodeData).toHaveProperty('id');
    expect(nodeData).toHaveProperty('title');
    expect(nodeData).toHaveProperty('file');
    expect(nodeData).toHaveProperty('file_type');
    expect(nodeData).toHaveProperty('path');
    
    if (expectedData) {
      Object.entries(expectedData).forEach(([key, value]) => {
        expect(nodeData[key as keyof NodeData]).toBe(value);
      });
    }
    
    return nodeData;
  }

  static generateTestNode(overrides: Partial<CreateNodeRequest> = {}): CreateNodeRequest {
    const timestamp = Date.now();
    return {
      title: `Test Node ${timestamp}`,
      content: `Test content created at ${new Date().toISOString()}`,
      tags: ['test', 'automated'],
      category: 'testing',
      ...overrides,
    };
  }

  static generateUniqueTitle(prefix: string = 'Test'): string {
    return `${prefix} ${Date.now()}-${Math.random().toString(36).substr(2, 5)}`;
  }
}

// Test cleanup utilities
export class TestCleanup {
  private static createdNodeIds: string[] = [];

  static trackNode(nodeId: string): void {
    TestCleanup.createdNodeIds.push(nodeId);
  }

  static async cleanupNodes(): Promise<void> {
    if (TestCleanup.createdNodeIds.length === 0) return;
    
    console.log(`🧹 Cleaning up ${TestCleanup.createdNodeIds.length} test nodes...`);
    
    const cleanupWithRetry = async (nodeId: string, retries = 2): Promise<void> => {
      for (let attempt = 0; attempt <= retries; attempt++) {
        try {
          const response = await ApiHelpers.deleteNode(nodeId);
          if (response.status === 200 || response.status === 404) {
            return; // Success or already deleted
          }
        } catch (error: any) {
          // Check if this is a connection error
          if (error.code === 'ECONNREFUSED' || error.code === 'ETIMEDOUT') {
            if (attempt < retries) {
              console.warn(`Cleanup retry ${attempt + 1}/${retries + 1} for node ${nodeId}`);
              await new Promise(resolve => setTimeout(resolve, 1000 * (attempt + 1))); // Exponential backoff
              continue;
            } else {
              console.warn(`Failed to cleanup node ${nodeId} after ${retries + 1} attempts: Connection issue`);
              return; // Give up gracefully
            }
          }
          
          if (attempt === retries) {
            console.warn(`Failed to cleanup node ${nodeId}:`, error.message || error);
          }
        }
      }
    };

    // Process cleanup in smaller batches to reduce server load
    const batchSize = 5;
    for (let i = 0; i < TestCleanup.createdNodeIds.length; i += batchSize) {
      const batch = TestCleanup.createdNodeIds.slice(i, i + batchSize);
      const batchPromises = batch.map(nodeId => cleanupWithRetry(nodeId));
      await Promise.allSettled(batchPromises);
      
      // Small delay between batches
      if (i + batchSize < TestCleanup.createdNodeIds.length) {
        await new Promise(resolve => setTimeout(resolve, 200));
      }
    }

    TestCleanup.createdNodeIds = [];
  }

  static async createTestNode(nodeData?: Partial<CreateNodeRequest>): Promise<NodeData> {
    const testNodeData = ApiHelpers.generateTestNode(nodeData);
    const response = await ApiHelpers.createNode(testNodeData);
    
    const nodeResponse = ApiHelpers.expectNodeResponse(response);
    TestCleanup.trackNode(nodeResponse.id);
    
    return nodeResponse;
  }
}

// Global cleanup after each test suite
afterEach(async () => {
  await TestCleanup.cleanupNodes();
});