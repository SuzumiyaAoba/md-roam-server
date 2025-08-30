import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { TEST_NODES, TEST_UPDATES } from '@/fixtures/testData';
import { NodeData, CreateNodeRequest, UpdateNodePayload } from '@/utils/types';

describe('Nodes API E2E Tests', () => {
  
  describe('POST /nodes - Node Creation', () => {
    it('should create a basic markdown node', async () => {
      const nodeData = TEST_NODES.markdown[0];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'md'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    it('should create a basic org node', async () => {
      const nodeData = TEST_NODES.org[0];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'org'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    it('should create a rich markdown node with all metadata', async () => {
      const nodeData = TEST_NODES.markdown[1];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'md'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    it('should create a rich org node with all metadata', async () => {
      const nodeData = TEST_NODES.org[1];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'org'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    it('should handle international content correctly', async () => {
      const nodeData = TEST_NODES.markdown[2];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'md'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    it('should default to markdown when file_type is not specified', async () => {
      const nodeData = {
        title: 'Default File Type Test',
        content: 'Testing default file type behavior'
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'md'
      });
      
      TestCleanup.trackNode(createdNode.id);
    });

    describe('Error Cases', () => {
      it('should reject node creation without title', async () => {
        const nodeData = TEST_NODES.invalid[0] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);
        
        ApiHelpers.expectErrorResponse(response, 400);
      });

      it('should reject node creation with empty title', async () => {
        const nodeData = TEST_NODES.invalid[1] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);
        
        ApiHelpers.expectErrorResponse(response, 400);
      });

      it('should reject node creation with invalid file type', async () => {
        const nodeData = TEST_NODES.invalid[2] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);
        
        ApiHelpers.expectErrorResponse(response, 400);
      });
    });
  });

  describe('GET /nodes/:id - Node Retrieval', () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode(TEST_NODES.markdown[1]);
    });

    it('should retrieve existing node by ID', async () => {
      const response = await ApiHelpers.getNode(testNode.id);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: testNode.title,
        file_type: testNode.file_type
      });
    });

    it('should return 404 for non-existent node', async () => {
      const fakeId = 'non-existent-id';
      const response = await ApiHelpers.getNode(fakeId);
      
      ApiHelpers.expectErrorResponse(response, 404);
    });
  });

  describe('PUT /nodes/:id - Node Updates', () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode(TEST_NODES.markdown[1]);
    });

    it('should update node title only', async () => {
      const updateData = TEST_UPDATES.valid[0];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: updateData.title,
        file_type: testNode.file_type
      });
    });

    it('should update node content only', async () => {
      const updateData = TEST_UPDATES.valid[1];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
    });

    it('should update both title and content', async () => {
      const updateData = TEST_UPDATES.valid[2];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: updateData.title,
        file_type: testNode.file_type
      });
    });

    it('should update tags and category', async () => {
      const updateData = TEST_UPDATES.valid[3];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
    });

    describe('Error Cases', () => {
      it('should return 404 for non-existent node', async () => {
        const fakeId = 'non-existent-id';
        const updateData = TEST_UPDATES.valid[0];
        const response = await ApiHelpers.updateNode(fakeId, updateData);
        
        ApiHelpers.expectErrorResponse(response, 404);
      });

      it('should reject update with empty title', async () => {
        const updateData = TEST_UPDATES.invalid[0] as UpdateNodePayload;
        const response = await ApiHelpers.updateNode(testNode.id, updateData);
        
        ApiHelpers.expectErrorResponse(response, 400);
      });

      it('should reject update with invalid file type', async () => {
        const updateData = TEST_UPDATES.invalid[1] as UpdateNodePayload;
        const response = await ApiHelpers.updateNode(testNode.id, updateData);
        
        ApiHelpers.expectErrorResponse(response, 400);
      });
    });
  });

  describe('DELETE /nodes/:id - Node Deletion', () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode();
    });

    it('should delete existing node', async () => {
      const response = await ApiHelpers.deleteNode(testNode.id);
      
      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
      
      // Verify node is actually deleted
      const getResponse = await ApiHelpers.getNode(testNode.id);
      ApiHelpers.expectErrorResponse(getResponse, 404);
    });

    it('should return 404 for non-existent node', async () => {
      const fakeId = 'non-existent-id';
      const response = await ApiHelpers.deleteNode(fakeId);
      
      ApiHelpers.expectErrorResponse(response, 404);
    });
  });

  describe('GET /nodes - All Nodes', () => {
    beforeEach(async () => {
      // Create a few test nodes
      await TestCleanup.createTestNode({ title: 'Test Node 1' });
      await TestCleanup.createTestNode({ title: 'Test Node 2' });
      await TestCleanup.createTestNode({ title: 'Test Node 3', file_type: 'org' });
    });

    it('should retrieve all nodes', async () => {
      const response = await ApiHelpers.getAllNodes();
      
      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
      
      expect(response.body).toHaveProperty('data');
      expect(Array.isArray(response.body.data)).toBe(true);
      expect(response.body.data.length).toBeGreaterThanOrEqual(3);
      
      // Verify node structure
      response.body.data.forEach((node: any) => {
        expect(node).toHaveProperty('id');
        expect(node).toHaveProperty('title');
        expect(node).toHaveProperty('file');
        expect(node).toHaveProperty('file_type');
        expect(node).toHaveProperty('path');
      });
    });
  });
});