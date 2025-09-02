import { describe, it, expect, beforeEach } from 'vitest';
import { TestCleanup, ApiHelpers } from '../utils/apiHelpers';

describe('Update Blocking Prevention E2E Tests', () => {

  beforeEach(async () => {
    await TestCleanup.cleanupNodes();
  });

  describe('Post-Update Data Retrieval', () => {
    it('should allow immediate file content retrieval after node update', async () => {
      // Create a test node
      const createResponse = await ApiHelpers.createNode({
        title: 'Blocking Test Node',
        content: 'Initial content for blocking test',
        category: 'test'
      });

      expect(createResponse.status).toBe(201);
      const nodeId = createResponse.body.id;
      TestCleanup.trackNode(nodeId);

      // Update the node
      const updateResponse = await ApiHelpers.updateNode(nodeId, {
        title: 'Updated Blocking Test Node',
        content: 'Updated content after blocking test',
        category: 'updated-test'
      });

      expect(updateResponse.status).toBe(200);
      expect(updateResponse.body.status).toBe('success');
      expect(updateResponse.body.title).toBe('Updated Blocking Test Node');
      
      // Response should indicate database sync was skipped and content preserved
      expect(updateResponse.body.note).toContain('use POST /sync to update database');

      // Immediately try to retrieve the file content (should be updated)
      const contentResponse = await ApiHelpers.getNodeContent(nodeId);
      
      expect(contentResponse.status).toBe(200);
      expect(contentResponse.body.status).toBe('success');
      
      // File content should be immediately updated
      expect(contentResponse.body.content).toContain('Updated content after blocking test');
      expect(contentResponse.body.content).toContain('title: Updated Blocking Test Node');
    }, 30000); // 30 second timeout

    it('should handle multiple rapid updates without blocking', async () => {
      // Create a test node
      const createResponse = await ApiHelpers.createNode({
        title: 'Rapid Update Test Node',
        content: 'Initial content',
        category: 'test'
      });

      expect(createResponse.status).toBe(201);
      const nodeId = createResponse.body.id;
      TestCleanup.trackNode(nodeId);

      // Perform multiple rapid updates
      const updatePromises = [];
      for (let i = 1; i <= 5; i++) {
        updatePromises.push(
          ApiHelpers.updateNode(nodeId, {
            title: `Rapid Update Test Node - Update ${i}`,
            content: `Updated content - iteration ${i}`,
            category: `test-${i}`
          })
        );
      }

      // Wait for all updates to complete
      const updateResponses = await Promise.all(updatePromises);
      
      // All updates should succeed
      updateResponses.forEach((response, index) => {
        expect(response.status).toBe(200);
        expect(response.body.status).toBe('success');
      });

      // Retrieve the final file content (not database state)
      const finalContentResponse = await ApiHelpers.getNodeContent(nodeId);
      expect(finalContentResponse.status).toBe(200);
      
      // The file content should be updated (final state may vary due to concurrency)
      expect(finalContentResponse.body.content).toContain('Rapid Update Test Node');
    }, 60000); // 60 second timeout

    it('should allow other operations during node updates', async () => {
      // Create multiple test nodes
      const node1Response = await ApiHelpers.createNode({
        title: 'Concurrent Test Node 1',
        content: 'Content 1'
      });
      const node2Response = await ApiHelpers.createNode({
        title: 'Concurrent Test Node 2', 
        content: 'Content 2'
      });

      expect(node1Response.status).toBe(201);
      expect(node2Response.status).toBe(201);
      
      const node1Id = node1Response.body.id;
      const node2Id = node2Response.body.id;
      TestCleanup.trackNode(node1Id);
      TestCleanup.trackNode(node2Id);

      // Start an update on node1 and immediately perform other operations
      const updatePromise = ApiHelpers.updateNode(node1Id, {
        title: 'Updated Concurrent Test Node 1',
        content: 'Updated content 1'
      });

      // While update is potentially running, perform other operations
      const [updateResponse, getResponse, listResponse, createResponse] = await Promise.all([
        updatePromise,
        ApiHelpers.getNode(node2Id),
        ApiHelpers.getAllNodes(),
        ApiHelpers.createNode({
          title: 'Concurrent Test Node 3',
          content: 'Content 3'
        })
      ]);

      // All operations should succeed
      expect(updateResponse.status).toBe(200);
      expect(getResponse.status).toBe(200);
      expect(listResponse.status).toBe(200);
      expect(createResponse.status).toBe(201);
      
      TestCleanup.trackNode(createResponse.body.id);
    }, 45000); // 45 second timeout

    it('should handle update errors without blocking server', async () => {
      // Create a test node
      const createResponse = await ApiHelpers.createNode({
        title: 'Error Test Node',
        content: 'Content for error testing'
      });

      expect(createResponse.status).toBe(201);
      const nodeId = createResponse.body.id;
      TestCleanup.trackNode(nodeId);

      // Attempt updates that might cause errors
      const invalidUpdateResponse = await ApiHelpers.updateNode(nodeId, {
        title: '', // Empty title should cause error
        content: 'Updated content'
      });

      // This should return an error but not block the server
      expect(invalidUpdateResponse.status).toBe(400);

      // Server should still be responsive
      const healthCheckResponse = await ApiHelpers.getNode(nodeId);
      expect(healthCheckResponse.status).toBe(200);
      expect(healthCheckResponse.body.title).toBe('Error Test Node'); // Original title preserved
    }, 30000);
  });

  describe('Server Responsiveness', () => {
    it('should respond to health checks during updates', async () => {
      // Create a node for updating
      const createResponse = await ApiHelpers.createNode({
        title: 'Health Check Test Node',
        content: 'Content for health check test'
      });

      expect(createResponse.status).toBe(201);
      const nodeId = createResponse.body.id;
      TestCleanup.trackNode(nodeId);

      // Start an update and immediately check server health
      const updatePromise = ApiHelpers.updateNode(nodeId, {
        title: 'Updated Health Check Test Node',
        content: 'Updated content for health check test'
      });

      // Perform health check while update might be running
      const statsResponse = await ApiHelpers.getStats();
      
      expect(statsResponse.status).toBe(200);
      expect(statsResponse.body.status).toBe('success');

      // Wait for update to complete
      const updateResponse = await updatePromise;
      expect(updateResponse.status).toBe(200);
    }, 30000);
  });
});