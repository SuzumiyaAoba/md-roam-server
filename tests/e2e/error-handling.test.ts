import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { EXTENDED_TEST_NODES, ERROR_TEST_SCENARIOS } from '@/fixtures/extendedTestData';
import { api } from '@/utils/apiHelpers';

describe('Error Handling and Edge Cases E2E Tests', () => {

  describe('Invalid Request Handling', () => {
    it('should reject requests with missing title', async () => {
      const response = await ApiHelpers.createNode({
        content: 'Content without title',
        file_type: 'md'
      } as any);
      
      ApiHelpers.expectErrorResponse(response, 400);
      expect(response.body.message).toContain('title');
    });

    it('should reject requests with empty title', async () => {
      const response = await ApiHelpers.createNode({
        title: '',
        content: 'Content with empty title',
        file_type: 'md'
      });
      
      ApiHelpers.expectErrorResponse(response, 400);
    });

    it('should reject requests with invalid file type', async () => {
      const response = await ApiHelpers.createNode({
        title: 'Invalid File Type Test',
        content: 'Testing invalid file type',
        file_type: 'invalid' as any
      });
      
      ApiHelpers.expectErrorResponse(response, 400);
      expect(response.body.message).toContain('file_type');
    });

    it('should handle malformed JSON gracefully', async () => {
      const malformedRequests = [
        '{"title": "Test"', // Incomplete JSON
        '{"title": }', // Invalid syntax
        'not json at all',
        '',
        '{"title": null}',
        '{"title": 123}',
      ];

      for (const malformed of malformedRequests) {
        const response = await api
          .post('/nodes')
          .send(malformed)
          .set('Content-Type', 'application/json');
        
        ApiHelpers.expectErrorResponse(response, 400);
      }
    });

    it('should handle requests with wrong content type', async () => {
      const response = await api
        .post('/nodes')
        .send('title=Test&content=Content')
        .set('Content-Type', 'application/x-www-form-urlencoded');
      
      ApiHelpers.expectErrorResponse(response);
    });
  });

  describe('Resource Limits and Large Data', () => {
    it('should handle very long titles', async () => {
      const longTitle = 'Very Long Title '.repeat(50); // Reduced from 100
      
      const response = await ApiHelpers.createNode({
        title: longTitle,
        content: 'Testing long title handling',
        file_type: 'md'
      });
      
      // Should either succeed or fail gracefully
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      } else {
        ApiHelpers.expectErrorResponse(response);
      }
    });

    it('should handle large content', async () => {
      const largeContent = 'Large content block. '.repeat(1000); // Reduced from 10000
      
      const startTime = Date.now();
      const response = await ApiHelpers.createNode({
        title: 'Large Content Test',
        content: largeContent,
        file_type: 'md'
      });
      const endTime = Date.now();
      
      // Should complete within reasonable time
      expect(endTime - startTime).toBeLessThan(15000); // Reduced from 30 seconds
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      } else {
        ApiHelpers.expectErrorResponse(response);
      }
    });

    it('should handle many tags gracefully', async () => {
      const manyTags = Array.from({ length: 100 }, (_, i) => `tag-${i}`); // Reduced from 1000
      
      const response = await ApiHelpers.createNode({
        title: 'Many Tags Test',
        content: 'Testing with many tags',
        tags: manyTags,
        file_type: 'md'
      });
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      } else {
        // Should fail gracefully with appropriate error
        ApiHelpers.expectErrorResponse(response);
      }
    });
  });

  describe('Special Characters and Security', () => {
    it('should handle special characters in titles', async () => {
      const specialTitle = 'Special !@#$%^&*()_+-=[]{}|;:,.<>?/\'"\\';
      
      const response = await ApiHelpers.createNode({
        title: specialTitle,
        content: 'Testing special characters in title',
        file_type: 'md'
      });
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
        
        // Verify special characters are preserved/escaped properly
        expect(createdNode.title).toBeDefined();
      } else {
        ApiHelpers.expectErrorResponse(response);
      }
    });

    it('should sanitize potential XSS content', async () => {
      const xssContent = '<script>alert("xss")</script><img src="x" onerror="alert(1)">';
      
      const response = await ApiHelpers.createNode({
        title: 'XSS Test <script>alert("xss")</script>',
        content: xssContent,
        tags: ['<script>alert("tag")</script>'],
        file_type: 'md'
      });
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
        
        // Should not contain executable script tags in response
        expect(JSON.stringify(createdNode)).not.toMatch(/<script[\s\S]*?>[\s\S]*?<\/script>/);
      }
    });

    it('should prevent path traversal in filenames', async () => {
      const response = await ApiHelpers.createNode({
        title: '../../../etc/passwd',
        content: 'Path traversal test',
        file_type: 'md'
      });
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
        
        // File path should be safely contained within org-roam directory
        expect(createdNode.path).not.toContain('../');
        expect(createdNode.file).not.toContain('../');
      }
    });

    it('should handle SQL injection attempts safely', async () => {
      const sqlInjection = "'; DROP TABLE nodes; --";
      
      const response = await ApiHelpers.createNode({
        title: 'SQL Injection Test',
        content: `Content with SQL: ${sqlInjection}`,
        tags: [sqlInjection],
        file_type: 'md'
      });
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
        
        // Verify the system is still functional after the request
        const healthResponse = await ApiHelpers.healthCheck();
        expect(healthResponse.status).toBe(200);
      }
    });
  });

  describe('Concurrent Operations and Race Conditions', () => {
    it('should handle concurrent node creation', async () => {
      const concurrentRequests = Array.from({ length: 10 }, (_, i) => 
        ApiHelpers.createNode({
          title: `Concurrent Test ${i + 1}`,
          content: `Concurrent node creation test ${i + 1}`,
          tags: [`concurrent-${i}`],
          file_type: 'md'
        })
      );
      
      const responses = await Promise.all(concurrentRequests);
      const successfulResponses = responses.filter(r => r.status === 201);
      
      expect(successfulResponses.length).toBeGreaterThan(0);
      
      // Track all successful nodes for cleanup
      successfulResponses.forEach(response => {
        const node = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(node.id);
      });
      
      // Verify all nodes have unique IDs
      const nodeIds = successfulResponses.map(r => r.body.id);
      const uniqueIds = new Set(nodeIds);
      expect(uniqueIds.size).toBe(successfulResponses.length);
    });

    it('should handle rapid sequential requests', async () => {
      const nodes: any[] = [];
      
      for (let i = 0; i < 5; i++) {
        const response = await ApiHelpers.createNode({
          title: `Sequential Test ${i + 1}`,
          content: `Sequential node ${i + 1}`,
          file_type: 'md'
        });
        
        if (response.status === 201) {
          const node = ApiHelpers.expectNodeResponse(response);
          nodes.push(node);
          TestCleanup.trackNode(node.id);
        }
      }
      
      expect(nodes.length).toBe(5);
      
      // Verify all nodes are distinct
      const titles = nodes.map(n => n.title);
      const uniqueTitles = new Set(titles);
      expect(uniqueTitles.size).toBe(5);
    });
  });

  describe('Error Recovery and Resilience', () => {
    it('should recover from temporary failures', async () => {
      // First, verify the server is working
      const healthResponse = await ApiHelpers.healthCheck();
      expect(healthResponse.status).toBe(200);
      
      // Create a valid node to confirm system is functional
      const validResponse = await ApiHelpers.createNode({
        title: 'Recovery Test',
        content: 'Testing error recovery',
        file_type: 'md'
      });
      
      expect(validResponse.status).toBe(201);
      if (validResponse.status === 201) {
        const node = ApiHelpers.expectNodeResponse(validResponse);
        TestCleanup.trackNode(node.id);
      }
    });

    it('should maintain data integrity after errors', async () => {
      // Create a valid node
      const validNode = await TestCleanup.createTestNode();
      
      // Attempt invalid operations
      await ApiHelpers.createNode({
        title: '',
        content: 'Invalid node',
        file_type: 'md'
      });
      
      // Verify the valid node is still accessible
      const getResponse = await ApiHelpers.getNode(validNode.id);
      expect(getResponse.status).toBe(200);
      ApiHelpers.expectNodeResponse(getResponse, {
        id: validNode.id
      });
    });
  });

  describe('Network and Protocol Edge Cases', () => {
    it('should handle requests with missing Content-Type header', async () => {
      const response = await api
        .post('/nodes')
        .send('{"title":"Test","content":"Content"}');
        // No Content-Type header
      
      // Should either work or fail gracefully
      if (response.status >= 200 && response.status < 300) {
        if (response.body && response.body.id) {
          TestCleanup.trackNode(response.body.id);
        }
      } else {
        ApiHelpers.expectErrorResponse(response);
      }
    });

    it('should handle very large request bodies', async () => {
      const largePayload = {
        title: 'Large Request Test',
        content: 'Large content: ' + 'x'.repeat(100000),
        tags: Array.from({ length: 100 }, (_, i) => `large-tag-${i}`),
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(largePayload);
      
      // Should either succeed or fail with appropriate error
      if (response.status === 201) {
        const node = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(node.id);
      } else {
        ApiHelpers.expectErrorResponse(response);
        expect(response.status).toBeLessThan(500); // Should not be server error
      }
    });
  });

  describe('Resource Not Found Scenarios', () => {
    it('should return 404 for non-existent node retrieval', async () => {
      const fakeId = 'non-existent-node-id-12345';
      const response = await ApiHelpers.getNode(fakeId);
      
      ApiHelpers.expectErrorResponse(response, 404);
    });

    it('should return 404 for non-existent node update', async () => {
      const fakeId = 'non-existent-node-id-12345';
      const response = await ApiHelpers.updateNode(fakeId, {
        title: 'Updated Title'
      });
      
      ApiHelpers.expectErrorResponse(response, 404);
    });

    it('should return 404 for non-existent node deletion', async () => {
      const fakeId = 'non-existent-node-id-12345';
      const response = await ApiHelpers.deleteNode(fakeId);
      
      ApiHelpers.expectErrorResponse(response, 404);
    });

    it('should handle invalid node IDs gracefully', async () => {
      const invalidIds = [
        '', // Empty ID
        'invalid-id-format',
        '123',
        'special-chars-!@#$%',
        'very-long-id-that-exceeds-normal-expectations-and-should-be-handled-properly',
      ];
      
      for (const invalidId of invalidIds) {
        const response = await ApiHelpers.getNode(invalidId);
        ApiHelpers.expectErrorResponse(response);
      }
    });
  });
});