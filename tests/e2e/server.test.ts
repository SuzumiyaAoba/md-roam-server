import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { DatabaseStats, ServerInfo } from '@/utils/types';

describe('Server API E2E Tests', () => {

  describe('GET / - Health Check', () => {
    it('should return server health information', async () => {
      const response = await ApiHelpers.healthCheck();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('message');
      expect(response.body).toHaveProperty('timestamp');
    });

    it('should include server information in health check', async () => {
      const response = await ApiHelpers.healthCheck();
      
      expect(response.status).toBe(200);
      
      // May include server info depending on implementation
      if (response.body.data) {
        const serverInfo = response.body.data as ServerInfo;
        expect(serverInfo).toHaveProperty('version');
        expect(serverInfo).toHaveProperty('server_port');
        expect(serverInfo).toHaveProperty('ui_port');
        expect(serverInfo).toHaveProperty('ui_enabled');
        expect(serverInfo).toHaveProperty('org_roam_directory');
        expect(serverInfo).toHaveProperty('endpoints');
        
        expect(typeof serverInfo.version).toBe('string');
        expect(typeof serverInfo.server_port).toBe('number');
        expect(typeof serverInfo.ui_port).toBe('number');
        expect(typeof serverInfo.ui_enabled).toBe('boolean');
        expect(typeof serverInfo.org_roam_directory).toBe('string');
        expect(Array.isArray(serverInfo.endpoints)).toBe(true);
      }
    });

    it('should respond quickly', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.healthCheck();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(500); // Should respond within 500ms
    });
  });

  describe('GET /stats - Database Statistics', () => {
    beforeEach(async () => {
      // Create some test data to ensure stats are meaningful
      await Promise.all([
        TestCleanup.createTestNode({
          title: 'Stats Test Node 1',
          content: 'Content for statistics testing',
          tags: ['stats', 'test'],
          category: 'testing'
        }),
        TestCleanup.createTestNode({
          title: 'Stats Test Node 2',
          content: 'More content with [[@refs]]',
          tags: ['stats', 'refs'],
          aliases: ['Stats Alias'],
          file_type: 'org'
        })
      ]);
    });

    it('should return database statistics', async () => {
      const response = await ApiHelpers.getStats();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('data');
      
      const stats = response.body.data as DatabaseStats;
      expect(stats).toHaveProperty('total_nodes');
      expect(stats).toHaveProperty('total_files');
      expect(stats).toHaveProperty('total_tags');
      expect(stats).toHaveProperty('total_aliases');
      expect(stats).toHaveProperty('total_refs');
      expect(stats).toHaveProperty('total_citations');
      expect(stats).toHaveProperty('total_links');
      expect(stats).toHaveProperty('database_size');
      expect(stats).toHaveProperty('last_sync');
    });

    it('should return numeric statistics', async () => {
      const response = await ApiHelpers.getStats();
      
      expect(response.status).toBe(200);
      const stats = response.body.data as DatabaseStats;
      
      expect(typeof stats.total_nodes).toBe('number');
      expect(typeof stats.total_files).toBe('number');
      expect(typeof stats.total_tags).toBe('number');
      expect(typeof stats.total_aliases).toBe('number');
      expect(typeof stats.total_refs).toBe('number');
      expect(typeof stats.total_citations).toBe('number');
      expect(typeof stats.total_links).toBe('number');
      
      expect(stats.total_nodes).toBeGreaterThanOrEqual(2); // At least our test nodes
      expect(stats.total_files).toBeGreaterThanOrEqual(2);
      expect(stats.total_tags).toBeGreaterThanOrEqual(2);
    });

    it('should return valid database size and last sync', async () => {
      const response = await ApiHelpers.getStats();
      
      expect(response.status).toBe(200);
      const stats = response.body.data as DatabaseStats;
      
      expect(typeof stats.database_size).toBe('string');
      expect(typeof stats.last_sync).toBe('string');
      
      // Database size should be a reasonable format (e.g., "123.45 KB")
      expect(stats.database_size).toMatch(/\d+(\.\d+)?\s*(B|KB|MB|GB)/);
      
      // Last sync should be a valid ISO timestamp
      const lastSyncDate = new Date(stats.last_sync);
      expect(lastSyncDate.getTime()).not.toBeNaN();
      expect(lastSyncDate.getTime()).toBeGreaterThan(0);
    });

    it('should respond within performance threshold', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getStats();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(500); // Should respond within 500ms
    });

    it('should handle multiple concurrent requests', async () => {
      const requests = Array(5).fill(null).map(() => ApiHelpers.getStats());
      const responses = await Promise.all(requests);
      
      responses.forEach(response => {
        expect(response.status).toBe(200);
        expect(response.body).toHaveProperty('status', 'success');
        expect(response.body).toHaveProperty('data');
      });
      
      // All responses should have the same statistics (or very similar)
      const firstStats = responses[0].body.data;
      responses.forEach(response => {
        const stats = response.body.data;
        expect(stats.total_nodes).toBe(firstStats.total_nodes);
        expect(stats.total_files).toBe(firstStats.total_files);
      });
    });
  });

  describe('POST /sync - Database Synchronization', () => {
    beforeEach(async () => {
      // Create test node that needs syncing
      await TestCleanup.createTestNode({
        title: 'Sync Test Node',
        content: 'This node should be synced to database',
        tags: ['sync-test']
      });
    });

    it('should sync database successfully', async () => {
      const response = await ApiHelpers.syncDatabase();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('message');
      expect(response.body).toHaveProperty('timestamp');
    });

    it('should update last sync time in stats', async () => {
      // Get initial stats
      const initialStatsResponse = await ApiHelpers.getStats();
      const initialLastSync = initialStatsResponse.body.data.last_sync;
      
      // Wait a moment then sync
      await new Promise(resolve => setTimeout(resolve, 1000));
      const syncResponse = await ApiHelpers.syncDatabase();
      expect(syncResponse.status).toBe(200);
      
      // Get updated stats
      const updatedStatsResponse = await ApiHelpers.getStats();
      const updatedLastSync = updatedStatsResponse.body.data.last_sync;
      
      // Last sync should be updated (newer)
      expect(new Date(updatedLastSync).getTime())
        .toBeGreaterThan(new Date(initialLastSync).getTime());
    });

    it('should handle concurrent sync requests', async () => {
      const syncRequests = Array(3).fill(null).map(() => ApiHelpers.syncDatabase());
      const responses = await Promise.all(syncRequests);
      
      // All sync requests should succeed
      responses.forEach(response => {
        expect(response.status).toBe(200);
        expect(response.body).toHaveProperty('status', 'success');
      });
    });

    it('should complete sync within reasonable time', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.syncDatabase();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(5000); // Should complete within 5 seconds
    });
  });

  describe('Server Stability', () => {
    it('should handle rapid sequential requests', async () => {
      const requests = [];
      for (let i = 0; i < 10; i++) {
        requests.push(ApiHelpers.healthCheck());
      }
      
      const responses = await Promise.all(requests);
      
      responses.forEach(response => {
        expect(response.status).toBe(200);
        expect(response.body).toHaveProperty('status', 'success');
      });
    });

    it('should maintain consistent response format', async () => {
      const healthResponse = await ApiHelpers.healthCheck();
      const statsResponse = await ApiHelpers.getStats();
      
      expect(healthResponse.status).toBe(200);
      expect(statsResponse.status).toBe(200);
      
      // Both should follow the base response format
      [healthResponse.body, statsResponse.body].forEach(body => {
        expect(body).toHaveProperty('status');
        expect(body).toHaveProperty('message');
        expect(body).toHaveProperty('timestamp');
        expect(typeof body.status).toBe('string');
        expect(typeof body.message).toBe('string');
        expect(typeof body.timestamp).toBe('string');
      });
    });
  });
});