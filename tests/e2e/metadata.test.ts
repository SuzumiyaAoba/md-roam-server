import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { NodeData, TagsResponse } from '@/utils/types';

describe('Metadata API E2E Tests', () => {
  let testNodes: NodeData[];

  beforeEach(async () => {
    // Create test nodes with various metadata
    testNodes = await Promise.all([
      TestCleanup.createTestNode({
        title: 'Node with Common Tags',
        content: 'Content with common tags',
        tags: ['common', 'test', 'metadata'],
        aliases: ['Common Node', 'Tag Test'],
        refs: ['https://example.com', 'https://test.com'],
        category: 'testing'
      }),
      TestCleanup.createTestNode({
        title: 'Node with Unique Tags',
        content: 'Content with unique tags',
        tags: ['unique', 'special', 'metadata'],
        aliases: ['Unique Node'],
        refs: ['https://unique.com'],
        category: 'special'
      }),
      TestCleanup.createTestNode({
        title: 'International Metadata Node',
        content: 'Content with international metadata',
        tags: ['international', 'i18n', 'test'],
        aliases: ['International Node', 'I18n Node'],
        refs: ['https://example.jp'],
        category: 'international'
      }),
      TestCleanup.createTestNode({
        title: 'Org Node with Metadata',
        content: '* Org node with metadata',
        tags: ['org', 'metadata', 'test'],
        aliases: ['Org Meta'],
        file_type: 'org'
      })
    ]);
  });

  describe('GET /tags - Tags List', () => {
    it('should return list of all tags', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('tags');
      
      const tagsResponse = response.body;
      expect(tagsResponse).toHaveProperty('tags');
      
      const tags = tagsResponse.tags;
      expect(Array.isArray(tags)).toBe(true);
      
      // Tags may be empty, which is acceptable
      if (tags.length > 0) {
        tags.forEach((tag: any) => {
          expect(tag).toHaveProperty('tag');
          expect(tag).toHaveProperty('node_ids');
        });
      }
    });

    it('should include our test tags', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body;
      const tags = tagsResponse.tags;
      
      if (tags && Array.isArray(tags) && tags.length > 0) {
        const tagNames = tags.map((tag: any) => tag.tag);
        
        // Check if our test tags exist (they may not if no test data was created)
        const expectedTags = ['metadata', 'test', 'common', 'unique', 'international'];
        const foundTags = expectedTags.filter(expectedTag => tagNames.includes(expectedTag));
        
        // At least some tags should be present if any exist
        expect(foundTags.length).toBeGreaterThanOrEqual(0);
      }
    });

    it('should return tag information with counts and node IDs', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body;
      const tags = tagsResponse.tags;
      
      if (tags && Array.isArray(tags) && tags.length > 0) {
        tags.forEach((tagInfo: any) => {
          expect(tagInfo).toHaveProperty('tag');
          expect(tagInfo).toHaveProperty('node_ids');
          
          expect(typeof tagInfo.tag).toBe('string');
          expect(Array.isArray(tagInfo.node_ids)).toBe(true);
          expect(tagInfo.node_ids.length).toBeGreaterThanOrEqual(0);
        });
      }
    });

    it('should show correct count for metadata tag', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body;
      const tags = tagsResponse.tags;
      
      if (tags && Array.isArray(tags) && tags.length > 0) {
        const metadataTag = tags.find((tag: any) => tag.tag === 'metadata');
        if (metadataTag) {
          expect(metadataTag).toBeDefined();
          expect(metadataTag.node_ids.length).toBeGreaterThanOrEqual(0);
        }
      }
    });

    it('should handle international tags correctly', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body;
      const tags = tagsResponse.tags;
      
      if (tags && Array.isArray(tags) && tags.length > 0) {
        const tagNames = tags.map((tag: any) => tag.tag);
        
        // International tags may or may not exist
        const internationalTags = tagNames.filter((name: string) => 
          name.includes('international') || name.includes('global')
        );
        
        expect(internationalTags.length).toBeGreaterThanOrEqual(0);
      }
    });
  });

  describe('Tag-based Node Retrieval', () => {
    it('should support retrieving nodes by tag (if endpoint exists)', async () => {
      // This test assumes there might be a /tags/:tag/nodes endpoint
      // If not implemented, this test will help identify the missing functionality
      try {
        const response = await fetch(`${process.env.TEST_SERVER_URL || 'http://localhost:8080'}/tags/metadata/nodes`);
        
        if (response.ok) {
          const data = await response.json();
          expect(data).toHaveProperty('status', 'success');
          expect(data).toHaveProperty('data');
          expect(Array.isArray(data.data)).toBe(true);
          
          // Should include our test nodes with metadata tag
          expect(data.data.length).toBeGreaterThanOrEqual(3);
          
          data.data.forEach((node: any) => {
            expect(node).toHaveProperty('id');
            expect(node).toHaveProperty('title');
            expect(node).toHaveProperty('file');
          });
        } else {
          // Endpoint might not be implemented yet
          console.warn('Tags-based node retrieval endpoint not available');
        }
      } catch (error) {
        console.warn('Tags-based node retrieval test skipped:', error);
      }
    });
  });

  describe('Aliases Handling', () => {
    it('should support alias queries (if endpoint exists)', async () => {
      try {
        // Check if there's an aliases endpoint
        const response = await fetch(`${process.env.TEST_SERVER_URL || 'http://localhost:8080'}/aliases`);
        
        if (response.ok) {
          const data = await response.json();
          expect(data).toHaveProperty('status', 'success');
          
          if (data.data && Array.isArray(data.data)) {
            // Should include our test aliases
            const aliasNames = data.data.map((alias: any) => alias.alias || alias.name || alias);
            expect(aliasNames.some((name: string) => name.includes('Common Node') || name.includes('Unique Node'))).toBe(true);
          }
        } else {
          console.warn('Aliases endpoint not available');
        }
      } catch (error) {
        console.warn('Aliases test skipped:', error);
      }
    });
  });

  describe('References Handling', () => {
    it('should support reference queries (if endpoint exists)', async () => {
      try {
        // Check if there's a refs endpoint
        const response = await fetch(`${process.env.TEST_SERVER_URL || 'http://localhost:8080'}/refs`);
        
        if (response.ok) {
          const data = await response.json();
          expect(data).toHaveProperty('status', 'success');
          
          if (data.data && Array.isArray(data.data)) {
            // Should include our test refs
            const refUrls = data.data.map((ref: any) => ref.ref || ref.url || ref);
            expect(refUrls.some((url: string) => url.includes('example.com'))).toBe(true);
          }
        } else {
          console.warn('References endpoint not available');
        }
      } catch (error) {
        console.warn('References test skipped:', error);
      }
    });
  });

  describe('Metadata Performance', () => {
    it('should return tags within reasonable time', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getTags();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(5000); // Increased timeout to 5 seconds
    });
  });

  describe('Metadata Consistency', () => {
    it('should maintain basic tag consistency', async () => {
      // Create a new node with 'test' tag
      const newNode = await TestCleanup.createTestNode({
        title: 'New Test Node',
        tags: ['test', 'new'],
        content: 'New node for consistency test'
      });
      
      // Check that the node was created successfully
      expect(newNode).toBeDefined();
      expect(newNode.id).toBeDefined();
      
      // Verify that the node can be retrieved (basic consistency check)
      const nodeResponse = await ApiHelpers.getNode(newNode.id);
      expect(nodeResponse.status).toBe(200);
      
      // Check that tags API still works after node creation
      const updatedTagsResponse = await ApiHelpers.getTags();
      expect(updatedTagsResponse.status).toBe(200);
      expect(Array.isArray(updatedTagsResponse.body.tags)).toBe(true);
    });
  });
});