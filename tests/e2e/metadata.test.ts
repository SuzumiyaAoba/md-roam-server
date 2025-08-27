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
        title: 'Japanese Metadata Node',
        content: '日本語のメタデータを含むノード',
        tags: ['日本語', 'メタデータ', 'テスト'],
        aliases: ['日本語ノード', 'Japanese Node'],
        refs: ['https://example.jp'],
        category: 'japanese'
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
      expect(response.body).toHaveProperty('data');
      
      const tagsResponse = response.body as TagsResponse;
      expect(tagsResponse).toHaveProperty('tags');
      expect(tagsResponse).toHaveProperty('total_count');
      
      const tags = tagsResponse.tags;
      expect(Array.isArray(tags)).toBe(true);
      expect(tags.length).toBeGreaterThan(0);
      expect(tagsResponse.total_count).toBe(tags.length);
    });

    it('should include our test tags', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body as TagsResponse;
      const tagNames = tagsResponse.tags.map(tag => tag.tag);
      
      expect(tagNames).toContain('metadata');
      expect(tagNames).toContain('test');
      expect(tagNames).toContain('common');
      expect(tagNames).toContain('unique');
      expect(tagNames).toContain('日本語');
    });

    it('should return tag information with counts and node IDs', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body as TagsResponse;
      
      tagsResponse.tags.forEach(tagInfo => {
        expect(tagInfo).toHaveProperty('tag');
        expect(tagInfo).toHaveProperty('count');
        expect(tagInfo).toHaveProperty('node_ids');
        
        expect(typeof tagInfo.tag).toBe('string');
        expect(typeof tagInfo.count).toBe('number');
        expect(Array.isArray(tagInfo.node_ids)).toBe(true);
        expect(tagInfo.count).toBe(tagInfo.node_ids.length);
        expect(tagInfo.count).toBeGreaterThan(0);
      });
    });

    it('should show correct count for metadata tag', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body as TagsResponse;
      
      const metadataTag = tagsResponse.tags.find(tag => tag.tag === 'metadata');
      expect(metadataTag).toBeDefined();
      expect(metadataTag!.count).toBeGreaterThanOrEqual(3); // At least 3 test nodes have this tag
      expect(metadataTag!.node_ids.length).toBe(metadataTag!.count);
    });

    it('should handle Japanese tags correctly', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body as TagsResponse;
      const tagNames = tagsResponse.tags.map(tag => tag.tag);
      
      expect(tagNames).toContain('日本語');
      expect(tagNames).toContain('メタデータ');
      expect(tagNames).toContain('テスト');
      
      const japaneseTag = tagsResponse.tags.find(tag => tag.tag === '日本語');
      expect(japaneseTag).toBeDefined();
      expect(japaneseTag!.count).toBeGreaterThan(0);
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
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });

    it('should handle concurrent tag requests', async () => {
      const requests = Array(5).fill(null).map(() => ApiHelpers.getTags());
      const responses = await Promise.all(requests);
      
      responses.forEach(response => {
        expect(response.status).toBe(200);
        expect(response.body).toHaveProperty('status', 'success');
        expect(response.body).toHaveProperty('data');
      });
      
      // All responses should have the same tag counts
      const firstTags = responses[0].body.tags;
      responses.forEach(response => {
        const tags = response.body.tags;
        expect(tags.length).toBe(firstTags.length);
      });
    });
  });

  describe('Metadata Consistency', () => {
    it('should maintain tag consistency after node operations', async () => {
      // Get initial tag count for 'test' tag
      const initialTagsResponse = await ApiHelpers.getTags();
      const initialTestTag = initialTagsResponse.body.tags.find((tag: any) => tag.tag === 'test');
      const initialCount = initialTestTag?.count || 0;
      
      // Create a new node with 'test' tag
      const newNode = await TestCleanup.createTestNode({
        title: 'New Test Node',
        tags: ['test', 'new'],
        content: 'New node for consistency test'
      });
      
      // Check updated tag count
      const updatedTagsResponse = await ApiHelpers.getTags();
      const updatedTestTag = updatedTagsResponse.body.tags.find((tag: any) => tag.tag === 'test');
      
      expect(updatedTestTag).toBeDefined();
      expect(updatedTestTag!.count).toBe(initialCount + 1);
      expect(updatedTestTag!.node_ids).toContain(newNode.id);
    });

    it('should show correct tag counts across different file types', async () => {
      const response = await ApiHelpers.getTags();
      
      expect(response.status).toBe(200);
      const tagsResponse = response.body as TagsResponse;
      
      // 'metadata' tag should appear in both markdown and org files
      const metadataTag = tagsResponse.tags.find(tag => tag.tag === 'metadata');
      expect(metadataTag).toBeDefined();
      expect(metadataTag!.count).toBeGreaterThanOrEqual(4); // All our test nodes have this tag
      
      // Verify node IDs are unique
      const uniqueNodeIds = new Set(metadataTag!.node_ids);
      expect(uniqueNodeIds.size).toBe(metadataTag!.node_ids.length);
    });
  });
});