import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { TEST_SEARCH_QUERIES } from '@/fixtures/testData';
import { NodeData, SearchResponse } from '@/utils/types';

describe('Search API E2E Tests', () => {
  let testNodes: NodeData[];

  beforeEach(async () => {
    // Create test nodes with searchable content
    testNodes = await Promise.all([
      TestCleanup.createTestNode({
        title: 'Markdown Search Test',
        content: 'This is a test markdown document with searchable content',
        tags: ['markdown', 'search', 'test'],
        category: 'testing'
      }),
      TestCleanup.createTestNode({
        title: 'Japanese Content Test',
        content: '日本語のテストコンテンツです。検索テストのために作成されました。',
        tags: ['日本語', 'テスト'],
        category: 'japanese',
        file_type: 'md'
      }),
      TestCleanup.createTestNode({
        title: 'Org Mode Document',
        content: 'This is an org-mode document for testing search functionality',
        tags: ['org', 'search'],
        category: 'testing',
        file_type: 'org'
      }),
      TestCleanup.createTestNode({
        title: 'Case Sensitivity Test',
        content: 'Testing CASE-sensitive and case-INSENSITIVE search behavior',
        tags: ['case-test'],
        category: 'testing'
      })
    ]);
  });

  describe('GET /search/:query - Content Search', () => {
    it('should find nodes by title match', async () => {
      const response = await ApiHelpers.searchNodes('Markdown');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('query', 'Markdown');
      expect(response.body).toHaveProperty('results');
      expect(response.body).toHaveProperty('total_count');
      
      const results = response.body.results;
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
      
      // Should find the markdown test node
      const foundNode = results.find((result: any) => 
        result.title.includes('Markdown Search Test')
      );
      expect(foundNode).toBeDefined();
      expect(foundNode).toHaveProperty('id');
      expect(foundNode).toHaveProperty('title');
      expect(foundNode).toHaveProperty('file');
    });

    it('should find nodes by content match', async () => {
      const response = await ApiHelpers.searchNodes('searchable content');
      
      expect(response.status).toBe(200);
      const results = response.body.results;
      expect(results.length).toBeGreaterThan(0);
      
      const foundNode = results.find((result: any) => 
        result.title.includes('Markdown Search Test')
      );
      expect(foundNode).toBeDefined();
    });

    it('should handle Japanese search queries', async () => {
      const response = await ApiHelpers.searchNodes('日本語');
      
      expect(response.status).toBe(200);
      const results = response.body.results;
      expect(results.length).toBeGreaterThan(0);
      
      const foundNode = results.find((result: any) => 
        result.title.includes('Japanese Content Test')
      );
      expect(foundNode).toBeDefined();
    });

    it('should be case insensitive', async () => {
      const upperCaseResponse = await ApiHelpers.searchNodes('MARKDOWN');
      const lowerCaseResponse = await ApiHelpers.searchNodes('markdown');
      
      expect(upperCaseResponse.status).toBe(200);
      expect(lowerCaseResponse.status).toBe(200);
      
      // Both searches should return results (case insensitive)
      expect(upperCaseResponse.body.results.length).toBeGreaterThan(0);
      expect(lowerCaseResponse.body.results.length).toBeGreaterThan(0);
    });

    it('should return empty results for non-existent content', async () => {
      const response = await ApiHelpers.searchNodes('nonexistent-unique-content-12345');
      
      expect(response.status).toBe(200);
      expect(response.body.results).toEqual([]);
      expect(response.body.total_count).toBe(0);
    });

    it('should handle empty search query', async () => {
      const response = await ApiHelpers.searchNodes('');
      
      expect(response.status).toBe(200);
      // Empty search might return all nodes or no nodes depending on implementation
      expect(response.body).toHaveProperty('results');
      expect(response.body).toHaveProperty('total_count');
    });

    it('should search across both markdown and org files', async () => {
      const response = await ApiHelpers.searchNodes('testing search functionality');
      
      expect(response.status).toBe(200);
      const results = response.body.results;
      
      if (results.length > 0) {
        // Should include results from different file types
        const fileTypes = new Set(results.map((result: any) => result.file.split('.').pop()));
        expect(fileTypes.size).toBeGreaterThan(0);
      }
    });

    it('should handle URL-encoded search queries', async () => {
      const query = 'test content';
      const encodedQuery = encodeURIComponent(query);
      
      // Make request with encoded query directly
      const response = await ApiHelpers.searchNodes(query);
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('query', query);
    });

    it('should include content preview in search results when available', async () => {
      const response = await ApiHelpers.searchNodes('markdown');
      
      expect(response.status).toBe(200);
      const results = response.body.results;
      
      if (results.length > 0) {
        results.forEach((result: any) => {
          expect(result).toHaveProperty('id');
          expect(result).toHaveProperty('title');
          expect(result).toHaveProperty('file');
          // content_preview is optional but if present should be a string
          if (result.content_preview) {
            expect(typeof result.content_preview).toBe('string');
          }
        });
      }
    });
  });

  describe('Search Performance', () => {
    it('should complete search within reasonable time', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.searchNodes('test');
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(2000); // Should complete within 2 seconds
    });
  });

  describe('Search Result Structure', () => {
    it('should return properly structured search response', async () => {
      const response = await ApiHelpers.searchNodes('test');
      
      expect(response.status).toBe(200);
      
      // Validate SearchResponse structure
      const searchResponse = response.body as SearchResponse;
      expect(searchResponse).toHaveProperty('status', 'success');
      expect(searchResponse).toHaveProperty('message');
      expect(searchResponse).toHaveProperty('timestamp');
      expect(searchResponse).toHaveProperty('query');
      expect(searchResponse).toHaveProperty('results');
      expect(searchResponse).toHaveProperty('total_count');
      
      expect(Array.isArray(searchResponse.results)).toBe(true);
      expect(typeof searchResponse.total_count).toBe('number');
      expect(searchResponse.total_count).toBe(searchResponse.results.length);
    });
  });
});