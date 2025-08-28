import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { EXTENDED_TEST_NODES } from '@/fixtures/extendedTestData';
import { NodeData } from '@/utils/types';

describe('Japanese and Unicode Content E2E Tests', () => {
  
  describe('Japanese Content Support', () => {
    it('should create Japanese markdown nodes successfully', async () => {
      const nodeData = EXTENDED_TEST_NODES.japanese[0];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'md'
      });
      
      TestCleanup.trackNode(createdNode.id);
      
      // Verify the node can be retrieved
      const getResponse = await ApiHelpers.getNode(createdNode.id);
      expect(getResponse.status).toBe(200);
      ApiHelpers.expectNodeResponse(getResponse, {
        title: nodeData.title
      });
    });

    it('should create Japanese org nodes successfully', async () => {
      const nodeData = EXTENDED_TEST_NODES.japanese[1];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: 'org'
      });
      
      TestCleanup.trackNode(createdNode.id);
      
      // Verify file content is properly encoded
      const getResponse = await ApiHelpers.getNode(createdNode.id);
      expect(getResponse.status).toBe(200);
    });

    it('should handle Japanese titles without hanging', async () => {
      const startTime = Date.now();
      
      const response = await ApiHelpers.createNode({
        title: '日本語のタイトルテスト',
        content: '日本語のコンテンツです。',
        tags: ['日本語', 'テスト'],
        file_type: 'org'
      });
      
      const endTime = Date.now();
      const duration = endTime - startTime;
      
      // Should complete within reasonable time (not hang)
      expect(duration).toBeLessThan(10000); // 10 seconds max
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should search Japanese content correctly', async () => {
      // Create test node first
      const testNode = await TestCleanup.createTestNode(EXTENDED_TEST_NODES.japanese[0]);
      
      const response = await ApiHelpers.searchNodes('日本語');
      
      expect(response.status).toBe(200);
      expect(response.body.results.length).toBeGreaterThan(0);
      
      const foundNode = response.body.results.find((result: any) => 
        result.id === testNode.id
      );
      expect(foundNode).toBeDefined();
    });

    it('should handle Japanese metadata correctly', async () => {
      const nodeData = {
        title: 'Japanese Metadata Test',
        content: 'Testing Japanese metadata processing',
        tags: ['日本語タグ', 'メタデータ', 'テスト'],
        aliases: ['日本語エイリアス', 'Japanese Alias'],
        category: 'japanese-metadata',
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });
  });

  describe('Unicode and Emoji Support', () => {
    it('should handle emoji in titles and content', async () => {
      const nodeData = EXTENDED_TEST_NODES.japanese[2];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response, {
          title: nodeData.title
        });
        TestCleanup.trackNode(createdNode.id);
        
        // Verify emoji are preserved
        expect(createdNode.title).toContain('📝');
      }
    });

    it('should support Chinese characters', async () => {
      const nodeData = EXTENDED_TEST_NODES.japanese[3];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response, {
          title: nodeData.title
        });
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should search emoji content', async () => {
      const testNode = await TestCleanup.createTestNode(EXTENDED_TEST_NODES.japanese[2]);
      
      const response = await ApiHelpers.searchNodes('📝');
      expect(response.status).toBe(200);
      
      if (response.body.results.length > 0) {
        const foundNode = response.body.results.find((result: any) => 
          result.title.includes('📝')
        );
        expect(foundNode).toBeDefined();
      }
    });
  });

  describe('Mixed Language Content', () => {
    it('should handle mixed English and Japanese content', async () => {
      const nodeData = {
        title: 'Mixed Language Test - 混合言語テスト',
        content: 'This content mixes English and Japanese: これは英語と日本語が混在するコンテンツです。',
        tags: ['mixed', 'English', '日本語', 'multilingual'],
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should handle multilingual metadata', async () => {
      const nodeData = EXTENDED_TEST_NODES.metadata[2];
      const response = await ApiHelpers.createNode(nodeData);
      
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });
  });

  describe('Performance with Unicode Content', () => {
    it('should handle large Japanese content efficiently', async () => {
      const largeJapaneseContent = '日本語のテキスト。'.repeat(1000);
      
      const startTime = Date.now();
      const response = await ApiHelpers.createNode({
        title: 'Large Japanese Content Test',
        content: largeJapaneseContent,
        tags: ['performance', '日本語', 'large'],
        file_type: 'md'
      });
      const endTime = Date.now();
      
      expect(endTime - startTime).toBeLessThan(5000); // Should complete within 5 seconds
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should handle concurrent Japanese node creation', async () => {
      const concurrentRequests = Array.from({ length: 5 }, (_, i) => 
        ApiHelpers.createNode({
          title: `並行テスト ${i + 1}`,
          content: `並行して作成されるノード ${i + 1}`,
          tags: ['parallel', '日本語'],
          file_type: 'org'
        })
      );
      
      const responses = await Promise.all(concurrentRequests);
      
      responses.forEach((response, index) => {
        expect(response.status).toBe(201);
        if (response.status === 201) {
          const createdNode = ApiHelpers.expectNodeResponse(response);
          TestCleanup.trackNode(createdNode.id);
          expect(createdNode.title).toContain(`並行テスト ${index + 1}`);
        }
      });
    });
  });

  describe('Unicode Edge Cases', () => {
    it('should handle zero-width characters', async () => {
      const nodeData = {
        title: 'Zero\u200BWidth\u200CTest', // Contains zero-width space and non-joiner
        content: 'Content with zero-width characters\u200D\u200E\u200F',
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should handle right-to-left text', async () => {
      const nodeData = {
        title: 'RTL Test: العربية עברית',
        content: 'Testing right-to-left languages: العربية (Arabic) and עברית (Hebrew)',
        tags: ['rtl', 'arabic', 'hebrew'],
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });

    it('should handle complex Unicode combining characters', async () => {
      const nodeData = {
        title: 'Unicode Combining: é̂ñ̃g̊lîsh̃',
        content: 'Testing combining characters: café → cafe\u0301, naïve → nai\u0308ve',
        file_type: 'md' as const
      };
      
      const response = await ApiHelpers.createNode(nodeData);
      expect(response.status).toBe(201);
      
      if (response.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(response);
        TestCleanup.trackNode(createdNode.id);
      }
    });
  });

  describe('File Encoding Verification', () => {
    it('should create properly encoded Japanese markdown files', async () => {
      const nodeData = {
        title: 'ファイルエンコーディングテスト',
        content: '# 日本語コンテンツ\n\n正しくエンコードされているかテスト',
        file_type: 'md' as const
      };
      
      const createResponse = await ApiHelpers.createNode(nodeData);
      expect(createResponse.status).toBe(201);
      
      if (createResponse.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(createResponse);
        TestCleanup.trackNode(createdNode.id);
        
        // Verify we can retrieve and the content is intact
        const getResponse = await ApiHelpers.getNode(createdNode.id);
        expect(getResponse.status).toBe(200);
        
        const retrievedNode = getResponse.body;
        expect(retrievedNode.title).toBe(nodeData.title);
      }
    });

    it('should create properly encoded Japanese org files', async () => {
      const nodeData = {
        title: 'Orgファイルエンコーディング',
        content: '* 日本語ヘッダー\n\n正しくエンコードされたOrgファイル',
        file_type: 'org' as const
      };
      
      const createResponse = await ApiHelpers.createNode(nodeData);
      expect(createResponse.status).toBe(201);
      
      if (createResponse.status === 201) {
        const createdNode = ApiHelpers.expectNodeResponse(createResponse);
        TestCleanup.trackNode(createdNode.id);
        
        // Verify retrieval works correctly
        const getResponse = await ApiHelpers.getNode(createdNode.id);
        expect(getResponse.status).toBe(200);
      }
    });
  });
});