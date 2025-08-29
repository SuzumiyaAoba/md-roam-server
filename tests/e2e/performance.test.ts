import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { EXTENDED_TEST_NODES, PERFORMANCE_TEST_SCENARIOS } from '@/fixtures/extendedTestData';
import { NodeData } from '@/utils/types';

describe('Performance E2E Tests', () => {
  const PERFORMANCE_THRESHOLDS = {
    nodeCreation: 5000, // 5 seconds
    nodeRetrieval: 1000, // 1 second  
    nodeUpdate: 3000, // 3 seconds
    nodeDeletion: 2000, // 2 seconds
    search: 2000, // 2 seconds
    bulkOperations: 30000, // 30 seconds
    concurrentOperations: 10000, // 10 seconds
  };

  describe('Individual Operation Performance', () => {
    it('should create nodes within performance threshold', async () => {
      const testCases = [
        { name: 'Simple MD', data: { title: 'Simple MD', content: 'Simple content', file_type: 'md' as const } },
        { name: 'Simple Org', data: { title: 'Simple Org', content: 'Simple content', file_type: 'org' as const } },
        { name: 'Rich MD', data: EXTENDED_TEST_NODES.formatting[0] },
        { name: 'Rich Org', data: EXTENDED_TEST_NODES.formatting[1] },
        { name: 'Rich Text', data: { title: 'Rich Text Performance', content: '# Header\n\nRich **bold** and *italic* text.', tags: ['performance', 'rich'], file_type: 'md' as const } },
      ];

      for (const testCase of testCases) {
        const startTime = Date.now();
        const response = await ApiHelpers.createNode(testCase.data);
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        expect(response.status).toBe(201);
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeCreation);
        
        if (response.status === 201) {
          const node = ApiHelpers.expectNodeResponse(response);
          TestCleanup.trackNode(node.id);
          
          console.log(`${testCase.name} creation: ${duration}ms`);
        }
      }
    });

    it('should retrieve nodes within performance threshold', async () => {
      // Create test nodes first
      const testNodes = await Promise.all([
        TestCleanup.createTestNode({ title: 'Retrieval Test 1', file_type: 'md' }),
        TestCleanup.createTestNode({ title: 'Retrieval Test 2', file_type: 'org' }),
        TestCleanup.createTestNode({ title: 'Performance Test Node', content: 'Performance test content', tags: ['performance'], file_type: 'md' }),
      ]);

      for (const testNode of testNodes) {
        const startTime = Date.now();
        const response = await ApiHelpers.getNode(testNode.id);
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        expect(response.status).toBe(200);
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeRetrieval);
        
        console.log(`Node retrieval: ${duration}ms`);
      }
    });

    it('should update nodes within performance threshold', async () => {
      const testNode = await TestCleanup.createTestNode();
      
      const updateCases = [
        { title: 'Updated Title' },
        { content: 'Updated content' },
        { tags: ['updated', 'performance'] },
        { title: 'Final Update', content: 'Final content', tags: ['final'] },
      ];

      for (const updateData of updateCases) {
        const startTime = Date.now();
        const response = await ApiHelpers.updateNode(testNode.id, updateData);
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        expect(response.status).toBe(200);
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeUpdate);
        
        console.log(`Node update: ${duration}ms`);
      }
    });

    it('should search within performance threshold', async () => {
      // Create searchable test data
      const searchableNodes = await Promise.all([
        TestCleanup.createTestNode({ title: 'Performance Search Test 1', tags: ['performance'] }),
        TestCleanup.createTestNode({ title: 'Performance Search Test 2', tags: ['performance'] }),
        TestCleanup.createTestNode({ title: 'Performance Test Node', content: 'Performance test content', tags: ['performance'], file_type: 'md' }),
      ]);

      const searchQueries = [
        'performance',
        'test',
        'content',
        'search',
      ];

      for (const query of searchQueries) {
        const startTime = Date.now();
        const response = await ApiHelpers.searchNodes(query);
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        expect(response.status).toBe(200);
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.search);
        
        console.log(`Search "${query}": ${duration}ms (${response.body.results.length} results)`);
      }
    });
  });

  describe('Bulk Operations Performance', () => {
    it('should handle bulk node creation efficiently', async () => {
      const bulkSizes = [10, 50, 100];
      
      for (const size of bulkSizes) {
        const bulkData = PERFORMANCE_TEST_SCENARIOS.bulkCreation(size, 'md');
        
        const startTime = Date.now();
        const responses = await Promise.all(
          bulkData.map(nodeData => ApiHelpers.createNode(nodeData))
        );
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        const successfulCreations = responses.filter(r => r.status === 201).length;
        
        expect(successfulCreations).toBeGreaterThan(size * 0.8); // At least 80% success
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.bulkOperations);
        
        // Track successful nodes for cleanup
        responses.forEach(response => {
          if (response.status === 201 && response.body.id) {
            TestCleanup.trackNode(response.body.id);
          }
        });
        
        const avgTimePerNode = duration / successfulCreations;
        console.log(`Bulk creation (${size} nodes): ${duration}ms total, ${avgTimePerNode.toFixed(2)}ms per node`);
      }
    });

    it('should handle bulk retrieval efficiently', async () => {
      // Create test nodes first
      const testNodes = await Promise.all(
        Array.from({ length: 20 }, (_, i) => 
          TestCleanup.createTestNode({ 
            title: `Bulk Retrieval Test ${i + 1}`,
            content: `Content for bulk retrieval test ${i + 1}`
          })
        )
      );

      const startTime = Date.now();
      const responses = await Promise.all(
        testNodes.map(node => ApiHelpers.getNode(node.id))
      );
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const successfulRetrievals = responses.filter(r => r.status === 200).length;
      
      expect(successfulRetrievals).toBe(testNodes.length);
      expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.bulkOperations);
      
      const avgTimePerRetrieval = duration / successfulRetrievals;
      console.log(`Bulk retrieval (${testNodes.length} nodes): ${duration}ms total, ${avgTimePerRetrieval.toFixed(2)}ms per node`);
    });

    it('should handle bulk updates efficiently', async () => {
      // Create test nodes first
      const testNodes = await Promise.all(
        Array.from({ length: 15 }, (_, i) => 
          TestCleanup.createTestNode({ title: `Bulk Update Test ${i + 1}` })
        )
      );

      const startTime = Date.now();
      const responses = await Promise.all(
        testNodes.map((node, i) => 
          ApiHelpers.updateNode(node.id, {
            title: `Updated Bulk Test ${i + 1}`,
            content: `Updated content ${i + 1}`
          })
        )
      );
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const successfulUpdates = responses.filter(r => r.status === 200).length;
      
      expect(successfulUpdates).toBe(testNodes.length);
      expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.bulkOperations);
      
      const avgTimePerUpdate = duration / successfulUpdates;
      console.log(`Bulk update (${testNodes.length} nodes): ${duration}ms total, ${avgTimePerUpdate.toFixed(2)}ms per node`);
    });
  });

  describe('Concurrent Operations Performance', () => {
    it('should handle concurrent node creation', async () => {
      const concurrentCount = 20;
      const nodeData = Array.from({ length: concurrentCount }, (_, i) => ({
        title: `Concurrent Test ${i + 1}`,
        content: `Concurrent creation test ${i + 1}`,
        tags: [`concurrent-${i}`],
        file_type: 'md' as const
      }));

      const startTime = Date.now();
      const responses = await Promise.all(
        nodeData.map(data => ApiHelpers.createNode(data))
      );
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const successfulCreations = responses.filter(r => r.status === 201);
      
      expect(successfulCreations.length).toBeGreaterThan(concurrentCount * 0.8);
      expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.concurrentOperations);
      
      // Track successful nodes for cleanup
      successfulCreations.forEach(response => {
        if (response.body.id) {
          TestCleanup.trackNode(response.body.id);
        }
      });
      
      console.log(`Concurrent creation (${concurrentCount} nodes): ${duration}ms, ${successfulCreations.length} successful`);
    });

    it('should handle mixed concurrent operations', async () => {
      // Create base nodes for updates/retrievals
      const baseNodes = await Promise.all(
        Array.from({ length: 5 }, (_, i) => 
          TestCleanup.createTestNode({ title: `Mixed Op Base ${i + 1}` })
        )
      );

      const operations = [
        // Create operations
        ...Array.from({ length: 10 }, (_, i) => 
          () => ApiHelpers.createNode({
            title: `Mixed Concurrent ${i + 1}`,
            content: `Mixed operation test ${i + 1}`,
            file_type: 'md'
          })
        ),
        // Read operations
        ...baseNodes.map(node => () => ApiHelpers.getNode(node.id)),
        // Update operations
        ...baseNodes.map((node, i) => () => ApiHelpers.updateNode(node.id, {
          title: `Updated Mixed ${i + 1}`
        })),
        // Search operations
        () => ApiHelpers.searchNodes('mixed'),
        () => ApiHelpers.searchNodes('concurrent'),
        () => ApiHelpers.getStats(),
      ];

      const startTime = Date.now();
      const responses = await Promise.all(operations.map(op => op()));
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const successfulOps = responses.filter(r => r.status >= 200 && r.status < 300).length;
      
      expect(successfulOps).toBeGreaterThan(operations.length * 0.8);
      expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.concurrentOperations);
      
      // Track any new nodes created
      responses.forEach(response => {
        if (response.status === 201 && response.body.id) {
          TestCleanup.trackNode(response.body.id);
        }
      });
      
      console.log(`Mixed concurrent ops (${operations.length} operations): ${duration}ms, ${successfulOps} successful`);
    });
  });

  describe('Large Data Performance', () => {
    it('should handle large content efficiently', async () => {
      const contentSizes = [1000, 10000, 100000]; // Characters
      
      for (const size of contentSizes) {
        const largeContent = 'Large content block. '.repeat(Math.ceil(size / 20));
        
        const startTime = Date.now();
        const response = await ApiHelpers.createNode({
          title: `Large Content Test ${size}`,
          content: largeContent.substring(0, size),
          file_type: 'md'
        });
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        if (response.status === 201) {
          const node = ApiHelpers.expectNodeResponse(response);
          TestCleanup.trackNode(node.id);
          
          // Also test retrieval of large content
          const retrievalStart = Date.now();
          const getResponse = await ApiHelpers.getNode(node.id);
          const retrievalEnd = Date.now();
          const retrievalDuration = retrievalEnd - retrievalStart;
          
          expect(getResponse.status).toBe(200);
          expect(retrievalDuration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeRetrieval * 3); // Allow 3x normal time for large content
          
          console.log(`Large content (${size} chars): creation ${duration}ms, retrieval ${retrievalDuration}ms`);
        } else {
          console.log(`Large content (${size} chars) failed: ${response.status} - ${response.body?.message}`);
        }
        
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeCreation * 3); // Allow extra time for large content
      }
    });

    it('should handle many metadata items efficiently', async () => {
      const metadataSizes = [100, 500, 1000];
      
      for (const size of metadataSizes) {
        const manyTags = Array.from({ length: size }, (_, i) => `tag-${i}`);
        const manyAliases = Array.from({ length: Math.floor(size / 2) }, (_, i) => `Alias ${i + 1}`);
        
        const startTime = Date.now();
        const response = await ApiHelpers.createNode({
          title: `Many Metadata Test ${size}`,
          content: 'Testing with large metadata',
          tags: manyTags,
          aliases: manyAliases,
          file_type: 'md'
        });
        const endTime = Date.now();
        
        const duration = endTime - startTime;
        
        if (response.status === 201) {
          const node = ApiHelpers.expectNodeResponse(response);
          TestCleanup.trackNode(node.id);
          
          console.log(`Many metadata (${size} tags): ${duration}ms`);
        } else {
          console.log(`Many metadata (${size} tags) failed: ${response.status}`);
        }
        
        expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.nodeCreation * 2);
      }
    });
  });

  describe('System Resource Usage', () => {
    it('should maintain performance under sustained load', async () => {
      const loadTestDuration = 10000; // 10 seconds
      const operationInterval = 100; // 100ms between operations
      const operations: Promise<any>[] = [];
      
      const startTime = Date.now();
      let operationCount = 0;
      
      const loadInterval = setInterval(() => {
        if (Date.now() - startTime >= loadTestDuration) {
          clearInterval(loadInterval);
          return;
        }
        
        operationCount++;
        const operation = ApiHelpers.createNode({
          title: `Load Test ${operationCount}`,
          content: `Sustained load test operation ${operationCount}`,
          tags: ['load-test'],
          file_type: operationCount % 2 === 0 ? 'md' : 'org'
        });
        
        operations.push(operation);
      }, operationInterval);
      
      // Wait for load test to complete
      await new Promise(resolve => setTimeout(resolve, loadTestDuration + 1000));
      
      // Wait for all operations to complete
      const responses = await Promise.all(operations);
      const endTime = Date.now();
      
      const totalDuration = endTime - startTime;
      const successfulOps = responses.filter(r => r.status === 201).length;
      const avgResponseTime = responses
        .filter(r => r.status === 201)
        .reduce((sum, _, index) => sum + 100, 0) / successfulOps; // Approximate
      
      // Track successful nodes for cleanup
      responses.forEach(response => {
        if (response.status === 201 && response.body.id) {
          TestCleanup.trackNode(response.body.id);
        }
      });
      
      console.log(`Sustained load: ${operationCount} operations in ${totalDuration}ms, ${successfulOps} successful`);
      
      expect(successfulOps).toBeGreaterThan(operationCount * 0.8);
    });

    it('should handle memory-intensive operations', async () => {
      // Create nodes with various large data patterns
      const memoryIntensiveNodes = [
        {
          title: 'Large String Content',
          content: 'x'.repeat(50000),
          file_type: 'md' as const
        },
        {
          title: 'Many Small Tags',
          content: 'Testing memory with many tags',
          tags: Array.from({ length: 1000 }, (_, i) => `tag${i}`),
          file_type: 'md' as const
        },
        {
          title: 'Repeated Pattern',
          content: 'Pattern: 日本語テスト English test 中文测试 '.repeat(1000),
          file_type: 'org' as const
        }
      ];

      const startTime = Date.now();
      const responses = await Promise.all(
        memoryIntensiveNodes.map(nodeData => ApiHelpers.createNode(nodeData))
      );
      const endTime = Date.now();
      
      const duration = endTime - startTime;
      const successfulNodes = responses.filter(r => r.status === 201);
      
      // Track successful nodes
      successfulNodes.forEach(response => {
        if (response.body.id) {
          TestCleanup.trackNode(response.body.id);
        }
      });
      
      console.log(`Memory-intensive operations: ${duration}ms, ${successfulNodes.length} successful`);
      
      expect(successfulNodes.length).toBeGreaterThan(0);
      expect(duration).toBeLessThan(PERFORMANCE_THRESHOLDS.bulkOperations);
    });
  });
});