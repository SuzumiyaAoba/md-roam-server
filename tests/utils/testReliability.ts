import { ApiHelpers } from "./apiHelpers";
import type { CreateNodePayload, NodeData } from "./types";

/**
 * Enhanced test reliability utilities for stable, reproducible testing
 */

// Retry configuration
export interface RetryConfig {
  maxAttempts: number;
  delayMs: number;
  backoffFactor: number;
  maxDelayMs: number;
}

export const DEFAULT_RETRY_CONFIG: RetryConfig = {
  maxAttempts: 3,
  delayMs: 1000,
  backoffFactor: 2,
  maxDelayMs: 10000,
};

// Test isolation utilities
export class TestIsolation {
  private static testRunId = Date.now().toString(36);
  private static testCounter = 0;

  /**
   * Generate unique test identifier to prevent cross-test contamination
   */
  static generateTestId(testName: string): string {
    TestIsolation.testCounter++;
    return `${TestIsolation.testRunId}-${TestIsolation.testCounter}-${testName}`;
  }

  /**
   * Create isolated test data with unique identifiers
   */
  static createIsolatedNode(
    baseData: Partial<CreateNodePayload>,
    testName: string,
  ): CreateNodePayload {
    const testId = TestIsolation.generateTestId(testName);
    return {
      title: `${baseData.title || "Test Node"} [${testId}]`,
      content: `${baseData.content || "Test content"}\n\n<!-- Test ID: ${testId} -->`,
      tags: [...(baseData.tags || []), `test-${testId}`, "isolated-test"],
      aliases: baseData.aliases?.map((alias) => `${alias} [${testId}]`) || [
        `Test Alias [${testId}]`,
      ],
      refs: baseData.refs || [],
      category: baseData.category || "test-isolation",
      file_type: baseData.file_type || "md",
    };
  }
}

// Retry mechanism with exponential backoff
export class RetryableOperations {
  /**
   * Execute operation with retry logic
   */
  static async withRetry<T>(
    operation: () => Promise<T>,
    config: Partial<RetryConfig> = {},
    operationName = "operation",
  ): Promise<T> {
    const finalConfig = { ...DEFAULT_RETRY_CONFIG, ...config };
    let lastError: Error;
    let delay = finalConfig.delayMs;

    for (let attempt = 1; attempt <= finalConfig.maxAttempts; attempt++) {
      try {
        return await operation();
      } catch (error) {
        lastError = error as Error;

        console.warn(
          `${operationName} failed on attempt ${attempt}/${finalConfig.maxAttempts}: ${lastError.message}`,
        );

        if (attempt === finalConfig.maxAttempts) {
          break;
        }

        // Wait before retry
        await new Promise((resolve) => setTimeout(resolve, delay));
        delay = Math.min(
          delay * finalConfig.backoffFactor,
          finalConfig.maxDelayMs,
        );
      }
    }

    throw new Error(
      `${operationName} failed after ${finalConfig.maxAttempts} attempts. Last error: ${lastError.message}`,
    );
  }

  /**
   * Robust node creation with retry
   */
  static async createNodeReliably(
    nodeData: CreateNodePayload,
    testName: string,
  ): Promise<NodeData> {
    const isolatedData = TestIsolation.createIsolatedNode(nodeData, testName);

    return await RetryableOperations.withRetry(
      async () => {
        const response = await ApiHelpers.createNode(isolatedData);
        if (response.status !== 201) {
          throw new Error(
            `Node creation failed with status ${response.status}: ${response.body?.message}`,
          );
        }
        return ApiHelpers.expectNodeResponse(response);
      },
      { maxAttempts: 3, delayMs: 1000 },
      "createNode",
    );
  }

  /**
   * Robust server health check with retry
   */
  static async waitForServerHealthy(timeoutMs = 30000): Promise<void> {
    const startTime = Date.now();

    await RetryableOperations.withRetry(
      async () => {
        if (Date.now() - startTime > timeoutMs) {
          throw new Error(`Server health check timeout after ${timeoutMs}ms`);
        }

        const response = await ApiHelpers.healthCheck();
        if (response.status !== 200) {
          throw new Error(`Server not healthy: ${response.status}`);
        }
      },
      { maxAttempts: 30, delayMs: 1000, maxDelayMs: 2000 },
      "serverHealthCheck",
    );
  }
}

// Enhanced cleanup with verification
export class RobustCleanup {
  private static trackedNodes = new Set<string>();
  private static cleanupAttempts = new Map<string, number>();
  private static maxCleanupAttempts = 3;

  static trackNode(nodeId: string): void {
    RobustCleanup.trackedNodes.add(nodeId);
    RobustCleanup.cleanupAttempts.set(nodeId, 0);
  }

  static async cleanupAllNodes(): Promise<CleanupResult> {
    const results: CleanupResult = {
      successful: [],
      failed: [],
      total: RobustCleanup.trackedNodes.size,
    };

    console.log(
      `🧹 Starting robust cleanup of ${RobustCleanup.trackedNodes.size} nodes...`,
    );

    for (const nodeId of RobustCleanup.trackedNodes) {
      const attempts = RobustCleanup.cleanupAttempts.get(nodeId) || 0;

      if (attempts >= RobustCleanup.maxCleanupAttempts) {
        results.failed.push({
          nodeId,
          reason: "Max cleanup attempts exceeded",
        });
        continue;
      }

      try {
        await RobustCleanup.cleanupSingleNode(nodeId);
        results.successful.push(nodeId);
        RobustCleanup.trackedNodes.delete(nodeId);
        RobustCleanup.cleanupAttempts.delete(nodeId);
      } catch (error) {
        const newAttempts = attempts + 1;
        RobustCleanup.cleanupAttempts.set(nodeId, newAttempts);

        if (newAttempts >= RobustCleanup.maxCleanupAttempts) {
          results.failed.push({
            nodeId,
            reason: `Failed after ${newAttempts} attempts: ${error}`,
          });
          RobustCleanup.trackedNodes.delete(nodeId);
          RobustCleanup.cleanupAttempts.delete(nodeId);
        }
      }
    }

    console.log(
      `✅ Cleanup completed: ${results.successful.length} successful, ${results.failed.length} failed`,
    );

    if (results.failed.length > 0) {
      console.warn("Failed cleanups:", results.failed);
    }

    return results;
  }

  private static async cleanupSingleNode(nodeId: string): Promise<void> {
    await RetryableOperations.withRetry(
      async () => {
        // First verify the node exists
        const getResponse = await ApiHelpers.getNode(nodeId);
        if (getResponse.status === 404) {
          // Node already deleted
          return;
        }

        // Delete the node
        const deleteResponse = await ApiHelpers.deleteNode(nodeId);
        if (deleteResponse.status !== 200) {
          throw new Error(`Delete failed with status ${deleteResponse.status}`);
        }

        // Verify deletion
        const verifyResponse = await ApiHelpers.getNode(nodeId);
        if (verifyResponse.status !== 404) {
          throw new Error(`Node still exists after deletion`);
        }
      },
      { maxAttempts: 2, delayMs: 500 },
      `deleteNode(${nodeId})`,
    );
  }

  static getCleanupStats(): CleanupStats {
    return {
      trackedNodes: RobustCleanup.trackedNodes.size,
      pendingCleanup: Array.from(RobustCleanup.trackedNodes),
      attemptCounts: Object.fromEntries(RobustCleanup.cleanupAttempts),
    };
  }

  static async forceCleanupAll(): Promise<void> {
    console.log(
      "🔥 Force cleanup: attempting to clean all tracked nodes regardless of failures",
    );

    for (const nodeId of RobustCleanup.trackedNodes) {
      try {
        await ApiHelpers.deleteNode(nodeId);
      } catch (error) {
        console.warn(`Force cleanup failed for ${nodeId}:`, error);
      }
    }

    // Clear all tracking
    RobustCleanup.trackedNodes.clear();
    RobustCleanup.cleanupAttempts.clear();
  }
}

// Test state verification
export class TestStateVerification {
  /**
   * Verify test environment is clean before starting
   */
  static async verifyCleanEnvironment(): Promise<EnvironmentState> {
    const stats = await ApiHelpers.getStats();
    const allNodes = await ApiHelpers.getAllNodes();

    const testNodes =
      allNodes.body.data?.filter(
        (node: any) =>
          node.title?.includes("[") ||
          node.tags?.some((tag: string) => tag.startsWith("test-")) ||
          node.category === "test-isolation",
      ) || [];

    return {
      isClean: testNodes.length === 0,
      testNodesFound: testNodes.length,
      totalNodes: stats.body.data?.total_nodes || 0,
      testNodeDetails: testNodes.map((node: any) => ({
        id: node.id,
        title: node.title,
        tags: node.tags || [],
      })),
    };
  }

  /**
   * Clean any leftover test data from previous runs
   */
  static async cleanLeftoverTestData(): Promise<number> {
    const envState = await TestStateVerification.verifyCleanEnvironment();

    if (envState.isClean) {
      return 0;
    }

    console.log(
      `🧹 Cleaning ${envState.testNodesFound} leftover test nodes...`,
    );

    let cleanedCount = 0;
    for (const testNode of envState.testNodeDetails) {
      try {
        await ApiHelpers.deleteNode(testNode.id);
        cleanedCount++;
      } catch (error) {
        console.warn(`Failed to clean leftover node ${testNode.id}:`, error);
      }
    }

    return cleanedCount;
  }
}

// Test timing and performance monitoring
export class TestPerformanceMonitor {
  private static measurements = new Map<string, TestMeasurement[]>();

  static startMeasurement(testName: string, operation: string): TestTimer {
    return {
      testName,
      operation,
      startTime: performance.now(),
      endTime: 0,
      duration: 0,
    };
  }

  static endMeasurement(timer: TestTimer): TestMeasurement {
    timer.endTime = performance.now();
    timer.duration = timer.endTime - timer.startTime;

    const measurement: TestMeasurement = {
      testName: timer.testName,
      operation: timer.operation,
      duration: timer.duration,
      timestamp: new Date().toISOString(),
    };

    if (!TestPerformanceMonitor.measurements.has(timer.testName)) {
      TestPerformanceMonitor.measurements.set(timer.testName, []);
    }
    TestPerformanceMonitor.measurements.get(timer.testName)?.push(measurement);

    // Log slow operations
    if (timer.duration > 5000) {
      console.warn(
        `⚠️  Slow operation detected: ${timer.testName}.${timer.operation} took ${timer.duration.toFixed(2)}ms`,
      );
    }

    return measurement;
  }

  static getTestMeasurements(testName?: string): TestMeasurement[] {
    if (testName) {
      return TestPerformanceMonitor.measurements.get(testName) || [];
    }

    const allMeasurements: TestMeasurement[] = [];
    for (const measurements of TestPerformanceMonitor.measurements.values()) {
      allMeasurements.push(...measurements);
    }
    return allMeasurements;
  }

  static getPerformanceReport(): PerformanceReport {
    const allMeasurements = TestPerformanceMonitor.getTestMeasurements();

    if (allMeasurements.length === 0) {
      return {
        totalTests: 0,
        totalOperations: 0,
        avgDuration: 0,
        maxDuration: 0,
        minDuration: 0,
        slowOperations: [],
        operationStats: {},
      };
    }

    const durations = allMeasurements.map((m) => m.duration);
    const operationStats: Record<
      string,
      { count: number; avgDuration: number; maxDuration: number }
    > = {};

    // Calculate operation statistics
    for (const measurement of allMeasurements) {
      if (!operationStats[measurement.operation]) {
        operationStats[measurement.operation] = {
          count: 0,
          avgDuration: 0,
          maxDuration: 0,
        };
      }

      const stats = operationStats[measurement.operation];
      stats.count++;
      stats.maxDuration = Math.max(stats.maxDuration, measurement.duration);
    }

    // Calculate averages
    for (const operation in operationStats) {
      const opMeasurements = allMeasurements.filter(
        (m) => m.operation === operation,
      );
      const avgDuration =
        opMeasurements.reduce((sum, m) => sum + m.duration, 0) /
        opMeasurements.length;
      operationStats[operation].avgDuration = avgDuration;
    }

    return {
      totalTests: new Set(allMeasurements.map((m) => m.testName)).size,
      totalOperations: allMeasurements.length,
      avgDuration: durations.reduce((a, b) => a + b, 0) / durations.length,
      maxDuration: Math.max(...durations),
      minDuration: Math.min(...durations),
      slowOperations: allMeasurements
        .filter((m) => m.duration > 5000)
        .sort((a, b) => b.duration - a.duration),
      operationStats,
    };
  }

  static clearMeasurements(): void {
    TestPerformanceMonitor.measurements.clear();
  }
}

// Type definitions
export interface CleanupResult {
  successful: string[];
  failed: Array<{ nodeId: string; reason: string }>;
  total: number;
}

export interface CleanupStats {
  trackedNodes: number;
  pendingCleanup: string[];
  attemptCounts: Record<string, number>;
}

export interface EnvironmentState {
  isClean: boolean;
  testNodesFound: number;
  totalNodes: number;
  testNodeDetails: Array<{
    id: string;
    title: string;
    tags: string[];
  }>;
}

export interface TestTimer {
  testName: string;
  operation: string;
  startTime: number;
  endTime: number;
  duration: number;
}

export interface TestMeasurement {
  testName: string;
  operation: string;
  duration: number;
  timestamp: string;
}

export interface PerformanceReport {
  totalTests: number;
  totalOperations: number;
  avgDuration: number;
  maxDuration: number;
  minDuration: number;
  slowOperations: TestMeasurement[];
  operationStats: Record<
    string,
    {
      count: number;
      avgDuration: number;
      maxDuration: number;
    }
  >;
}
