import { ApiHelpers } from './apiHelpers';
import { TestMeasurement, PerformanceReport } from './testReliability';

/**
 * Comprehensive test monitoring, alerting, and analytics system
 */

// Real-time Test Monitoring
export class TestMonitor {
  private static alerts = new Map<string, TestAlert[]>();
  private static metrics = new Map<string, TestMetric[]>();
  private static thresholds: MonitoringThresholds = {
    responseTime: { warning: 2000, critical: 5000 },
    errorRate: { warning: 0.05, critical: 0.1 },
    memoryUsage: { warning: 0.8, critical: 0.95 },
    testDuration: { warning: 30000, critical: 60000 },
  };

  /**
   * Start monitoring a test suite
   */
  static startMonitoring(suiteName: string): TestMonitorSession {
    const session: TestMonitorSession = {
      suiteName,
      startTime: Date.now(),
      endTime: 0,
      testCount: 0,
      passCount: 0,
      failCount: 0,
      skipCount: 0,
      errors: [],
      warnings: [],
      performance: {
        avgResponseTime: 0,
        maxResponseTime: 0,
        minResponseTime: Infinity,
        totalRequests: 0,
        errorCount: 0,
      },
    };

    this.alerts.set(suiteName, []);
    this.metrics.set(suiteName, []);
    
    console.log(`📊 Starting monitoring for test suite: ${suiteName}`);
    return session;
  }

  /**
   * Record test result
   */
  static recordTestResult(suiteName: string, testName: string, result: TestResult): void {
    const metric: TestMetric = {
      suiteName,
      testName,
      timestamp: Date.now(),
      duration: result.duration,
      status: result.status,
      errorMessage: result.errorMessage,
      responseTime: result.responseTime,
      memoryUsage: this.getMemoryUsage(),
    };

    if (!this.metrics.has(suiteName)) {
      this.metrics.set(suiteName, []);
    }
    this.metrics.get(suiteName)!.push(metric);

    // Check thresholds and generate alerts
    this.checkThresholds(suiteName, metric);

    // Log significant events
    if (result.status === 'failed') {
      console.warn(`❌ Test failed: ${testName} - ${result.errorMessage || 'Unknown error'}`);
    } else if (result.responseTime && result.responseTime > this.thresholds.responseTime.warning) {
      console.warn(`⚠️  Slow test: ${testName} took ${result.responseTime}ms`);
    }
  }

  /**
   * Check monitoring thresholds and generate alerts
   */
  private static checkThresholds(suiteName: string, metric: TestMetric): void {
    const alerts = this.alerts.get(suiteName) || [];

    // Response time alerts
    if (metric.responseTime) {
      if (metric.responseTime > this.thresholds.responseTime.critical) {
        alerts.push({
          type: 'critical',
          category: 'performance',
          message: `Critical response time: ${metric.responseTime}ms (threshold: ${this.thresholds.responseTime.critical}ms)`,
          timestamp: Date.now(),
          testName: metric.testName,
          value: metric.responseTime,
        });
      } else if (metric.responseTime > this.thresholds.responseTime.warning) {
        alerts.push({
          type: 'warning',
          category: 'performance',
          message: `High response time: ${metric.responseTime}ms (threshold: ${this.thresholds.responseTime.warning}ms)`,
          timestamp: Date.now(),
          testName: metric.testName,
          value: metric.responseTime,
        });
      }
    }

    // Memory usage alerts
    if (metric.memoryUsage > this.thresholds.memoryUsage.critical) {
      alerts.push({
        type: 'critical',
        category: 'memory',
        message: `Critical memory usage: ${(metric.memoryUsage * 100).toFixed(1)}%`,
        timestamp: Date.now(),
        testName: metric.testName,
        value: metric.memoryUsage,
      });
    } else if (metric.memoryUsage > this.thresholds.memoryUsage.warning) {
      alerts.push({
        type: 'warning',
        category: 'memory',
        message: `High memory usage: ${(metric.memoryUsage * 100).toFixed(1)}%`,
        timestamp: Date.now(),
        testName: metric.testName,
        value: metric.memoryUsage,
      });
    }

    this.alerts.set(suiteName, alerts);
  }

  /**
   * Get current memory usage percentage
   */
  private static getMemoryUsage(): number {
    if (typeof process !== 'undefined' && process.memoryUsage) {
      const usage = process.memoryUsage();
      const total = usage.heapTotal + usage.external;
      const used = usage.heapUsed + usage.external;
      return used / total;
    }
    return 0;
  }

  /**
   * Get monitoring report for a test suite
   */
  static getMonitoringReport(suiteName: string): MonitoringReport | null {
    const metrics = this.metrics.get(suiteName);
    const alerts = this.alerts.get(suiteName);

    if (!metrics) {
      return null;
    }

    const testCount = metrics.length;
    const passCount = metrics.filter(m => m.status === 'passed').length;
    const failCount = metrics.filter(m => m.status === 'failed').length;
    const skipCount = metrics.filter(m => m.status === 'skipped').length;

    const responseTimes = metrics
      .map(m => m.responseTime)
      .filter(rt => rt !== undefined) as number[];

    const durations = metrics.map(m => m.duration);

    return {
      suiteName,
      summary: {
        testCount,
        passCount,
        failCount,
        skipCount,
        successRate: testCount > 0 ? passCount / testCount : 0,
        errorRate: testCount > 0 ? failCount / testCount : 0,
      },
      performance: {
        avgResponseTime: responseTimes.length > 0 
          ? responseTimes.reduce((a, b) => a + b, 0) / responseTimes.length : 0,
        maxResponseTime: responseTimes.length > 0 ? Math.max(...responseTimes) : 0,
        minResponseTime: responseTimes.length > 0 ? Math.min(...responseTimes) : 0,
        avgTestDuration: durations.reduce((a, b) => a + b, 0) / durations.length,
        maxTestDuration: Math.max(...durations),
        minTestDuration: Math.min(...durations),
        totalDuration: durations.reduce((a, b) => a + b, 0),
      },
      alerts: alerts || [],
      trends: this.calculateTrends(metrics),
      timestamp: Date.now(),
    };
  }

  /**
   * Calculate performance trends
   */
  private static calculateTrends(metrics: TestMetric[]): TrendAnalysis {
    const recentMetrics = metrics.slice(-10); // Last 10 tests
    const olderMetrics = metrics.slice(-20, -10); // Previous 10 tests

    const recentAvgResponseTime = this.calculateAverage(
      recentMetrics.map(m => m.responseTime).filter(rt => rt !== undefined) as number[]
    );
    const olderAvgResponseTime = this.calculateAverage(
      olderMetrics.map(m => m.responseTime).filter(rt => rt !== undefined) as number[]
    );

    const recentSuccessRate = recentMetrics.length > 0 
      ? recentMetrics.filter(m => m.status === 'passed').length / recentMetrics.length : 0;
    const olderSuccessRate = olderMetrics.length > 0 
      ? olderMetrics.filter(m => m.status === 'passed').length / olderMetrics.length : 0;

    return {
      responseTime: {
        trend: recentAvgResponseTime > olderAvgResponseTime ? 'degrading' : 'improving',
        change: recentAvgResponseTime - olderAvgResponseTime,
        changePercent: olderAvgResponseTime > 0 
          ? ((recentAvgResponseTime - olderAvgResponseTime) / olderAvgResponseTime) * 100 : 0,
      },
      successRate: {
        trend: recentSuccessRate > olderSuccessRate ? 'improving' : 'degrading',
        change: recentSuccessRate - olderSuccessRate,
        changePercent: olderSuccessRate > 0 
          ? ((recentSuccessRate - olderSuccessRate) / olderSuccessRate) * 100 : 0,
      },
    };
  }

  private static calculateAverage(values: number[]): number {
    return values.length > 0 ? values.reduce((a, b) => a + b, 0) / values.length : 0;
  }

  /**
   * Clear monitoring data
   */
  static clearMonitoringData(suiteName?: string): void {
    if (suiteName) {
      this.alerts.delete(suiteName);
      this.metrics.delete(suiteName);
    } else {
      this.alerts.clear();
      this.metrics.clear();
    }
  }

  /**
   * Get all active alerts
   */
  static getActiveAlerts(): AlertSummary {
    const allAlerts: TestAlert[] = [];
    for (const alerts of this.alerts.values()) {
      allAlerts.push(...alerts);
    }

    return {
      total: allAlerts.length,
      critical: allAlerts.filter(a => a.type === 'critical').length,
      warning: allAlerts.filter(a => a.type === 'warning').length,
      byCategory: this.groupAlertsByCategory(allAlerts),
      recent: allAlerts.filter(a => Date.now() - a.timestamp < 300000), // Last 5 minutes
    };
  }

  private static groupAlertsByCategory(alerts: TestAlert[]): Record<string, number> {
    const grouped: Record<string, number> = {};
    for (const alert of alerts) {
      grouped[alert.category] = (grouped[alert.category] || 0) + 1;
    }
    return grouped;
  }
}

// Health Check System
export class HealthCheckMonitor {
  private static checks = new Map<string, HealthCheck>();
  private static checkInterval = 60000; // 1 minute
  private static timers = new Map<string, NodeJS.Timeout>();

  /**
   * Register a health check
   */
  static registerHealthCheck(check: HealthCheckConfig): void {
    const healthCheck: HealthCheck = {
      ...check,
      status: 'unknown',
      lastCheck: 0,
      lastSuccess: 0,
      lastError: undefined,
      consecutiveFailures: 0,
    };

    this.checks.set(check.name, healthCheck);

    // Start periodic checking
    if (check.interval) {
      const timer = setInterval(() => this.runHealthCheck(check.name), check.interval);
      this.timers.set(check.name, timer);
    }

    console.log(`🏥 Health check registered: ${check.name}`);
  }

  /**
   * Run a specific health check
   */
  static async runHealthCheck(checkName: string): Promise<HealthCheckResult> {
    const check = this.checks.get(checkName);
    if (!check) {
      throw new Error(`Health check not found: ${checkName}`);
    }

    const startTime = Date.now();

    try {
      await check.checkFunction();
      
      const result: HealthCheckResult = {
        name: checkName,
        status: 'healthy',
        duration: Date.now() - startTime,
        timestamp: Date.now(),
        message: 'Check passed successfully',
      };

      check.status = 'healthy';
      check.lastCheck = Date.now();
      check.lastSuccess = Date.now();
      check.consecutiveFailures = 0;
      check.lastError = undefined;

      return result;
    } catch (error) {
      const result: HealthCheckResult = {
        name: checkName,
        status: 'unhealthy',
        duration: Date.now() - startTime,
        timestamp: Date.now(),
        message: error instanceof Error ? error.message : String(error),
      };

      check.status = 'unhealthy';
      check.lastCheck = Date.now();
      check.lastError = result.message;
      check.consecutiveFailures++;

      console.warn(`🏥 Health check failed: ${checkName} - ${result.message}`);
      return result;
    }
  }

  /**
   * Run all health checks
   */
  static async runAllHealthChecks(): Promise<HealthCheckSummary> {
    const results: HealthCheckResult[] = [];
    
    for (const checkName of this.checks.keys()) {
      try {
        const result = await this.runHealthCheck(checkName);
        results.push(result);
      } catch (error) {
        results.push({
          name: checkName,
          status: 'unhealthy',
          duration: 0,
          timestamp: Date.now(),
          message: error instanceof Error ? error.message : String(error),
        });
      }
    }

    const healthyCount = results.filter(r => r.status === 'healthy').length;
    
    return {
      overall: healthyCount === results.length ? 'healthy' : 'unhealthy',
      totalChecks: results.length,
      healthyChecks: healthyCount,
      unhealthyChecks: results.length - healthyCount,
      results,
      timestamp: Date.now(),
    };
  }

  /**
   * Get health check status
   */
  static getHealthStatus(): Record<string, HealthCheck> {
    return Object.fromEntries(this.checks);
  }

  /**
   * Stop all health checks
   */
  static stopAllHealthChecks(): void {
    for (const timer of this.timers.values()) {
      clearInterval(timer);
    }
    this.timers.clear();
    console.log('🏥 All health checks stopped');
  }

  /**
   * Default health checks for the md-roam server
   */
  static registerDefaultHealthChecks(): void {
    // Server connectivity check
    this.registerHealthCheck({
      name: 'server_connectivity',
      description: 'Check if the server is responding',
      checkFunction: async () => {
        const response = await ApiHelpers.healthCheck();
        if (response.status !== 200) {
          throw new Error(`Server returned status ${response.status}`);
        }
      },
      interval: 30000, // 30 seconds
      timeout: 5000,
    });

    // Database connectivity check
    this.registerHealthCheck({
      name: 'database_connectivity',
      description: 'Check if the database is accessible',
      checkFunction: async () => {
        const response = await ApiHelpers.getStats();
        if (response.status !== 200) {
          throw new Error(`Stats endpoint returned status ${response.status}`);
        }
        if (!response.body?.data) {
          throw new Error('Stats response missing data');
        }
      },
      interval: 60000, // 1 minute
      timeout: 10000,
    });

    // API responsiveness check
    this.registerHealthCheck({
      name: 'api_responsiveness',
      description: 'Check API response time',
      checkFunction: async () => {
        const startTime = Date.now();
        const response = await ApiHelpers.getAllNodes();
        const responseTime = Date.now() - startTime;
        
        if (response.status !== 200) {
          throw new Error(`Nodes endpoint returned status ${response.status}`);
        }
        if (responseTime > 5000) {
          throw new Error(`API too slow: ${responseTime}ms`);
        }
      },
      interval: 45000, // 45 seconds
      timeout: 8000,
    });
  }
}

// Test Analytics and Reporting
export class TestAnalytics {
  private static testHistory: TestHistoryEntry[] = [];
  private static maxHistorySize = 1000;

  /**
   * Record test execution
   */
  static recordExecution(execution: TestExecution): void {
    const entry: TestHistoryEntry = {
      ...execution,
      timestamp: Date.now(),
    };

    this.testHistory.push(entry);

    // Maintain history size limit
    if (this.testHistory.length > this.maxHistorySize) {
      this.testHistory = this.testHistory.slice(-this.maxHistorySize);
    }
  }

  /**
   * Generate analytics report
   */
  static generateAnalyticsReport(timeRange?: TimeRange): AnalyticsReport {
    const filteredHistory = timeRange 
      ? this.testHistory.filter(entry => 
          entry.timestamp >= timeRange.start && entry.timestamp <= timeRange.end
        )
      : this.testHistory;

    if (filteredHistory.length === 0) {
      return this.getEmptyReport();
    }

    // Test execution trends
    const executionTrends = this.calculateExecutionTrends(filteredHistory);
    
    // Performance metrics
    const performanceMetrics = this.calculatePerformanceMetrics(filteredHistory);
    
    // Quality metrics
    const qualityMetrics = this.calculateQualityMetrics(filteredHistory);
    
    // Failure analysis
    const failureAnalysis = this.analyzeFailures(filteredHistory);

    return {
      timeRange: timeRange || { 
        start: Math.min(...filteredHistory.map(e => e.timestamp)), 
        end: Math.max(...filteredHistory.map(e => e.timestamp)) 
      },
      summary: {
        totalExecutions: filteredHistory.length,
        uniqueSuites: new Set(filteredHistory.map(e => e.suiteName)).size,
        totalTests: filteredHistory.reduce((sum, e) => sum + e.testCount, 0),
        totalPasses: filteredHistory.reduce((sum, e) => sum + e.passCount, 0),
        totalFailures: filteredHistory.reduce((sum, e) => sum + e.failCount, 0),
        avgExecutionTime: this.calculateAverage(filteredHistory.map(e => e.duration)),
      },
      trends: executionTrends,
      performance: performanceMetrics,
      quality: qualityMetrics,
      failures: failureAnalysis,
      timestamp: Date.now(),
    };
  }

  private static calculateExecutionTrends(history: TestHistoryEntry[]): ExecutionTrends {
    const dailyGroups = this.groupByDay(history);
    const dailyStats = Object.entries(dailyGroups).map(([date, entries]) => ({
      date,
      executions: entries.length,
      tests: entries.reduce((sum, e) => sum + e.testCount, 0),
      passes: entries.reduce((sum, e) => sum + e.passCount, 0),
      failures: entries.reduce((sum, e) => sum + e.failCount, 0),
      avgDuration: this.calculateAverage(entries.map(e => e.duration)),
    }));

    return {
      daily: dailyStats,
      overall: {
        trend: this.calculateTrendDirection(dailyStats.map(d => d.executions)),
        successRateTrend: this.calculateTrendDirection(
          dailyStats.map(d => d.tests > 0 ? d.passes / d.tests : 0)
        ),
      },
    };
  }

  private static calculatePerformanceMetrics(history: TestHistoryEntry[]): PerformanceMetrics {
    const durations = history.map(e => e.duration);
    const avgResponseTimes = history
      .map(e => e.avgResponseTime)
      .filter(rt => rt !== undefined && rt > 0) as number[];

    return {
      executionTime: {
        avg: this.calculateAverage(durations),
        min: Math.min(...durations),
        max: Math.max(...durations),
        p95: this.calculatePercentile(durations, 0.95),
        trend: this.calculateTrendDirection(durations.slice(-10)),
      },
      responseTime: avgResponseTimes.length > 0 ? {
        avg: this.calculateAverage(avgResponseTimes),
        min: Math.min(...avgResponseTimes),
        max: Math.max(...avgResponseTimes),
        p95: this.calculatePercentile(avgResponseTimes, 0.95),
        trend: this.calculateTrendDirection(avgResponseTimes.slice(-10)),
      } : undefined,
    };
  }

  private static calculateQualityMetrics(history: TestHistoryEntry[]): QualityMetrics {
    const successRates = history.map(e => 
      e.testCount > 0 ? e.passCount / e.testCount : 0
    );
    
    const flakyTests = this.identifyFlakyTests(history);

    return {
      successRate: {
        current: this.calculateAverage(successRates.slice(-5)),
        overall: this.calculateAverage(successRates),
        trend: this.calculateTrendDirection(successRates.slice(-10)),
      },
      stability: {
        flakyTestCount: flakyTests.length,
        flakyTests: flakyTests.slice(0, 10), // Top 10 flaky tests
        consistencyScore: this.calculateConsistencyScore(history),
      },
    };
  }

  private static analyzeFailures(history: TestHistoryEntry[]): FailureAnalysis {
    const failures = history.filter(e => e.failCount > 0);
    const failuresByType = this.groupFailuresByType(failures);
    const topFailingTests = this.getTopFailingTests(failures);

    return {
      totalFailures: failures.reduce((sum, f) => sum + f.failCount, 0),
      failureRate: history.length > 0 ? failures.length / history.length : 0,
      byType: failuresByType,
      topFailingTests,
      recentTrend: this.calculateTrendDirection(
        history.slice(-10).map(e => e.failCount)
      ),
    };
  }

  // Utility methods
  private static groupByDay(history: TestHistoryEntry[]): Record<string, TestHistoryEntry[]> {
    return history.reduce((groups, entry) => {
      const date = new Date(entry.timestamp).toISOString().split('T')[0];
      if (!groups[date]) groups[date] = [];
      groups[date].push(entry);
      return groups;
    }, {} as Record<string, TestHistoryEntry[]>);
  }

  private static calculateAverage(values: number[]): number {
    return values.length > 0 ? values.reduce((a, b) => a + b, 0) / values.length : 0;
  }

  private static calculatePercentile(values: number[], percentile: number): number {
    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * percentile) - 1;
    return sorted[index] || 0;
  }

  private static calculateTrendDirection(values: number[]): 'improving' | 'degrading' | 'stable' {
    if (values.length < 2) return 'stable';
    
    const recent = values.slice(-Math.min(5, values.length));
    const older = values.slice(-Math.min(10, values.length), -5);
    
    if (older.length === 0) return 'stable';
    
    const recentAvg = this.calculateAverage(recent);
    const olderAvg = this.calculateAverage(older);
    
    const changePercent = Math.abs((recentAvg - olderAvg) / olderAvg);
    
    if (changePercent < 0.05) return 'stable'; // Less than 5% change
    return recentAvg > olderAvg ? 'improving' : 'degrading';
  }

  private static identifyFlakyTests(history: TestHistoryEntry[]): string[] {
    // This is a simplified implementation
    // In practice, you'd track individual test results
    return [];
  }

  private static calculateConsistencyScore(history: TestHistoryEntry[]): number {
    if (history.length === 0) return 1;
    
    const successRates = history.map(e => e.testCount > 0 ? e.passCount / e.testCount : 1);
    const variance = this.calculateVariance(successRates);
    
    // Convert variance to consistency score (0-1, higher is better)
    return Math.max(0, 1 - variance);
  }

  private static calculateVariance(values: number[]): number {
    if (values.length === 0) return 0;
    
    const mean = this.calculateAverage(values);
    const squaredDiffs = values.map(value => Math.pow(value - mean, 2));
    return this.calculateAverage(squaredDiffs);
  }

  private static groupFailuresByType(failures: TestHistoryEntry[]): Record<string, number> {
    // Simplified grouping - in practice, you'd analyze error messages
    return {
      'timeout': failures.filter(f => f.errors?.some(e => e.includes('timeout'))).length,
      'connection': failures.filter(f => f.errors?.some(e => e.includes('connection'))).length,
      'assertion': failures.filter(f => f.errors?.some(e => e.includes('assertion'))).length,
      'other': failures.length,
    };
  }

  private static getTopFailingTests(failures: TestHistoryEntry[]): Array<{ testName: string; failureCount: number }> {
    // Simplified implementation
    return [];
  }

  private static getEmptyReport(): AnalyticsReport {
    return {
      timeRange: { start: Date.now(), end: Date.now() },
      summary: {
        totalExecutions: 0,
        uniqueSuites: 0,
        totalTests: 0,
        totalPasses: 0,
        totalFailures: 0,
        avgExecutionTime: 0,
      },
      trends: {
        daily: [],
        overall: { trend: 'stable', successRateTrend: 'stable' },
      },
      performance: {
        executionTime: {
          avg: 0, min: 0, max: 0, p95: 0, trend: 'stable',
        },
      },
      quality: {
        successRate: {
          current: 0, overall: 0, trend: 'stable',
        },
        stability: {
          flakyTestCount: 0, flakyTests: [], consistencyScore: 1,
        },
      },
      failures: {
        totalFailures: 0,
        failureRate: 0,
        byType: {},
        topFailingTests: [],
        recentTrend: 'stable',
      },
      timestamp: Date.now(),
    };
  }

  /**
   * Clear analytics history
   */
  static clearHistory(): void {
    this.testHistory = [];
  }

  /**
   * Export analytics data
   */
  static exportData(): TestHistoryEntry[] {
    return [...this.testHistory];
  }
}

// Type definitions
export interface TestAlert {
  type: 'warning' | 'critical';
  category: string;
  message: string;
  timestamp: number;
  testName: string;
  value: number;
}

export interface TestMetric {
  suiteName: string;
  testName: string;
  timestamp: number;
  duration: number;
  status: 'passed' | 'failed' | 'skipped';
  errorMessage?: string;
  responseTime?: number;
  memoryUsage: number;
}

export interface TestResult {
  status: 'passed' | 'failed' | 'skipped';
  duration: number;
  errorMessage?: string;
  responseTime?: number;
}

export interface MonitoringThresholds {
  responseTime: { warning: number; critical: number };
  errorRate: { warning: number; critical: number };
  memoryUsage: { warning: number; critical: number };
  testDuration: { warning: number; critical: number };
}

export interface TestMonitorSession {
  suiteName: string;
  startTime: number;
  endTime: number;
  testCount: number;
  passCount: number;
  failCount: number;
  skipCount: number;
  errors: string[];
  warnings: string[];
  performance: {
    avgResponseTime: number;
    maxResponseTime: number;
    minResponseTime: number;
    totalRequests: number;
    errorCount: number;
  };
}

export interface MonitoringReport {
  suiteName: string;
  summary: {
    testCount: number;
    passCount: number;
    failCount: number;
    skipCount: number;
    successRate: number;
    errorRate: number;
  };
  performance: {
    avgResponseTime: number;
    maxResponseTime: number;
    minResponseTime: number;
    avgTestDuration: number;
    maxTestDuration: number;
    minTestDuration: number;
    totalDuration: number;
  };
  alerts: TestAlert[];
  trends: TrendAnalysis;
  timestamp: number;
}

export interface TrendAnalysis {
  responseTime: {
    trend: 'improving' | 'degrading' | 'stable';
    change: number;
    changePercent: number;
  };
  successRate: {
    trend: 'improving' | 'degrading' | 'stable';
    change: number;
    changePercent: number;
  };
}

export interface AlertSummary {
  total: number;
  critical: number;
  warning: number;
  byCategory: Record<string, number>;
  recent: TestAlert[];
}

export interface HealthCheckConfig {
  name: string;
  description: string;
  checkFunction: () => Promise<void>;
  interval?: number;
  timeout?: number;
}

export interface HealthCheck extends HealthCheckConfig {
  status: 'healthy' | 'unhealthy' | 'unknown';
  lastCheck: number;
  lastSuccess: number;
  lastError?: string;
  consecutiveFailures: number;
}

export interface HealthCheckResult {
  name: string;
  status: 'healthy' | 'unhealthy';
  duration: number;
  timestamp: number;
  message: string;
}

export interface HealthCheckSummary {
  overall: 'healthy' | 'unhealthy';
  totalChecks: number;
  healthyChecks: number;
  unhealthyChecks: number;
  results: HealthCheckResult[];
  timestamp: number;
}

export interface TestExecution {
  suiteName: string;
  testCount: number;
  passCount: number;
  failCount: number;
  skipCount: number;
  duration: number;
  avgResponseTime?: number;
  errors?: string[];
}

export interface TestHistoryEntry extends TestExecution {
  timestamp: number;
}

export interface TimeRange {
  start: number;
  end: number;
}

export interface AnalyticsReport {
  timeRange: TimeRange;
  summary: {
    totalExecutions: number;
    uniqueSuites: number;
    totalTests: number;
    totalPasses: number;
    totalFailures: number;
    avgExecutionTime: number;
  };
  trends: ExecutionTrends;
  performance: PerformanceMetrics;
  quality: QualityMetrics;
  failures: FailureAnalysis;
  timestamp: number;
}

export interface ExecutionTrends {
  daily: Array<{
    date: string;
    executions: number;
    tests: number;
    passes: number;
    failures: number;
    avgDuration: number;
  }>;
  overall: {
    trend: 'improving' | 'degrading' | 'stable';
    successRateTrend: 'improving' | 'degrading' | 'stable';
  };
}

export interface PerformanceMetrics {
  executionTime: {
    avg: number;
    min: number;
    max: number;
    p95: number;
    trend: 'improving' | 'degrading' | 'stable';
  };
  responseTime?: {
    avg: number;
    min: number;
    max: number;
    p95: number;
    trend: 'improving' | 'degrading' | 'stable';
  };
}

export interface QualityMetrics {
  successRate: {
    current: number;
    overall: number;
    trend: 'improving' | 'degrading' | 'stable';
  };
  stability: {
    flakyTestCount: number;
    flakyTests: string[];
    consistencyScore: number;
  };
}

export interface FailureAnalysis {
  totalFailures: number;
  failureRate: number;
  byType: Record<string, number>;
  topFailingTests: Array<{ testName: string; failureCount: number }>;
  recentTrend: 'improving' | 'degrading' | 'stable';
}