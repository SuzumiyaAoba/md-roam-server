import { TestReportGenerator } from "./testCoverage";
import { HealthCheckMonitor, TestAnalytics } from "./testMonitoring";
import { RobustCleanup } from "./testReliability";

/**
 * CI/CD integration utilities for automated testing, deployment, and quality gates
 */

// CI/CD Pipeline Integration
export class CiCdIntegration {
  private static config: CiConfig | null = null;

  /**
   * Initialize CI/CD configuration
   */
  static async initialize(
    configOverrides: Partial<CiConfig> = {},
  ): Promise<void> {
    const defaultConfig: CiConfig = {
      environment: process.env.NODE_ENV || "test",
      parallel: {
        enabled: true,
        maxConcurrency: parseInt(process.env.TEST_CONCURRENCY || "4", 10),
        timeoutMultiplier: parseFloat(
          process.env.TEST_TIMEOUT_MULTIPLIER || "1.5",
        ),
      },
      qualityGates: {
        minCoverage: parseFloat(process.env.MIN_COVERAGE || "85"),
        maxFailureRate: parseFloat(process.env.MAX_FAILURE_RATE || "5"),
        maxResponseTime: parseInt(process.env.MAX_RESPONSE_TIME || "2000", 10),
        requiredHealthChecks: ["server_connectivity", "database_connectivity"],
      },
      notifications: {
        enabled: process.env.CI === "true",
        webhookUrl: process.env.SLACK_WEBHOOK_URL,
        channels: {
          success: process.env.SUCCESS_CHANNEL || "#ci-success",
          failure: process.env.FAILURE_CHANNEL || "#ci-alerts",
          coverage: process.env.COVERAGE_CHANNEL || "#test-coverage",
        },
      },
      artifacts: {
        enabled: true,
        retentionDays: parseInt(
          process.env.ARTIFACT_RETENTION_DAYS || "30",
          10,
        ),
        paths: ["coverage/", "test-results/", "logs/", "reports/"],
      },
      deployment: {
        enabled: process.env.AUTO_DEPLOY === "true",
        environments: ["staging", "production"],
        requiresApproval: ["production"],
      },
    };

    CiCdIntegration.config = { ...defaultConfig, ...configOverrides };
    console.log(
      `🔧 CI/CD configuration initialized for ${CiCdIntegration.config.environment} environment`,
    );
  }

  /**
   * Execute pre-test setup
   */
  static async preTestSetup(): Promise<PreTestResult> {
    console.log("🚀 Starting pre-test setup...");
    const result: PreTestResult = {
      success: true,
      steps: [],
      duration: 0,
    };

    const startTime = Date.now();

    try {
      // Clean up any leftover test data
      const cleanupStep = await CiCdIntegration.executeStep(
        "cleanup",
        async () => {
          await RobustCleanup.forceCleanupAll();
        },
      );
      result.steps.push(cleanupStep);

      // Initialize health checks
      const healthCheckStep = await CiCdIntegration.executeStep(
        "health_checks",
        async () => {
          HealthCheckMonitor.registerDefaultHealthChecks();
          const healthSummary = await HealthCheckMonitor.runAllHealthChecks();
          if (healthSummary.overall !== "healthy") {
            throw new Error(
              `Health checks failed: ${healthSummary.unhealthyChecks}/${healthSummary.totalChecks} unhealthy`,
            );
          }
        },
      );
      result.steps.push(healthCheckStep);

      // Verify environment configuration
      const configStep = await CiCdIntegration.executeStep(
        "environment_config",
        async () => {
          await CiCdIntegration.verifyEnvironmentConfig();
        },
      );
      result.steps.push(configStep);

      result.duration = Date.now() - startTime;
      console.log(`✅ Pre-test setup completed in ${result.duration}ms`);
    } catch (error) {
      result.success = false;
      result.error = error instanceof Error ? error.message : String(error);
      result.duration = Date.now() - startTime;

      console.error(`❌ Pre-test setup failed: ${result.error}`);

      if (CiCdIntegration.config?.notifications.enabled) {
        await CiCdIntegration.sendNotification(
          "failure",
          `Pre-test setup failed: ${result.error}`,
        );
      }
    }

    return result;
  }

  /**
   * Execute post-test cleanup and reporting
   */
  static async postTestCleanup(
    testResults: TestExecutionSummary,
  ): Promise<PostTestResult> {
    console.log("🧹 Starting post-test cleanup and reporting...");
    const result: PostTestResult = {
      success: true,
      steps: [],
      duration: 0,
      reports: {},
    };

    const startTime = Date.now();

    try {
      // Generate comprehensive test report
      const reportStep = await CiCdIntegration.executeStep(
        "generate_reports",
        async () => {
          const coverageReport =
            await TestReportGenerator.generateComprehensiveReport();
          const analyticsReport = TestAnalytics.generateAnalyticsReport();

          result.reports = {
            coverage: coverageReport,
            analytics: analyticsReport,
          };
        },
      );
      result.steps.push(reportStep);

      // Clean up test resources
      const cleanupStep = await CiCdIntegration.executeStep(
        "cleanup",
        async () => {
          const cleanupResult = await RobustCleanup.cleanupAllNodes();
          if (cleanupResult.failed.length > 0) {
            console.warn(
              `Some cleanup operations failed: ${cleanupResult.failed.length} nodes`,
            );
          }
        },
      );
      result.steps.push(cleanupStep);

      // Quality gate evaluation
      const qualityGateStep = await CiCdIntegration.executeStep(
        "quality_gates",
        async () => {
          const gateResult = await CiCdIntegration.evaluateQualityGates(
            testResults,
            result.reports,
          );
          if (!gateResult.passed) {
            throw new Error(
              `Quality gates failed: ${gateResult.failures.join(", ")}`,
            );
          }
        },
      );
      result.steps.push(qualityGateStep);

      // Send notifications
      if (CiCdIntegration.config?.notifications.enabled) {
        const notificationStep = await CiCdIntegration.executeStep(
          "notifications",
          async () => {
            await CiCdIntegration.sendTestResultNotifications(
              testResults,
              result.reports,
            );
          },
        );
        result.steps.push(notificationStep);
      }

      result.duration = Date.now() - startTime;
      console.log(`✅ Post-test cleanup completed in ${result.duration}ms`);
    } catch (error) {
      result.success = false;
      result.error = error instanceof Error ? error.message : String(error);
      result.duration = Date.now() - startTime;

      console.error(`❌ Post-test cleanup failed: ${result.error}`);

      if (CiCdIntegration.config?.notifications.enabled) {
        await CiCdIntegration.sendNotification(
          "failure",
          `Post-test cleanup failed: ${result.error}`,
        );
      }
    }

    return result;
  }

  /**
   * Evaluate quality gates
   */
  private static async evaluateQualityGates(
    testResults: TestExecutionSummary,
    reports: { coverage?: any; analytics?: any },
  ): Promise<QualityGateResult> {
    const result: QualityGateResult = {
      passed: true,
      failures: [],
      details: {},
    };

    if (!CiCdIntegration.config) {
      throw new Error("CI/CD configuration not initialized");
    }

    const gates = CiCdIntegration.config.qualityGates;

    // Test success rate gate
    const successRate =
      testResults.totalTests > 0
        ? (testResults.passCount / testResults.totalTests) * 100
        : 0;
    const failureRate = 100 - successRate;

    if (failureRate > gates.maxFailureRate) {
      result.passed = false;
      result.failures.push(
        `Failure rate ${failureRate.toFixed(1)}% exceeds limit ${gates.maxFailureRate}%`,
      );
    }
    result.details.failureRate = failureRate;

    // Coverage gate (if coverage report available)
    if (reports.coverage) {
      const coveragePercent =
        reports.coverage.apiCoverage?.coveragePercentage || 0;
      if (coveragePercent < gates.minCoverage) {
        result.passed = false;
        result.failures.push(
          `Coverage ${coveragePercent.toFixed(1)}% below minimum ${gates.minCoverage}%`,
        );
      }
      result.details.coverage = coveragePercent;
    }

    // Response time gate
    if (
      testResults.avgResponseTime &&
      testResults.avgResponseTime > gates.maxResponseTime
    ) {
      result.passed = false;
      result.failures.push(
        `Average response time ${testResults.avgResponseTime}ms exceeds limit ${gates.maxResponseTime}ms`,
      );
    }
    result.details.avgResponseTime = testResults.avgResponseTime;

    // Health checks gate
    const healthSummary = await HealthCheckMonitor.runAllHealthChecks();
    const missingHealthChecks = gates.requiredHealthChecks.filter(
      (checkName) =>
        !healthSummary.results.some(
          (r) => r.name === checkName && r.status === "healthy",
        ),
    );

    if (missingHealthChecks.length > 0) {
      result.passed = false;
      result.failures.push(
        `Required health checks failed: ${missingHealthChecks.join(", ")}`,
      );
    }
    result.details.healthChecks = healthSummary;

    return result;
  }

  /**
   * Execute a single setup/cleanup step
   */
  private static async executeStep(
    stepName: string,
    stepFunction: () => Promise<void>,
  ): Promise<CiStep> {
    const startTime = Date.now();

    try {
      await stepFunction();
      return {
        name: stepName,
        success: true,
        duration: Date.now() - startTime,
      };
    } catch (error) {
      return {
        name: stepName,
        success: false,
        duration: Date.now() - startTime,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Verify environment configuration
   */
  private static async verifyEnvironmentConfig(): Promise<void> {
    const requiredEnvVars = ["NODE_ENV"];
    const missing = requiredEnvVars.filter((varName) => !process.env[varName]);

    if (missing.length > 0) {
      throw new Error(
        `Missing required environment variables: ${missing.join(", ")}`,
      );
    }

    // Verify server is accessible
    try {
      const response = await fetch("http://localhost:8080/stats");
      if (!response.ok) {
        throw new Error(`Server health check failed: ${response.status}`);
      }
    } catch (error) {
      throw new Error(`Cannot connect to server: ${error}`);
    }
  }

  /**
   * Send test result notifications
   */
  private static async sendTestResultNotifications(
    testResults: TestExecutionSummary,
    reports: { coverage?: any; analytics?: any },
  ): Promise<void> {
    if (
      !CiCdIntegration.config?.notifications.enabled ||
      !CiCdIntegration.config.notifications.webhookUrl
    ) {
      return;
    }

    const isSuccess = testResults.failCount === 0;
    const channel = isSuccess
      ? CiCdIntegration.config.notifications.channels.success
      : CiCdIntegration.config.notifications.channels.failure;

    const message = CiCdIntegration.formatTestResultMessage(
      testResults,
      reports,
      isSuccess,
    );

    await CiCdIntegration.sendNotification(
      isSuccess ? "success" : "failure",
      message,
      channel,
    );
  }

  /**
   * Format test result message for notifications
   */
  private static formatTestResultMessage(
    testResults: TestExecutionSummary,
    reports: { coverage?: any; analytics?: any },
    isSuccess: boolean,
  ): string {
    const emoji = isSuccess ? "✅" : "❌";
    const status = isSuccess ? "PASSED" : "FAILED";

    let message = `${emoji} Test Suite ${status}\n\n`;
    message += `📊 **Results:**\n`;
    message += `• Total Tests: ${testResults.totalTests}\n`;
    message += `• Passed: ${testResults.passCount}\n`;
    message += `• Failed: ${testResults.failCount}\n`;
    message += `• Duration: ${(testResults.duration / 1000).toFixed(1)}s\n`;

    if (testResults.avgResponseTime) {
      message += `• Avg Response Time: ${testResults.avgResponseTime}ms\n`;
    }

    if (reports.coverage?.apiCoverage) {
      message += `\n📈 **Coverage:**\n`;
      message += `• API Coverage: ${reports.coverage.apiCoverage.coveragePercentage.toFixed(1)}%\n`;
      message += `• Endpoints: ${reports.coverage.apiCoverage.coveredEndpoints.length}/${reports.coverage.apiCoverage.totalEndpoints}\n`;
    }

    if (!isSuccess && testResults.errors?.length) {
      message += `\n🔍 **Errors:**\n`;
      testResults.errors.slice(0, 3).forEach((error) => {
        message += `• ${error}\n`;
      });
      if (testResults.errors.length > 3) {
        message += `• ... and ${testResults.errors.length - 3} more\n`;
      }
    }

    message += `\n🏗️ Environment: ${CiCdIntegration.config?.environment || "unknown"}\n`;
    message += `📅 Time: ${new Date().toISOString()}\n`;

    return message;
  }

  /**
   * Send notification to configured webhook
   */
  private static async sendNotification(
    type: "success" | "failure" | "info",
    message: string,
    channel?: string,
  ): Promise<void> {
    if (!CiCdIntegration.config?.notifications.webhookUrl) {
      return;
    }

    try {
      const payload = {
        text: message,
        channel: channel,
        username: "Test Bot",
        icon_emoji: type === "success" ? ":white_check_mark:" : ":x:",
      };

      const response = await fetch(
        CiCdIntegration.config.notifications.webhookUrl,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        },
      );

      if (!response.ok) {
        console.warn(`Failed to send notification: ${response.status}`);
      }
    } catch (error) {
      console.warn(`Error sending notification: ${error}`);
    }
  }
}

// Test Orchestration
export class TestOrchestrator {
  private static testSuites: Map<string, TestSuite> = new Map();

  /**
   * Register test suite
   */
  static registerSuite(suite: TestSuite): void {
    TestOrchestrator.testSuites.set(suite.name, suite);
    console.log(`📋 Registered test suite: ${suite.name}`);
  }

  /**
   * Execute all test suites
   */
  static async executeAllSuites(
    options: ExecutionOptions = {},
  ): Promise<OrchestrationResult> {
    console.log("🎭 Starting test orchestration...");

    const result: OrchestrationResult = {
      success: true,
      suites: [],
      totalDuration: 0,
      summary: {
        totalTests: 0,
        passCount: 0,
        failCount: 0,
        skipCount: 0,
        duration: 0,
        avgResponseTime: 0,
        errors: [],
      },
    };

    const startTime = Date.now();

    // Pre-test setup
    const preTestResult = await CiCdIntegration.preTestSetup();
    if (!preTestResult.success) {
      result.success = false;
      result.error = `Pre-test setup failed: ${preTestResult.error}`;
      return result;
    }

    try {
      // Execute suites based on options
      const suitesToRun =
        options.suites || Array.from(TestOrchestrator.testSuites.keys());

      if (options.parallel && suitesToRun.length > 1) {
        result.suites = await TestOrchestrator.executeInParallel(
          suitesToRun,
          options,
        );
      } else {
        result.suites = await TestOrchestrator.executeSequentially(
          suitesToRun,
          options,
        );
      }

      // Calculate summary
      result.summary = TestOrchestrator.calculateSummary(result.suites);
      result.totalDuration = Date.now() - startTime;

      // Check if any suite failed
      result.success = result.suites.every((suite) => suite.success);
    } catch (error) {
      result.success = false;
      result.error = error instanceof Error ? error.message : String(error);
      result.totalDuration = Date.now() - startTime;
    }

    // Post-test cleanup
    await CiCdIntegration.postTestCleanup(result.summary);

    console.log(`🎭 Test orchestration completed in ${result.totalDuration}ms`);
    return result;
  }

  /**
   * Execute suites in parallel
   */
  private static async executeInParallel(
    suiteNames: string[],
    options: ExecutionOptions,
  ): Promise<SuiteResult[]> {
    console.log(`⚡ Executing ${suiteNames.length} suites in parallel`);

    const maxConcurrency = options.maxConcurrency || 4;
    const results: SuiteResult[] = [];

    // Execute in batches to control concurrency
    for (let i = 0; i < suiteNames.length; i += maxConcurrency) {
      const batch = suiteNames.slice(i, i + maxConcurrency);
      const batchPromises = batch.map((suiteName) =>
        TestOrchestrator.executeSuite(suiteName, options),
      );
      const batchResults = await Promise.allSettled(batchPromises);

      batchResults.forEach((result, index) => {
        if (result.status === "fulfilled") {
          results.push(result.value);
        } else {
          results.push({
            name: batch[index],
            success: false,
            duration: 0,
            testCount: 0,
            passCount: 0,
            failCount: 0,
            skipCount: 0,
            error: result.reason?.message || String(result.reason),
          });
        }
      });
    }

    return results;
  }

  /**
   * Execute suites sequentially
   */
  private static async executeSequentially(
    suiteNames: string[],
    options: ExecutionOptions,
  ): Promise<SuiteResult[]> {
    console.log(`📝 Executing ${suiteNames.length} suites sequentially`);

    const results: SuiteResult[] = [];

    for (const suiteName of suiteNames) {
      const result = await TestOrchestrator.executeSuite(suiteName, options);
      results.push(result);

      // Stop on first failure if fail-fast is enabled
      if (options.failFast && !result.success) {
        console.log("🛑 Stopping execution due to failure (fail-fast enabled)");
        break;
      }
    }

    return results;
  }

  /**
   * Execute a single test suite
   */
  private static async executeSuite(
    suiteName: string,
    options: ExecutionOptions,
  ): Promise<SuiteResult> {
    const suite = TestOrchestrator.testSuites.get(suiteName);
    if (!suite) {
      return {
        name: suiteName,
        success: false,
        duration: 0,
        testCount: 0,
        passCount: 0,
        failCount: 0,
        skipCount: 0,
        error: `Suite not found: ${suiteName}`,
      };
    }

    console.log(`🧪 Executing suite: ${suiteName}`);
    const startTime = Date.now();

    try {
      const result = await suite.execute(options);

      // Record execution for analytics
      TestAnalytics.recordExecution({
        suiteName,
        testCount: result.testCount,
        passCount: result.passCount,
        failCount: result.failCount,
        skipCount: result.skipCount,
        duration: result.duration,
        avgResponseTime: result.avgResponseTime,
        errors: result.error ? [result.error] : [],
      });

      return result;
    } catch (error) {
      const duration = Date.now() - startTime;
      const errorResult: SuiteResult = {
        name: suiteName,
        success: false,
        duration,
        testCount: 0,
        passCount: 0,
        failCount: 1,
        skipCount: 0,
        error: error instanceof Error ? error.message : String(error),
      };

      // Record failed execution
      TestAnalytics.recordExecution({
        suiteName,
        testCount: 0,
        passCount: 0,
        failCount: 1,
        skipCount: 0,
        duration,
        errors: [errorResult.error!],
      });

      return errorResult;
    }
  }

  /**
   * Calculate overall summary from suite results
   */
  private static calculateSummary(
    suiteResults: SuiteResult[],
  ): TestExecutionSummary {
    return suiteResults.reduce(
      (summary, suite) => ({
        totalTests: summary.totalTests + suite.testCount,
        passCount: summary.passCount + suite.passCount,
        failCount: summary.failCount + suite.failCount,
        skipCount: summary.skipCount + suite.skipCount,
        duration: summary.duration + suite.duration,
        avgResponseTime: suite.avgResponseTime
          ? (summary.avgResponseTime + suite.avgResponseTime) / 2
          : summary.avgResponseTime,
        errors: suite.error ? [...summary.errors, suite.error] : summary.errors,
      }),
      {
        totalTests: 0,
        passCount: 0,
        failCount: 0,
        skipCount: 0,
        duration: 0,
        avgResponseTime: 0,
        errors: [],
      },
    );
  }

  /**
   * Get registered suites
   */
  static getRegisteredSuites(): string[] {
    return Array.from(TestOrchestrator.testSuites.keys());
  }
}

// Deployment Integration
export class DeploymentIntegration {
  /**
   * Trigger deployment based on test results
   */
  static async triggerDeployment(
    testResults: TestExecutionSummary,
    environment: string,
  ): Promise<DeploymentResult> {
    console.log(`🚀 Triggering deployment to ${environment}`);

    const result: DeploymentResult = {
      success: false,
      environment,
      duration: 0,
      steps: [],
    };

    const startTime = Date.now();

    try {
      // Validate deployment conditions
      if (testResults.failCount > 0) {
        throw new Error(
          `Deployment blocked: ${testResults.failCount} tests failed`,
        );
      }

      // Execute deployment steps
      result.steps.push(
        await DeploymentIntegration.executeDeploymentStep(
          "validate",
          async () => {
            await DeploymentIntegration.validateDeploymentConditions(
              testResults,
              environment,
            );
          },
        ),
      );

      result.steps.push(
        await DeploymentIntegration.executeDeploymentStep(
          "deploy",
          async () => {
            await DeploymentIntegration.performDeployment(environment);
          },
        ),
      );

      result.steps.push(
        await DeploymentIntegration.executeDeploymentStep(
          "verify",
          async () => {
            await DeploymentIntegration.verifyDeployment(environment);
          },
        ),
      );

      result.success = result.steps.every((step) => step.success);
      result.duration = Date.now() - startTime;

      console.log(`✅ Deployment to ${environment} completed successfully`);
    } catch (error) {
      result.success = false;
      result.error = error instanceof Error ? error.message : String(error);
      result.duration = Date.now() - startTime;

      console.error(`❌ Deployment to ${environment} failed: ${result.error}`);
    }

    return result;
  }

  private static async executeDeploymentStep(
    stepName: string,
    stepFunction: () => Promise<void>,
  ): Promise<DeploymentStep> {
    const startTime = Date.now();

    try {
      await stepFunction();
      return {
        name: stepName,
        success: true,
        duration: Date.now() - startTime,
      };
    } catch (error) {
      return {
        name: stepName,
        success: false,
        duration: Date.now() - startTime,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  private static async validateDeploymentConditions(
    _testResults: TestExecutionSummary,
    environment: string,
  ): Promise<void> {
    // Add deployment validation logic here
    console.log(`Validating deployment conditions for ${environment}`);
  }

  private static async performDeployment(environment: string): Promise<void> {
    // Add actual deployment logic here
    console.log(`Performing deployment to ${environment}`);

    // Simulate deployment
    await new Promise((resolve) => setTimeout(resolve, 2000));
  }

  private static async verifyDeployment(environment: string): Promise<void> {
    // Add deployment verification logic here
    console.log(`Verifying deployment to ${environment}`);
  }
}

// Type definitions
export interface CiConfig {
  environment: string;
  parallel: {
    enabled: boolean;
    maxConcurrency: number;
    timeoutMultiplier: number;
  };
  qualityGates: {
    minCoverage: number;
    maxFailureRate: number;
    maxResponseTime: number;
    requiredHealthChecks: string[];
  };
  notifications: {
    enabled: boolean;
    webhookUrl?: string;
    channels: {
      success: string;
      failure: string;
      coverage: string;
    };
  };
  artifacts: {
    enabled: boolean;
    retentionDays: number;
    paths: string[];
  };
  deployment: {
    enabled: boolean;
    environments: string[];
    requiresApproval: string[];
  };
}

export interface PreTestResult {
  success: boolean;
  steps: CiStep[];
  duration: number;
  error?: string;
}

export interface PostTestResult {
  success: boolean;
  steps: CiStep[];
  duration: number;
  reports: {
    coverage?: any;
    analytics?: any;
  };
  error?: string;
}

export interface CiStep {
  name: string;
  success: boolean;
  duration: number;
  error?: string;
}

export interface QualityGateResult {
  passed: boolean;
  failures: string[];
  details: Record<string, any>;
}

export interface TestExecutionSummary {
  totalTests: number;
  passCount: number;
  failCount: number;
  skipCount: number;
  duration: number;
  avgResponseTime?: number;
  errors: string[];
}

export interface TestSuite {
  name: string;
  execute: (options: ExecutionOptions) => Promise<SuiteResult>;
}

export interface ExecutionOptions {
  parallel?: boolean;
  maxConcurrency?: number;
  failFast?: boolean;
  timeout?: number;
  suites?: string[];
  tags?: string[];
}

export interface SuiteResult {
  name: string;
  success: boolean;
  duration: number;
  testCount: number;
  passCount: number;
  failCount: number;
  skipCount: number;
  avgResponseTime?: number;
  error?: string;
}

export interface OrchestrationResult {
  success: boolean;
  suites: SuiteResult[];
  totalDuration: number;
  summary: TestExecutionSummary;
  error?: string;
}

export interface DeploymentResult {
  success: boolean;
  environment: string;
  duration: number;
  steps: DeploymentStep[];
  error?: string;
}

export interface DeploymentStep {
  name: string;
  success: boolean;
  duration: number;
  error?: string;
}
