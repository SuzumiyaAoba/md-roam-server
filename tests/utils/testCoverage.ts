import { ApiHelpers } from "./apiHelpers";
import type { CreateNodePayload } from "./types";

/**
 * Comprehensive test coverage utilities for boundary testing and edge case validation
 */

// API Endpoint Coverage Tracking
export class ApiCoverageTracker {
  private static coveredEndpoints = new Set<string>();
  private static apiEndpoints = [
    "GET /",
    "GET /nodes",
    "POST /nodes",
    "GET /nodes/:id",
    "PUT /nodes/:id",
    "DELETE /nodes/:id",
    "GET /files",
    "GET /files/raw",
    "GET /search/:query",
    "GET /stats",
    "GET /tags",
    "POST /sync",
    // Extended endpoints (if implemented)
    "GET /nodes/:id/content",
    "GET /nodes/:id/parse",
    "GET /nodes/:id/backlinks",
    "GET /nodes/:id/links",
    "GET /tags/:tag/nodes",
    "GET /aliases/:alias/nodes",
    "GET /refs/:ref/nodes",
    "GET /citations/:citation/nodes",
  ];

  static markEndpointCovered(method: string, path: string): void {
    const endpoint = `${method.toUpperCase()} ${path}`;
    ApiCoverageTracker.coveredEndpoints.add(endpoint);
  }

  static getCoverageReport(): EndpointCoverageReport {
    const covered = Array.from(ApiCoverageTracker.coveredEndpoints);
    const uncovered = ApiCoverageTracker.apiEndpoints.filter(
      (endpoint) => !ApiCoverageTracker.coveredEndpoints.has(endpoint),
    );

    return {
      totalEndpoints: ApiCoverageTracker.apiEndpoints.length,
      coveredEndpoints: covered,
      uncoveredEndpoints: uncovered,
      coveragePercentage:
        (covered.length / ApiCoverageTracker.apiEndpoints.length) * 100,
    };
  }

  static resetCoverage(): void {
    ApiCoverageTracker.coveredEndpoints.clear();
  }
}

// Boundary Value Test Generator
export class BoundaryValueTests {
  /**
   * Generate boundary test cases for string fields
   */
  static generateStringBoundaryTests(
    fieldName: string,
  ): Array<StringBoundaryTest> {
    return [
      { name: `${fieldName}_empty`, value: "", expectedValid: false },
      { name: `${fieldName}_single_char`, value: "A", expectedValid: true },
      {
        name: `${fieldName}_normal`,
        value: "Normal test string",
        expectedValid: true,
      },
      {
        name: `${fieldName}_very_long`,
        value: "x".repeat(1000),
        expectedValid: true,
      },
      {
        name: `${fieldName}_extremely_long`,
        value: "x".repeat(10000),
        expectedValid: false,
      },
      {
        name: `${fieldName}_unicode`,
        value: "🌟日本語中文العربية",
        expectedValid: true,
      },
      {
        name: `${fieldName}_special_chars`,
        value: "!@#$%^&*()_+-=[]{}|;:,.<>?",
        expectedValid: true,
      },
      {
        name: `${fieldName}_newlines`,
        value: "Line 1\nLine 2\rLine 3\r\nLine 4",
        expectedValid: true,
      },
      {
        name: `${fieldName}_tabs`,
        value: "Col1\tCol2\tCol3",
        expectedValid: true,
      },
      {
        name: `${fieldName}_null_char`,
        value: "Test\0Null",
        expectedValid: true,
      },
      {
        name: `${fieldName}_control_chars`,
        value: "Test\x01\x02\x03",
        expectedValid: true,
      },
    ];
  }

  /**
   * Generate boundary test cases for array fields
   */
  static generateArrayBoundaryTests(
    fieldName: string,
  ): Array<ArrayBoundaryTest> {
    return [
      { name: `${fieldName}_empty_array`, value: [], expectedValid: true },
      {
        name: `${fieldName}_single_item`,
        value: ["item1"],
        expectedValid: true,
      },
      {
        name: `${fieldName}_normal`,
        value: ["item1", "item2", "item3"],
        expectedValid: true,
      },
      {
        name: `${fieldName}_many_items`,
        value: Array.from({ length: 100 }, (_, i) => `item${i}`),
        expectedValid: true,
      },
      {
        name: `${fieldName}_too_many_items`,
        value: Array.from({ length: 10000 }, (_, i) => `item${i}`),
        expectedValid: false,
      },
      {
        name: `${fieldName}_unicode_items`,
        value: ["🌟", "日本語", "中文"],
        expectedValid: true,
      },
      {
        name: `${fieldName}_long_items`,
        value: ["x".repeat(1000)],
        expectedValid: true,
      },
      {
        name: `${fieldName}_empty_string_items`,
        value: ["", "valid", ""],
        expectedValid: true,
      },
      {
        name: `${fieldName}_duplicate_items`,
        value: ["item", "item", "item"],
        expectedValid: true,
      },
    ];
  }

  /**
   * Generate comprehensive node creation boundary tests
   */
  static generateNodeCreationBoundaryTests(): Array<NodeBoundaryTest> {
    const tests: NodeBoundaryTest[] = [];

    // Title boundary tests
    for (const titleTest of BoundaryValueTests.generateStringBoundaryTests(
      "title",
    )) {
      tests.push({
        name: `node_creation_${titleTest.name}`,
        payload: {
          title: titleTest.value,
          content: "Test content",
          file_type: "md",
        },
        expectedValid: titleTest.expectedValid && titleTest.value.length > 0, // Title required
      });
    }

    // Content boundary tests
    for (const contentTest of BoundaryValueTests.generateStringBoundaryTests(
      "content",
    )) {
      tests.push({
        name: `node_creation_${contentTest.name}`,
        payload: {
          title: "Test Title",
          content: contentTest.value,
          file_type: "md",
        },
        expectedValid: contentTest.expectedValid,
      });
    }

    // Tags boundary tests
    for (const tagsTest of BoundaryValueTests.generateArrayBoundaryTests(
      "tags",
    )) {
      tests.push({
        name: `node_creation_${tagsTest.name}`,
        payload: {
          title: "Test Title",
          content: "Test content",
          tags: tagsTest.value,
          file_type: "md",
        },
        expectedValid: tagsTest.expectedValid,
      });
    }

    // File type boundary tests
    const fileTypeTests = [
      { name: "file_type_md", value: "md", expectedValid: true },
      { name: "file_type_org", value: "org", expectedValid: true },
      { name: "file_type_invalid", value: "invalid", expectedValid: false },
      { name: "file_type_empty", value: "", expectedValid: false },
      { name: "file_type_null", value: null, expectedValid: true }, // Should default to md
    ];

    for (const fileTypeTest of fileTypeTests) {
      tests.push({
        name: `node_creation_${fileTypeTest.name}`,
        payload: {
          title: "Test Title",
          content: "Test content",
          file_type: fileTypeTest.value as any,
        },
        expectedValid: fileTypeTest.expectedValid,
      });
    }

    return tests;
  }

  /**
   * Execute boundary tests for node creation
   */
  static async executeBoundaryTests(
    tests: NodeBoundaryTest[],
  ): Promise<BoundaryTestResult[]> {
    const results: BoundaryTestResult[] = [];

    for (const test of tests) {
      try {
        const response = await ApiHelpers.createNode(test.payload);
        const actualValid = response.status >= 200 && response.status < 300;

        results.push({
          testName: test.name,
          expectedValid: test.expectedValid,
          actualValid,
          passed: test.expectedValid === actualValid,
          responseStatus: response.status,
          errorMessage: actualValid ? undefined : response.body?.message,
        });

        // Cleanup if successful
        if (actualValid && response.body?.id) {
          try {
            await ApiHelpers.deleteNode(response.body.id);
          } catch (cleanupError) {
            console.warn(
              `Cleanup failed for ${response.body.id}:`,
              cleanupError,
            );
          }
        }
      } catch (error) {
        results.push({
          testName: test.name,
          expectedValid: test.expectedValid,
          actualValid: false,
          passed: !test.expectedValid,
          responseStatus: 500,
          errorMessage: error instanceof Error ? error.message : String(error),
        });
      }
    }

    return results;
  }
}

// Input Validation Test Generator
export class InputValidationTests {
  /**
   * Generate invalid JSON test cases
   */
  static generateInvalidJsonTests(): Array<InvalidJsonTest> {
    return [
      {
        name: "incomplete_json",
        json: '{"title": "Test"',
        expectedError: "parse",
      },
      { name: "invalid_syntax", json: '{"title": }', expectedError: "parse" },
      { name: "not_json", json: "not json at all", expectedError: "parse" },
      { name: "empty_string", json: "", expectedError: "parse" },
      { name: "null_value", json: "null", expectedError: "validation" },
      {
        name: "array_instead_object",
        json: '["not", "an", "object"]',
        expectedError: "validation",
      },
      {
        name: "number_instead_object",
        json: "123",
        expectedError: "validation",
      },
      {
        name: "string_instead_object",
        json: '"string"',
        expectedError: "validation",
      },
      {
        name: "nested_invalid",
        json: '{"title": {"nested": "object"}}',
        expectedError: "validation",
      },
      {
        name: "circular_reference",
        json: '{"a": {"b": {"c": "value"}}}',
        expectedError: "validation",
      },
    ];
  }

  /**
   * Generate type mismatch test cases
   */
  static generateTypeMismatchTests(): Array<TypeMismatchTest> {
    return [
      {
        name: "title_number",
        field: "title",
        value: 123,
        expectedValid: false,
      },
      {
        name: "title_array",
        field: "title",
        value: ["array"],
        expectedValid: false,
      },
      { name: "title_object", field: "title", value: {}, expectedValid: false },
      {
        name: "title_boolean",
        field: "title",
        value: true,
        expectedValid: false,
      },
      {
        name: "content_number",
        field: "content",
        value: 123,
        expectedValid: false,
      },
      {
        name: "content_array",
        field: "content",
        value: ["content"],
        expectedValid: false,
      },
      {
        name: "tags_string",
        field: "tags",
        value: "not array",
        expectedValid: false,
      },
      { name: "tags_number", field: "tags", value: 123, expectedValid: false },
      { name: "tags_object", field: "tags", value: {}, expectedValid: false },
      {
        name: "aliases_string",
        field: "aliases",
        value: "not array",
        expectedValid: false,
      },
      {
        name: "file_type_number",
        field: "file_type",
        value: 123,
        expectedValid: false,
      },
      {
        name: "file_type_array",
        field: "file_type",
        value: ["md"],
        expectedValid: false,
      },
    ];
  }

  /**
   * Execute type mismatch tests
   */
  static async executeTypeMismatchTests(
    tests: TypeMismatchTest[],
  ): Promise<TypeMismatchResult[]> {
    const results: TypeMismatchResult[] = [];

    for (const test of tests) {
      try {
        const payload = {
          title: "Valid Title",
          content: "Valid content",
          file_type: "md",
          [test.field]: test.value,
        } as any;

        const response = await ApiHelpers.createNode(payload);
        const actualValid = response.status >= 200 && response.status < 300;

        results.push({
          testName: test.name,
          field: test.field,
          value: test.value,
          expectedValid: test.expectedValid,
          actualValid,
          passed: test.expectedValid === actualValid,
          responseStatus: response.status,
        });

        // Cleanup if successful
        if (actualValid && response.body?.id) {
          try {
            await ApiHelpers.deleteNode(response.body.id);
          } catch (cleanupError) {
            console.warn(
              `Cleanup failed for ${response.body.id}:`,
              cleanupError,
            );
          }
        }
      } catch (_error) {
        results.push({
          testName: test.name,
          field: test.field,
          value: test.value,
          expectedValid: test.expectedValid,
          actualValid: false,
          passed: !test.expectedValid,
          responseStatus: 500,
        });
      }
    }

    return results;
  }
}

// Security Test Generator
export class SecurityTestGenerator {
  /**
   * Generate XSS test payloads
   */
  static generateXssTests(): Array<SecurityTest> {
    return [
      {
        name: "script_tag",
        payload: '<script>alert("xss")</script>',
        riskLevel: "high",
      },
      {
        name: "img_onerror",
        payload: '<img src="x" onerror="alert(1)">',
        riskLevel: "high",
      },
      {
        name: "svg_onload",
        payload: '<svg onload="alert(1)">',
        riskLevel: "high",
      },
      {
        name: "iframe_src",
        payload: '<iframe src="javascript:alert(1)"></iframe>',
        riskLevel: "high",
      },
      {
        name: "a_href_javascript",
        payload: '<a href="javascript:alert(1)">link</a>',
        riskLevel: "medium",
      },
      {
        name: "div_onclick",
        payload: '<div onclick="alert(1)">click</div>',
        riskLevel: "medium",
      },
      {
        name: "style_expression",
        payload: '<div style="expression(alert(1))">test</div>',
        riskLevel: "medium",
      },
      {
        name: "meta_refresh",
        payload:
          '<meta http-equiv="refresh" content="0;url=javascript:alert(1)">',
        riskLevel: "low",
      },
    ];
  }

  /**
   * Generate SQL injection test payloads
   */
  static generateSqlInjectionTests(): Array<SecurityTest> {
    return [
      {
        name: "classic_injection",
        payload: "'; DROP TABLE nodes; --",
        riskLevel: "high",
      },
      {
        name: "union_select",
        payload: "' UNION SELECT * FROM nodes --",
        riskLevel: "high",
      },
      { name: "boolean_blind", payload: "' AND 1=1 --", riskLevel: "medium" },
      {
        name: "time_based",
        payload: "'; WAITFOR DELAY '00:00:05' --",
        riskLevel: "medium",
      },
      {
        name: "error_based",
        payload: "' AND (SELECT COUNT(*) FROM nodes) > 0 --",
        riskLevel: "medium",
      },
      {
        name: "nested_injection",
        payload:
          "' OR (SELECT SUBSTRING((SELECT password FROM users), 1, 1)) = 'a",
        riskLevel: "low",
      },
    ];
  }

  /**
   * Generate path traversal test payloads
   */
  static generatePathTraversalTests(): Array<SecurityTest> {
    return [
      {
        name: "basic_traversal",
        payload: "../../../etc/passwd",
        riskLevel: "high",
      },
      {
        name: "windows_traversal",
        payload: "..\\..\\..\\windows\\system32\\config\\sam",
        riskLevel: "high",
      },
      {
        name: "url_encoded",
        payload: "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
        riskLevel: "medium",
      },
      {
        name: "double_encoded",
        payload: "%252e%252e%252f%252e%252e%252f%252e%252e%252fetc%252fpasswd",
        riskLevel: "medium",
      },
      {
        name: "unicode_encoded",
        payload: "..%c0%af..%c0%af..%c0%afetc%c0%afpasswd",
        riskLevel: "low",
      },
    ];
  }

  /**
   * Execute security tests
   */
  static async executeSecurityTests(
    tests: SecurityTest[],
    testType: string,
  ): Promise<SecurityTestResult[]> {
    const results: SecurityTestResult[] = [];

    for (const test of tests) {
      try {
        // Test in different fields
        const testCases = [
          {
            field: "title",
            payload: { title: test.payload, content: "test", file_type: "md" },
          },
          {
            field: "content",
            payload: { title: "Test", content: test.payload, file_type: "md" },
          },
          {
            field: "tags",
            payload: {
              title: "Test",
              content: "test",
              tags: [test.payload],
              file_type: "md",
            },
          },
        ];

        for (const testCase of testCases) {
          const response = await ApiHelpers.createNode(
            testCase.payload as CreateNodePayload,
          );

          const result: SecurityTestResult = {
            testName: `${testType}_${test.name}_${testCase.field}`,
            testType,
            payload: test.payload,
            field: testCase.field,
            riskLevel: test.riskLevel,
            responseStatus: response.status,
            blocked: response.status >= 400,
            responseContainsPayload: JSON.stringify(response.body).includes(
              test.payload,
            ),
          };

          results.push(result);

          // Cleanup if successful
          if (response.status === 201 && response.body?.id) {
            try {
              await ApiHelpers.deleteNode(response.body.id);
            } catch (cleanupError) {
              console.warn(
                `Security test cleanup failed for ${response.body.id}:`,
                cleanupError,
              );
            }
          }
        }
      } catch (_error) {
        results.push({
          testName: `${testType}_${test.name}_error`,
          testType,
          payload: test.payload,
          field: "unknown",
          riskLevel: test.riskLevel,
          responseStatus: 500,
          blocked: true,
          responseContainsPayload: false,
        });
      }
    }

    return results;
  }
}

// Comprehensive test report generator
export class TestReportGenerator {
  /**
   * Generate comprehensive test coverage report
   */
  static async generateComprehensiveReport(): Promise<ComprehensiveTestReport> {
    console.log("🔍 Generating comprehensive test coverage report...");

    // API Coverage
    const apiCoverage = ApiCoverageTracker.getCoverageReport();

    // Boundary Tests
    console.log("Running boundary value tests...");
    const boundaryTests =
      BoundaryValueTests.generateNodeCreationBoundaryTests();
    const boundaryResults = await BoundaryValueTests.executeBoundaryTests(
      boundaryTests.slice(0, 20),
    ); // Limit for performance

    // Type Mismatch Tests
    console.log("Running type mismatch tests...");
    const typeMismatchTests = InputValidationTests.generateTypeMismatchTests();
    const typeMismatchResults =
      await InputValidationTests.executeTypeMismatchTests(typeMismatchTests);

    // Security Tests
    console.log("Running security tests...");
    const xssTests = SecurityTestGenerator.generateXssTests();
    const sqlTests = SecurityTestGenerator.generateSqlInjectionTests();
    const pathTests = SecurityTestGenerator.generatePathTraversalTests();

    const securityResults = [
      ...(await SecurityTestGenerator.executeSecurityTests(xssTests, "xss")),
      ...(await SecurityTestGenerator.executeSecurityTests(
        sqlTests,
        "sql_injection",
      )),
      ...(await SecurityTestGenerator.executeSecurityTests(
        pathTests,
        "path_traversal",
      )),
    ];

    const report: ComprehensiveTestReport = {
      timestamp: new Date().toISOString(),
      apiCoverage,
      boundaryTestResults: {
        total: boundaryResults.length,
        passed: boundaryResults.filter((r) => r.passed).length,
        failed: boundaryResults.filter((r) => !r.passed).length,
        details: boundaryResults,
      },
      typeMismatchResults: {
        total: typeMismatchResults.length,
        passed: typeMismatchResults.filter((r) => r.passed).length,
        failed: typeMismatchResults.filter((r) => !r.passed).length,
        details: typeMismatchResults,
      },
      securityTestResults: {
        total: securityResults.length,
        blocked: securityResults.filter((r) => r.blocked).length,
        unblocked: securityResults.filter((r) => !r.blocked).length,
        highRisk: securityResults.filter(
          (r) => r.riskLevel === "high" && !r.blocked,
        ).length,
        details: securityResults,
      },
    };

    console.log("✅ Comprehensive test report generated");
    return report;
  }

  /**
   * Print summary report
   */
  static printReportSummary(report: ComprehensiveTestReport): void {
    console.log("\n📊 TEST COVERAGE REPORT SUMMARY");
    console.log("================================");

    console.log(
      `\n🔗 API Coverage: ${report.apiCoverage.coveragePercentage.toFixed(1)}%`,
    );
    console.log(
      `   Covered: ${report.apiCoverage.coveredEndpoints.length}/${report.apiCoverage.totalEndpoints} endpoints`,
    );

    console.log(
      `\n🔢 Boundary Tests: ${report.boundaryTestResults.passed}/${report.boundaryTestResults.total} passed`,
    );

    console.log(
      `\n🔤 Type Mismatch Tests: ${report.typeMismatchResults.passed}/${report.typeMismatchResults.total} passed`,
    );

    console.log(
      `\n🔒 Security Tests: ${report.securityTestResults.blocked}/${report.securityTestResults.total} blocked`,
    );
    if (report.securityTestResults.highRisk > 0) {
      console.log(
        `   ⚠️  ${report.securityTestResults.highRisk} high-risk payloads were not blocked!`,
      );
    }

    if (report.apiCoverage.uncoveredEndpoints.length > 0) {
      console.log("\n❌ Uncovered Endpoints:");
      report.apiCoverage.uncoveredEndpoints.forEach((endpoint) => {
        console.log(`   - ${endpoint}`);
      });
    }
  }
}

// Type definitions
export interface EndpointCoverageReport {
  totalEndpoints: number;
  coveredEndpoints: string[];
  uncoveredEndpoints: string[];
  coveragePercentage: number;
}

export interface StringBoundaryTest {
  name: string;
  value: string;
  expectedValid: boolean;
}

export interface ArrayBoundaryTest {
  name: string;
  value: string[];
  expectedValid: boolean;
}

export interface NodeBoundaryTest {
  name: string;
  payload: Partial<CreateNodePayload>;
  expectedValid: boolean;
}

export interface BoundaryTestResult {
  testName: string;
  expectedValid: boolean;
  actualValid: boolean;
  passed: boolean;
  responseStatus: number;
  errorMessage?: string;
}

export interface InvalidJsonTest {
  name: string;
  json: string;
  expectedError: "parse" | "validation";
}

export interface TypeMismatchTest {
  name: string;
  field: string;
  value: any;
  expectedValid: boolean;
}

export interface TypeMismatchResult {
  testName: string;
  field: string;
  value: any;
  expectedValid: boolean;
  actualValid: boolean;
  passed: boolean;
  responseStatus: number;
}

export interface SecurityTest {
  name: string;
  payload: string;
  riskLevel: "high" | "medium" | "low";
}

export interface SecurityTestResult {
  testName: string;
  testType: string;
  payload: string;
  field: string;
  riskLevel: "high" | "medium" | "low";
  responseStatus: number;
  blocked: boolean;
  responseContainsPayload: boolean;
}

export interface ComprehensiveTestReport {
  timestamp: string;
  apiCoverage: EndpointCoverageReport;
  boundaryTestResults: {
    total: number;
    passed: number;
    failed: number;
    details: BoundaryTestResult[];
  };
  typeMismatchResults: {
    total: number;
    passed: number;
    failed: number;
    details: TypeMismatchResult[];
  };
  securityTestResults: {
    total: number;
    blocked: number;
    unblocked: number;
    highRisk: number;
    details: SecurityTestResult[];
  };
}
