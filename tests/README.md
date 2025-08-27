# E2E Testing Framework

Comprehensive end-to-end testing framework for md-roam-server using TypeScript + Vitest + SuperTest.

## Overview

This E2E testing framework provides comprehensive test coverage for all API endpoints and functionality of the md-roam-server. It includes automated server lifecycle management, test data fixtures, and performance validation.

## Technologies

- **TypeScript** - Type-safe test development
- **Vitest** - Fast test runner with built-in coverage
- **SuperTest** - HTTP API testing library
- **Node.js** - Test runtime environment

## Project Structure

```
tests/
├── README.md                    # This file
├── package.json                 # Dependencies and scripts
├── tsconfig.json               # TypeScript configuration
├── vitest.config.ts            # Vitest test configuration
├── utils/                      # Test utilities
│   ├── testSetup.ts            # Server lifecycle management
│   ├── apiHelpers.ts           # API wrapper functions
│   └── types.ts                # TypeScript type definitions
├── fixtures/                   # Test data
│   └── testData.ts             # Predefined test scenarios
└── e2e/                        # Test suites
    ├── nodes.test.ts           # Node CRUD operations
    ├── search.test.ts          # Search functionality
    ├── files.test.ts           # File operations
    ├── server.test.ts          # Server health and stats
    └── metadata.test.ts        # Tags and metadata
```

## Quick Start

### 1. Setup Dependencies

```bash
# From project root
make e2e-setup
```

This installs all required npm dependencies in the `tests/` directory.

### 2. Run Tests

```bash
# Run complete E2E test suite
make e2e

# Run tests in watch mode (for development)
make e2e-watch

# Run tests with coverage report
make e2e-coverage
```

### Manual Test Execution

```bash
# Navigate to tests directory
cd tests

# Run all tests
npm test

# Run specific test file
npm test nodes.test.ts

# Run tests in watch mode
npm run test:watch

# Run with coverage
npm run test:coverage
```

## Test Suites

### Node Operations (`nodes.test.ts`)
- **Create nodes** with both Markdown and Org formats
- **Retrieve nodes** by ID with proper error handling
- **Update nodes** with partial data modifications
- **Delete nodes** with cleanup verification
- **List all nodes** with pagination support
- **Error handling** for invalid inputs and missing resources

### Search Functionality (`search.test.ts`)
- **Content search** across titles and body text
- **Multi-language support** including Japanese content
- **Case-insensitive matching**
- **Empty query handling**
- **Performance benchmarks** with response time validation
- **Result structure validation** with proper metadata

### File Operations (`files.test.ts`)
- **File listing** from database and filesystem
- **File metadata** including size, modification time, and paths
- **Multi-format support** for both .md and .org files
- **Performance testing** for file operations
- **Path validation** and security checks

### Server Health (`server.test.ts`)
- **Health check** endpoint validation
- **Database statistics** with comprehensive metrics
- **Database synchronization** operations
- **Server stability** under concurrent load
- **Performance monitoring** with response time thresholds

### Metadata Management (`metadata.test.ts`)
- **Tag listing** with count and node associations
- **Japanese tag support** with Unicode handling
- **Tag-based node retrieval** (if implemented)
- **Aliases and references** endpoint testing
- **Metadata consistency** across node operations

## Configuration

### Test Environment Variables

```bash
TEST_SERVER_URL=http://localhost:8080  # API server endpoint
TEST_UI_URL=http://localhost:35901     # UI server endpoint
```

### Timeouts and Thresholds

The framework includes performance thresholds:
- Node creation: 5 seconds
- Node retrieval: 1 second
- Search operations: 2 seconds
- File operations: 1 second
- Health checks: 500ms

### Test Data Fixtures

Pre-defined test scenarios in `fixtures/testData.ts`:
- **Valid nodes** with comprehensive metadata
- **Invalid payloads** for error testing
- **Multi-language content** including Japanese
- **Performance test data** generators
- **Edge cases** with special characters and formats

## Test Utilities

### ApiHelpers Class
Provides type-safe wrappers for all API endpoints:
```typescript
// Node operations
await ApiHelpers.createNode(nodeData);
await ApiHelpers.getNode(nodeId);
await ApiHelpers.updateNode(nodeId, updates);
await ApiHelpers.deleteNode(nodeId);

// Search and discovery
await ApiHelpers.searchNodes(query);
await ApiHelpers.getTags();
await ApiHelpers.getStats();

// Response validation
ApiHelpers.expectSuccessResponse(response);
ApiHelpers.expectErrorResponse(response, 404);
```

### TestCleanup Class
Automatic test data cleanup:
```typescript
// Create tracked test node (auto-cleanup)
const node = await TestCleanup.createTestNode(nodeData);

// Manual cleanup
await TestCleanup.cleanupNodes();
```

### TestServerManager Class
Automated server lifecycle:
- Checks if server is already running
- Starts server if needed using project scripts
- Waits for server to be healthy before tests
- Manages server shutdown after tests

## Integration with Make

The framework integrates with the project's Makefile:

```bash
make e2e           # Run complete test suite
make e2e-watch     # Development mode with auto-rerun
make e2e-coverage  # Generate coverage reports
make e2e-setup     # Install dependencies
make e2e-clean     # Clean test environment
```

## Development Workflow

### Adding New Tests

1. Create test file in `e2e/` directory
2. Import required utilities and types
3. Use `describe` and `it` blocks for test organization
4. Use `TestCleanup.createTestNode()` for test data
5. Use `ApiHelpers` for API interactions
6. Include proper assertions and error handling

Example:
```typescript
import { describe, it, expect } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';

describe('New Feature Tests', () => {
  it('should handle new endpoint', async () => {
    const testNode = await TestCleanup.createTestNode();
    const response = await ApiHelpers.someNewEndpoint(testNode.id);
    
    expect(response.status).toBe(200);
    ApiHelpers.expectSuccessResponse(response);
  });
});
```

### Test Data Management

1. Add new fixtures to `fixtures/testData.ts`
2. Include both valid and invalid test cases
3. Support multi-language content when applicable
4. Provide performance test data generators

### Type Definitions

Update `utils/types.ts` with new API response types:
```typescript
export interface NewEndpointResponse extends SuccessResponse {
  data: SomeDataType[];
  metadata?: ResponseMetadata;
}
```

## Coverage Reports

Coverage reports are generated in `tests/coverage/`:
- **HTML report**: `coverage/index.html`
- **JSON data**: `coverage/coverage.json`
- **Text summary**: Console output

Coverage includes:
- Line coverage
- Function coverage
- Branch coverage
- Statement coverage

## Performance Monitoring

The framework includes performance assertions:
- Response time validation
- Concurrent request handling
- Memory usage monitoring (via test execution time)
- Database operation efficiency

## Troubleshooting

### Common Issues

**Server startup fails:**
```bash
# Check if ports are already in use
make status
make stop
make e2e
```

**Test timeouts:**
- Increase timeout in `vitest.config.ts`
- Check server resources and performance
- Verify test data isn't too large

**Database issues:**
```bash
# Clean and reset database
make reset
make e2e
```

**NPM dependency issues:**
```bash
make e2e-clean
make e2e-setup
```

### Debug Mode

Run tests with debug output:
```bash
cd tests
DEBUG=1 npm test
```

### Manual Server Testing

Test server without framework:
```bash
make dev
curl http://localhost:8080/stats
curl http://localhost:8080/nodes
```

## Contributing

1. Write tests for new API endpoints
2. Include error cases and edge conditions
3. Add performance benchmarks for new features
4. Update type definitions for new response formats
5. Include Japanese/Unicode test cases when applicable
6. Maintain test isolation and cleanup

## Future Enhancements

- **API versioning** support in test framework
- **Load testing** with stress scenarios  
- **Integration testing** with org-roam-ui
- **Database migration** testing
- **Configuration validation** testing
- **Security testing** for input validation