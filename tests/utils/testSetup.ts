import { execSync } from 'child_process';
import { existsSync } from 'fs';
import { join } from 'path';

// Global test configuration
export const TEST_CONFIG = {
  SERVER_URL: process.env.TEST_SERVER_URL || 'http://localhost:8080',
  UI_URL: process.env.TEST_UI_URL || 'http://localhost:35901',
  TIMEOUT: 30000,
  STARTUP_DELAY: 3000, // Wait time for server to fully start
} as const;

// Server management
export class TestServerManager {
  private static instance: TestServerManager;
  private serverProcess: any = null;
  private isServerRunning = false;

  static getInstance(): TestServerManager {
    if (!TestServerManager.instance) {
      TestServerManager.instance = new TestServerManager();
    }
    return TestServerManager.instance;
  }

  async startServer(): Promise<void> {
    // In global setup mode, just check if server is healthy
    const isHealthy = await this.checkServerHealth();
    if (isHealthy) {
      console.log('✅ Server already running and healthy');
      this.isServerRunning = true;
      return;
    }

    // If server is not healthy, throw an error since global setup should have started it
    throw new Error('Server is not running. Global setup may have failed.');
  }

  async stopServer(): Promise<void> {
    // In global setup mode, individual tests don't stop the server
    // Server will be stopped in global teardown
    console.log('ℹ️  Server management handled by global setup/teardown');
    this.isServerRunning = false;
  }

  private async checkServerHealth(): Promise<boolean> {
    try {
      const response = await fetch(`${TEST_CONFIG.SERVER_URL}/`);
      return response.ok;
    } catch {
      return false;
    }
  }

  private async waitForServer(): Promise<void> {
    const maxRetries = 20;
    const retryDelay = 1000;

    for (let i = 0; i < maxRetries; i++) {
      try {
        const isHealthy = await this.checkServerHealth();
        if (isHealthy) {
          return;
        }
      } catch {
        // Server not ready yet
      }
      
      await new Promise(resolve => setTimeout(resolve, retryDelay));
    }
    
    throw new Error('Server failed to start within timeout period');
  }
}

// Setup and teardown hooks
beforeAll(async () => {
  const serverManager = TestServerManager.getInstance();
  await serverManager.startServer();
}, TEST_CONFIG.TIMEOUT);

afterAll(async () => {
  const serverManager = TestServerManager.getInstance();
  await serverManager.stopServer();
}, TEST_CONFIG.TIMEOUT);