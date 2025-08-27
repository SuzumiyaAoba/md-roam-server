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
    if (this.isServerRunning) {
      console.log('ℹ️  Server already running, skipping startup');
      return;
    }

    try {
      console.log('🚀 Starting md-roam server for tests...');
      
      // Check if server is already running
      const isAlreadyRunning = await this.checkServerHealth();
      if (isAlreadyRunning) {
        console.log('✅ Server already running and healthy');
        this.isServerRunning = true;
        return;
      }

      // Navigate to project root and start server
      const projectRoot = join(process.cwd(), '..');
      process.chdir(projectRoot);

      // Start server using existing script
      execSync('./start.sh', { stdio: 'pipe' });
      
      // Wait for server to start
      await this.waitForServer();
      this.isServerRunning = true;
      
      console.log('✅ Server started successfully');
    } catch (error) {
      console.error('❌ Failed to start server:', error);
      throw error;
    }
  }

  async stopServer(): Promise<void> {
    if (!this.isServerRunning) {
      return;
    }

    try {
      console.log('🛑 Stopping md-roam server...');
      
      const projectRoot = join(process.cwd(), '..');
      process.chdir(projectRoot);
      
      execSync('./stop.sh', { stdio: 'pipe' });
      this.isServerRunning = false;
      
      console.log('✅ Server stopped successfully');
    } catch (error) {
      console.error('❌ Failed to stop server:', error);
      // Continue anyway as server might have been manually stopped
    }
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