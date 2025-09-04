import { join } from "node:path";

// Global test configuration
export const TEST_CONFIG = {
  SERVER_URL: process.env.TEST_SERVER_URL || "http://localhost:3001",
  TIMEOUT: 90000, // Increased timeout to match vitest config
  STARTUP_DELAY: 3000, // Wait time for server to fully start
  TEST_ORG_ROAM_DIR: join(process.cwd(), "..", "tmp", "org-roam"), // Test org-roam directory
  TEST_CONFIG_FILE: join(process.cwd(), "config", "test-config.yml"), // Test config file
} as const;

// Server management
export class TestServerManager {
  private static instance: TestServerManager;

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
      console.log("✅ Server already running and healthy");
      console.log("📁 Test org-roam directory:", TEST_CONFIG.TEST_ORG_ROAM_DIR);
      console.log("⚙️  Test config file:", TEST_CONFIG.TEST_CONFIG_FILE);
      this.isServerRunning = true;
      return;
    }

    // If server is not healthy, throw an error since global setup should have started it
    throw new Error("Server is not running. Global setup may have failed.");
  }

  async stopServer(): Promise<void> {
    // In global setup mode, individual tests don't stop the server
    // Server will be stopped in global teardown
    console.log("ℹ️  Server management handled by global setup/teardown");
    this.isServerRunning = false;
  }

  private async checkServerHealth(): Promise<boolean> {
    try {
      const response = await fetch(`${TEST_CONFIG.SERVER_URL}/health`);
      return response.ok;
    } catch {
      return false;
    }
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
