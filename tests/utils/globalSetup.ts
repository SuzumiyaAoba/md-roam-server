import { type ChildProcess, spawn } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { join } from "node:path";

let serverProcess: ChildProcess | null = null;

// Global setup for test suite
export async function setup() {
  console.log("🚀 Global setup: Starting md-roam server...");

  try {
    const projectRoot = join(process.cwd(), "..");
    const testConfigPath = join(process.cwd(), "config", "test-config.yml");
    const tmpDir = join(projectRoot, "tmp");
    const testOrgRoamDir = join(tmpDir, "org-roam");

    // Create tmp directory and test org-roam directory
    if (!existsSync(tmpDir)) {
      mkdirSync(tmpDir, { recursive: true });
      console.log("📁 Created tmp directory:", tmpDir);
    }

    if (!existsSync(testOrgRoamDir)) {
      mkdirSync(testOrgRoamDir, { recursive: true });
      console.log("📁 Created test org-roam directory:", testOrgRoamDir);
    }

    // Check if server is already running
    const isRunning = await checkServerHealth();
    if (isRunning) {
      console.log("✅ Server already running");
      return;
    }

    // Start both servers with test configuration
    const env = {
      ...process.env,
      MD_ROAM_CONFIG_FILE: testConfigPath,
      API_PORT: "3001",
      EMACS_SERVER_URL: "http://localhost:8080",
    };

    // First start the Emacs server on port 8080 using start.sh
    console.log("🔧 Starting Emacs server on port 8080...");

    const emacsStartProcess = spawn("bash", ["./start.sh"], {
      cwd: projectRoot,
      env: { ...env, MD_ROAM_CONFIG_FILE: testConfigPath },
      stdio: ["pipe", "pipe", "pipe"],
    });

    // Wait for start.sh to complete
    await new Promise<void>((resolve, reject) => {
      emacsStartProcess.on("exit", (code) => {
        if (code === 0) {
          console.log("✅ Emacs server started successfully");
          resolve();
        } else {
          reject(new Error(`Emacs server failed to start with code ${code}`));
        }
      });

      emacsStartProcess.on("error", (error) => {
        reject(error);
      });
    });

    // Give Emacs server a moment to be ready
    await new Promise((resolve) => setTimeout(resolve, 2000));

    // Then start the Hono API server on port 3001
    console.log("🔧 Starting Hono API server on port 3001...");
    console.log("Debug - Environment variables:", {
      API_PORT: env.API_PORT,
      EMACS_SERVER_URL: env.EMACS_SERVER_URL,
    });
    serverProcess = spawn("bun", ["--watch", "src/app/api/index.ts"], {
      cwd: projectRoot,
      env,
      stdio: ["pipe", "pipe", "pipe"],
    });

    if (serverProcess.stdout) {
      serverProcess.stdout.on("data", (data) => {
        console.log(`[Server] ${data.toString().trim()}`);
      });
    }

    if (serverProcess.stderr) {
      serverProcess.stderr.on("data", (data) => {
        console.error(`[Server Error] ${data.toString().trim()}`);
      });
    }

    serverProcess.on("exit", (code) => {
      console.log(`Server process exited with code ${code}`);
      serverProcess = null;
    });

    // Wait for server to be ready
    await waitForServer();
    console.log("✅ Server started successfully with test configuration");
    console.log("📁 Test org-roam directory:", testOrgRoamDir);
  } catch (error) {
    console.error("❌ Failed to start server in global setup:", error);
    throw error;
  }
}

export async function teardown() {
  console.log("🛑 Global teardown: Stopping md-roam server...");

  try {
    // Stop server
    if (serverProcess?.pid && !serverProcess.killed) {
      console.log("🔧 Terminating server process...");
      serverProcess.kill("SIGTERM");

      // Wait a bit for graceful shutdown
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Force kill if still running
      if (serverProcess.pid && !serverProcess.killed) {
        console.log("🔧 Force killing server process...");
        serverProcess.kill("SIGKILL");
      }

      serverProcess = null;
    }

    // Also stop Emacs daemon
    console.log("🔧 Stopping Emacs daemon...");
    const _stopProcess = spawn("bash", ["./stop.sh"], {
      cwd: join(process.cwd(), ".."),
      stdio: "pipe",
    });

    await new Promise((resolve) => setTimeout(resolve, 1000));

    console.log("✅ Server stopped successfully");
  } catch (error) {
    console.error("❌ Failed to stop server in global teardown:", error);
    // Don't throw in teardown to avoid masking test failures
  }
}

async function checkServerHealth(): Promise<boolean> {
  try {
    const response = await fetch("http://localhost:3001/health");
    return response.ok;
  } catch {
    return false;
  }
}

async function waitForServer(maxAttempts = 45, delayMs = 1000): Promise<void> {
  console.log("⏳ Waiting for server to be ready...");

  for (let i = 0; i < maxAttempts; i++) {
    try {
      if (await checkServerHealth()) {
        console.log(`✅ Server is ready after ${i + 1} attempts`);
        return;
      }
    } catch (_error) {
      // Ignore connection errors while waiting
    }

    if (i % 5 === 0 && i > 0) {
      console.log(
        `⏳ Still waiting for server... (attempt ${i + 1}/${maxAttempts})`,
      );
    }

    await new Promise((resolve) => setTimeout(resolve, delayMs));
  }
  throw new Error(
    `Server failed to start within ${(maxAttempts * delayMs) / 1000} seconds`,
  );
}
