import { execSync } from 'child_process';
import { join } from 'path';
import { existsSync, mkdirSync } from 'fs';

// Global setup for test suite
export async function setup() {
  console.log('🚀 Global setup: Starting md-roam server...');
  
  try {
    const projectRoot = join(process.cwd(), '..');
    const testConfigPath = join(process.cwd(), 'config', 'test-config.yml');
    const tmpDir = join(projectRoot, 'tmp');
    const testOrgRoamDir = join(tmpDir, 'org-roam');
    
    // Create tmp directory and test org-roam directory
    if (!existsSync(tmpDir)) {
      mkdirSync(tmpDir, { recursive: true });
      console.log('📁 Created tmp directory:', tmpDir);
    }
    
    if (!existsSync(testOrgRoamDir)) {
      mkdirSync(testOrgRoamDir, { recursive: true });
      console.log('📁 Created test org-roam directory:', testOrgRoamDir);
    }
    
    // Check if server is already running
    const isRunning = await checkServerHealth();
    if (isRunning) {
      console.log('✅ Server already running');
      return;
    }

    // Start server with test configuration
    const env = { ...process.env, MD_ROAM_CONFIG_FILE: testConfigPath };
    execSync('./start.sh', { 
      stdio: 'pipe',
      cwd: projectRoot,
      env
    });
    
    // Wait for server to be ready
    await waitForServer();
    console.log('✅ Server started successfully with test configuration');
    console.log('📁 Test org-roam directory:', testOrgRoamDir);
    
  } catch (error) {
    console.error('❌ Failed to start server in global setup:', error);
    throw error;
  }
}

export async function teardown() {
  console.log('🛑 Global teardown: Stopping md-roam server...');
  
  try {
    const projectRoot = join(process.cwd(), '..');
    
    execSync('./stop.sh', { 
      stdio: 'pipe',
      cwd: projectRoot 
    });
    
    console.log('✅ Server stopped successfully');
  } catch (error) {
    console.error('❌ Failed to stop server in global teardown:', error);
    // Don't throw in teardown to avoid masking test failures
  }
}

async function checkServerHealth(): Promise<boolean> {
  try {
    const response = await fetch('http://localhost:8080/stats');
    return response.ok;
  } catch {
    return false;
  }
}

async function waitForServer(maxAttempts = 30, delayMs = 1000): Promise<void> {
  for (let i = 0; i < maxAttempts; i++) {
    if (await checkServerHealth()) {
      return;
    }
    await new Promise(resolve => setTimeout(resolve, delayMs));
  }
  throw new Error('Server failed to start within timeout');
}