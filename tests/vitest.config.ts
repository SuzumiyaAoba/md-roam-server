import { defineConfig } from 'vitest/config';
import { resolve } from 'path';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    setupFiles: ['./utils/testSetup.ts'],
    testTimeout: 30000,
    hookTimeout: 30000,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'coverage/',
        '**/*.d.ts',
        '**/*.config.*',
        '**/fixtures/**',
      ],
    },
    include: ['e2e/**/*.{test,spec}.{js,ts}'],
    exclude: ['node_modules/', 'dist/', 'coverage/'],
    reporters: ['verbose'],
  },
  resolve: {
    alias: {
      '@/utils': resolve(__dirname, './utils'),
      '@/fixtures': resolve(__dirname, './fixtures'),
      '@/e2e': resolve(__dirname, './e2e'),
    },
  },
});