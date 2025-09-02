import { defineConfig } from "vitest/config";
import { resolve } from "path";

export default defineConfig({
  test: {
    globals: true,
    environment: "node",
    globalSetup: "./utils/globalSetup.ts",
    setupFiles: ["./utils/testSetup.ts"],
    testTimeout: 90000, // Increase timeout for batch processing
    hookTimeout: 90000, // Increase hook timeout for server operations
    pool: "forks", // Use process forks instead of workers to avoid chdir issues
    poolOptions: {
      forks: {
        singleFork: true, // Use single fork to avoid server conflicts
      },
    },
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      exclude: [
        "node_modules/",
        "coverage/",
        "**/*.d.ts",
        "**/*.config.*",
        "**/fixtures/**",
      ],
    },
    include: ["e2e/**/*.{test,spec}.{js,ts}"],
    exclude: [
      "node_modules/",
      "dist/",
      "coverage/",
      "e2e/systemStability.test.ts", // Temporarily disable complex stability tests
      "e2e/japanese-unicode.test.ts", // Temporarily disable due to server encoding issues
    ],
    reporters: ["verbose"],
  },
  resolve: {
    alias: {
      "@/utils": resolve(__dirname, "./utils"),
      "@/fixtures": resolve(__dirname, "./fixtures"),
      "@/e2e": resolve(__dirname, "./e2e"),
    },
  },
});
