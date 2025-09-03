import { serve } from "@hono/node-server";
import { Hono } from "hono";

// import { swaggerUI } from "@hono/swagger-ui";

import { fileRouter } from "@/features/file-management/api";
// Import routers from features
import { nodeRouter } from "@/features/node-management/api";
import { searchRouter } from "@/features/search/api";
import { tagRouter } from "@/features/tag-management/api";
import { statsRouter } from "@/shared/api/stats";

const app = new Hono();

// Health check endpoint
app.get("/health", (c) => {
  return c.json({
    status: "ok",
    message: "MD-Roam Server API is running",
    timestamp: new Date().toISOString(),
  });
});

// Mount feature routes
app.route("/api/nodes", nodeRouter);
app.route("/api/search", searchRouter);
app.route("/api/files", fileRouter);
app.route("/api/tags", tagRouter);
app.route("/api", statsRouter);

// Legacy routes (without /api prefix for backward compatibility)
app.route("/nodes", nodeRouter);
app.route("/search", searchRouter);
app.route("/files", fileRouter);
app.route("/tags", tagRouter);
app.route("/stats", statsRouter.route("/stats", statsRouter));
app.route("/config", statsRouter.route("/config", statsRouter));
app.route("/sync", statsRouter.route("/sync", statsRouter));

// API Documentation
// app.get("/docs", swaggerUI({
//   url: "/docs/openapi.json",
//   documentation: "/docs",
// }));

const port = process.env["API_PORT"]
  ? Number.parseInt(process.env["API_PORT"], 10)
  : 8080;

console.log(`🚀 MD-Roam Server API starting on port ${port}`);

export { app };

// Start server if this file is run directly
if (import.meta.main) {
  serve({
    fetch: app.fetch,
    port,
  });
}
