import { serve } from "@hono/node-server";
import { Hono } from "hono";
import { HTTPException } from "hono/http-exception";

// import { swaggerUI } from "@hono/swagger-ui";

import { fileRouter } from "@/features/file-management/api";
// Import routers from features
import { nodeRouter } from "@/features/node-management/api";
import { searchRouter } from "@/features/search/api";
import { tagRouter } from "@/features/tag-management/api";
import { statsRouter } from "@/shared/api/stats";

const app = new Hono();

// Global error handler for JSON parsing and other errors
app.onError((err, c) => {
  console.error("API Error:", err);

  if (err instanceof HTTPException) {
    // Check if this is a zod validation error
    if (err.status === 400 && err.message.includes("Validation")) {
      return c.json(
        {
          status: "error",
          message: "Validation failed",
          error: err.message,
          timestamp: new Date().toISOString(),
        },
        400,
      );
    }

    return c.json(
      {
        status: "error",
        message: err.message,
        timestamp: new Date().toISOString(),
      },
      err.status,
    );
  }

  // Handle JSON parsing errors
  if (
    err.message?.includes("Unexpected token") ||
    err.message?.includes("JSON") ||
    err.name === "SyntaxError"
  ) {
    return c.json(
      {
        status: "error",
        message: "Invalid JSON format",
        error: "Request body must be valid JSON",
        timestamp: new Date().toISOString(),
      },
      400,
    );
  }

  // Handle content-type errors
  if (
    err.message?.includes("Content-Type") ||
    err.message?.includes("content-type")
  ) {
    return c.json(
      {
        status: "error",
        message: "Invalid content type",
        error: "Content-Type must be application/json",
        timestamp: new Date().toISOString(),
      },
      400,
    );
  }

  // Generic server error
  return c.json(
    {
      status: "error",
      message: "Internal server error",
      error: err.message || "Unknown error",
      timestamp: new Date().toISOString(),
    },
    500,
  );
});

// Custom 404 handler to ensure JSON response
app.notFound((c) => {
  return c.json(
    {
      status: "error",
      message: "Not found",
      error: "The requested resource was not found",
      timestamp: new Date().toISOString(),
    },
    404,
  );
});

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
app.route("/", statsRouter); // Mount statsRouter at root to handle /stats, /config, /sync

// API Documentation
// app.get("/docs", swaggerUI({
//   url: "/docs/openapi.json",
//   documentation: "/docs",
// }));

const port = process.env["API_PORT"]
  ? Number.parseInt(process.env["API_PORT"], 10)
  : 3001;

console.log(`🚀 MD-Roam Server API starting on port ${port}`);

export { app };

// Start server if this file is run directly
if (import.meta.main) {
  serve({
    fetch: app.fetch,
    port,
  });
}
