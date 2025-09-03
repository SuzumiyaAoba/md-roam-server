import { serve } from "@hono/node-server";
import { Hono } from "hono";
import { cors } from "hono/cors";
import { logger } from "hono/logger";
import { prettyJSON } from "hono/pretty-json";
import type { ZodError } from "zod";
import filesRoutes from "@/api/files";
import nodesRoutes from "@/api/nodes";
import searchRoutes from "@/api/search";
import statsRoutes from "@/api/stats";
import tagsRoutes from "@/api/tags";

const app = new Hono();

// Error handling middleware for Zod validation errors
app.onError((err, c) => {
  if (err.name === "ZodError") {
    const zodError = err as ZodError;
    return c.json(
      {
        status: "error",
        message: "Validation failed",
        timestamp: new Date().toISOString(),
        error_code: "VALIDATION_ERROR",
        details: {
          issues: zodError.issues || [],
        },
      },
      400,
    );
  }

  // Generic error response
  return c.json(
    {
      status: "error",
      message: err.message || "Internal server error",
      timestamp: new Date().toISOString(),
      error_code: "INTERNAL_ERROR",
    },
    500,
  );
});

// Middleware
app.use(logger());
app.use(prettyJSON());
app.use(
  "*",
  cors({
    origin: ["http://localhost:3000"],
    allowHeaders: ["Content-Type", "Authorization"],
    allowMethods: ["GET", "POST", "PUT", "DELETE"],
  }),
);

// Health check
app.get("/health", (c) => {
  return c.json({
    status: "success",
    message: "MD-Roam Server API is healthy",
    timestamp: new Date().toISOString(),
    service: "md-roam-server-api",
  });
});

// API Routes - Complete proxy for all Emacs server functionality
app.route("/api/nodes", nodesRoutes);
app.route("/api/search", searchRoutes);
app.route("/api/files", filesRoutes);
app.route("/api/stats", statsRoutes);
app.route("/api/tags", tagsRoutes);

// Legacy compatibility routes (direct proxy without /api prefix)
app.route("/nodes", nodesRoutes);
app.route("/search", searchRoutes);
app.route("/files", filesRoutes);
app.route("/tags", tagsRoutes);

// Stats routes with multiple paths
app.route("/stats", statsRoutes);
app.route("/config", statsRoutes);
app.route("/sync", statsRoutes);

// Default route
app.get("/", (c) => {
  return c.json({
    message: "MD-Roam Server API - Unified Endpoint",
    version: "1.0.0",
    architecture: "Single API server with Emacs proxy",
    endpoints: {
      health: "/health",
      nodes: "/api/nodes",
      search: "/api/search",
      files: "/api/files",
      stats: "/api/stats",
      tags: "/api/tags",
    },
    documentation: "/docs",
  });
});

const port = process.env.API_PORT
  ? Number.parseInt(process.env.API_PORT, 10)
  : 3001;

console.log(`🚀 MD-Roam Server API starting on port ${port}`);

serve({
  fetch: app.fetch,
  port,
});
