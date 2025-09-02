import { serve } from "@hono/node-server";
import { Hono } from "hono";
import { cors } from "hono/cors";
import { logger } from "hono/logger";
import { prettyJSON } from "hono/pretty-json";
import filesRoutes from "./api/files";
import nodesRoutes from "./api/nodes";
import searchRoutes from "./api/search";
import statsRoutes from "./api/stats";
import tagsRoutes from "./api/tags";
import uiRoutes from "./api/ui";

const app = new Hono();

// Middleware
app.use(logger());
app.use(prettyJSON());
app.use(
  "*",
  cors({
    origin: ["http://localhost:3000", "http://localhost:35901"],
    allowHeaders: ["Content-Type", "Authorization"],
    allowMethods: ["GET", "POST", "PUT", "DELETE"],
  }),
);

// Health check
app.get("/health", (c) => {
  return c.json({
    status: "ok",
    timestamp: new Date().toISOString(),
    service: "md-roam-server-api",
  });
});

// API Routes - Complete proxy for all Emacs server functionality
app.route("/api/nodes", nodesRoutes);
app.route("/api/search", searchRoutes);
app.route("/api/files", filesRoutes);
app.route("/api/stats", statsRoutes);
app.route("/api/ui", uiRoutes);
app.route("/api/tags", tagsRoutes);

// Legacy compatibility routes (direct proxy without /api prefix)
app.route("/nodes", nodesRoutes);
app.route("/search", searchRoutes);
app.route("/files", filesRoutes);
app.route("/tags", tagsRoutes);
app.route("/ui", uiRoutes);

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
      ui: "/api/ui",
      tags: "/api/tags",
    },
    legacy_endpoints: {
      nodes: "/nodes",
      search: "/search",
      files: "/files",
      stats: "/stats",
      ui: "/ui",
      tags: "/tags",
      config: "/config",
      sync: "/sync",
    },
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
