import { Hono } from "hono";
import { NodeFileService } from "@/shared/services/node-file-service";
import { errorResponse, successResponse } from "@/shared/lib/response";

const statsRouter = new Hono();
const nodeFileService = new NodeFileService(
  process.env.ORG_ROAM_DIRECTORY || "./tmp/org-roam"
);

// GET /stats - Get server statistics
statsRouter.get("/stats", async (c) => {
  try {
    const result = nodeFileService.getStats();
    return c.json({
      status: "success",
      message: "Statistics retrieved successfully",
      timestamp: new Date().toISOString(),
      ...result, // Spread the stats directly into the response body
    });
  } catch (error) {
    console.error("Error retrieving statistics:", error);
    return errorResponse(
      c,
      "Failed to retrieve statistics",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// GET /config - Get server configuration
statsRouter.get("/config", async (c) => {
  try {
    const config = {
      org_roam_directory: process.env.ORG_ROAM_DIRECTORY || "./tmp/org-roam",
      server_type: "file_service",
      api_version: "1.0.0",
      supported_formats: ["md", "org"],
    };
    return successResponse(
      c,
      "Configuration retrieved successfully",
      config,
    );
  } catch (error) {
    console.error("Error retrieving configuration:", error);
    return errorResponse(
      c,
      "Failed to retrieve configuration",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// POST /sync - Sync database
statsRouter.post("/sync", async (c) => {
  try {
    // In file-based mode, "sync" means scan all files and return stats
    const allNodes = nodeFileService.getAllNodes();
    const stats = nodeFileService.getStats();
    const result = {
      message: "File system sync completed",
      nodes_found: allNodes.length,
      stats: stats
    };
    return successResponse(c, "Database sync completed", result, 201);
  } catch (error) {
    console.error("Error syncing database:", error);
    return errorResponse(
      c,
      "Failed to sync database",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

export { statsRouter };
