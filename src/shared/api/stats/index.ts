import { Hono } from "hono";
import { EmacsClient } from "@/shared/api/emacs-client";
import { errorResponse, successResponse } from "@/shared/lib/response";

const statsRouter = new Hono();
const emacsClient = new EmacsClient();

// GET /stats - Get server statistics
statsRouter.get("/stats", async (c) => {
  try {
    const result = await emacsClient.getStats();
    return successResponse(
      c,
      "Statistics retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving statistics:", error);
    return errorResponse(
      c,
      "Failed to retrieve statistics",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// GET /config - Get server configuration
statsRouter.get("/config", async (c) => {
  try {
    const result = await emacsClient.getConfig();
    return successResponse(
      c,
      "Configuration retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving configuration:", error);
    return errorResponse(
      c,
      "Failed to retrieve configuration",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// POST /sync - Sync database
statsRouter.post("/sync", async (c) => {
  try {
    const result = await emacsClient.syncDatabase();
    return successResponse(c, "Database sync completed", result.data || result);
  } catch (error) {
    console.error("Error syncing database:", error);
    return errorResponse(
      c,
      "Failed to sync database",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

export { statsRouter };
