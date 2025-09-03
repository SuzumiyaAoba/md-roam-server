import { Hono } from "hono";
import { EmacsClient } from "@/utils/emacs-client";
import { errorResponse, successResponse } from "@/utils/response";

const stats = new Hono();
const emacsClient = new EmacsClient();

// GET /stats - Get server statistics
stats.get("/", async (c) => {
  try {
    const result = await emacsClient.get("/stats");
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
stats.get("/config", async (c) => {
  try {
    const result = await emacsClient.get("/config");
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
stats.post("/sync", async (c) => {
  try {
    const result = await emacsClient.post("/sync", {});
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

export default stats;
