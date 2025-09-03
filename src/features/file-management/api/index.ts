import { Hono } from "hono";
import { EmacsClient } from "@/shared/api/emacs-client";
import { errorResponse, successResponse } from "@/shared/lib/response";

const fileRouter = new Hono();
const emacsClient = new EmacsClient();

// GET /files - Get all files
fileRouter.get("/", async (c) => {
  try {
    const result = await emacsClient.getFiles();
    return successResponse(
      c,
      "Files retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving files:", error);
    return errorResponse(
      c,
      "Failed to retrieve files",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

export { fileRouter };
