import { Hono } from "hono";
import { EmacsClient } from "@/utils/emacs-client";
import { errorResponse, successResponse } from "@/utils/response";

const files = new Hono();
const emacsClient = new EmacsClient();

// GET /files - Get all files
files.get("/", async (c) => {
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

export default files;
