import { Hono } from "hono";
import { EmacsClient } from "../utils/emacs-client";
import { errorResponse, successResponse } from "../utils/response";

const ui = new Hono();
const emacsClient = new EmacsClient();

// GET /ui - Get UI status
ui.get("/", async (c) => {
  try {
    const result = await emacsClient.get("/ui");
    return successResponse(
      c,
      "UI status retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving UI status:", error);
    return errorResponse(
      c,
      "Failed to retrieve UI status",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

export default ui;
