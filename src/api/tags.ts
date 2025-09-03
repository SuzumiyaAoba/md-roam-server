import { Hono } from "hono";
import type { ApiResponse } from "@/types";
import { EmacsClient } from "@/utils/emacs-client";
import { errorResponse, successResponse } from "@/utils/response";

const tags = new Hono();
const emacsClient = new EmacsClient();

// GET / - Get all tags
tags.get("/", async (c) => {
  try {
    const result = (await emacsClient.get("/tags")) as ApiResponse;
    return successResponse(
      c,
      "Tags retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving tags:", error);
    return errorResponse(
      c,
      "Failed to retrieve tags",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// GET /:tag/nodes - Get nodes by tag
tags.get("/:tag/nodes", async (c) => {
  try {
    const tag = c.req.param("tag");
    const result = (await emacsClient.get(
      `/tags/${encodeURIComponent(tag)}/nodes`,
    )) as ApiResponse;
    return successResponse(
      c,
      "Tagged nodes retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving tagged nodes:", error);
    return errorResponse(
      c,
      "Failed to retrieve tagged nodes",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

export default tags;
