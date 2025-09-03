import { Hono } from "hono";
import { EmacsClient } from "@/shared/api/emacs-client";
import { errorResponse, successResponse } from "@/shared/lib/response";
import { EmacsTaggedNodesResponseSchema } from "@/shared/lib/schemas/emacs-response";

const tagRouter = new Hono();
const emacsClient = new EmacsClient();

// GET / - Get all tags
tagRouter.get("/", async (c) => {
  try {
    const result = await emacsClient.getTags();
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
tagRouter.get("/:tag/nodes", async (c) => {
  try {
    const tag = c.req.param("tag");
    const result = await emacsClient.get(
      `/tags/${encodeURIComponent(tag)}/nodes`,
      EmacsTaggedNodesResponseSchema,
    );
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

export { tagRouter };
