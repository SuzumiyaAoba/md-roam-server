import { Hono } from "hono";
import { NodeFileService } from "@/shared/services/node-file-service";
import { errorResponse, successResponse } from "@/shared/lib/response";

const tagRouter = new Hono();
const nodeFileService = new NodeFileService(
  process.env.ORG_ROAM_DIRECTORY || "./tmp/org-roam"
);

// GET / - Get all tags
tagRouter.get("/", async (c) => {
  try {
    const result = nodeFileService.getAllTags();
    return successResponse(
      c,
      "Tags retrieved successfully",
      result,
    );
  } catch (error) {
    console.error("Error retrieving tags:", error);
    return errorResponse(
      c,
      "Failed to retrieve tags",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// GET /:tag/nodes - Get nodes by tag
tagRouter.get("/:tag/nodes", async (c) => {
  try {
    const tag = c.req.param("tag");
    const result = nodeFileService.getNodesByTag(tag);
    return successResponse(
      c,
      "Tagged nodes retrieved successfully",
      result,
    );
  } catch (error) {
    console.error("Error retrieving tagged nodes:", error);
    return errorResponse(
      c,
      "Failed to retrieve tagged nodes",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

export { tagRouter };
