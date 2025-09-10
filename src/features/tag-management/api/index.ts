import { Hono } from "hono";
import { errorResponse, successResponse } from "@/shared/lib/response";
import { NodeFileService } from "@/shared/services/node-file-service";

const tagRouter = new Hono();
const nodeFileService = new NodeFileService(
  process.env["ORG_ROAM_DIRECTORY"] || "./tmp/org-roam",
);

// GET / - Get all tags
tagRouter.get("/", async (c) => {
  try {
    const result = nodeFileService.getAllTags();
    return c.json({
      status: "success",
      message: "Tags retrieved successfully",
      tags: result,
      timestamp: new Date().toISOString(),
    });
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
    return successResponse(c, "Tagged nodes retrieved successfully", result);
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
