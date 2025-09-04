import { zValidator } from "@hono/zod-validator";
import { Hono } from "hono";
import { z } from "zod";
import { NodeFileService } from "@/shared/services/node-file-service";
import { errorResponse, successResponse } from "@/shared/lib/response";

const searchRouter = new Hono();
const nodeFileService = new NodeFileService(
  process.env.ORG_ROAM_DIRECTORY || "./tmp/org-roam"
);

// GET /tags/:tag/nodes - Get nodes by tag (define specific routes first)
searchRouter.get(
  "/tags/:tag/nodes",
  zValidator(
    "param",
    z.object({
      tag: z.string().min(1, "Tag is required"),
    }),
  ),
  async (c) => {
    try {
      const { tag } = c.req.valid("param");
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
  },
);

// GET /aliases/:alias/nodes - Get nodes by alias
searchRouter.get(
  "/aliases/:alias/nodes",
  zValidator(
    "param",
    z.object({
      alias: z.string().min(1, "Alias is required"),
    }),
  ),
  async (c) => {
    try {
      const { alias } = c.req.valid("param");
      // For now, return empty array since we don't have alias search implemented
      // TODO: Implement alias search in NodeFileService
      const result: any[] = [];
      return successResponse(
        c,
        "Aliased nodes retrieved successfully",
        result,
      );
    } catch (error) {
      console.error("Error retrieving aliased nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve aliased nodes",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

// GET /refs/:ref/nodes - Get nodes by reference
searchRouter.get(
  "/refs/:ref/nodes",
  zValidator(
    "param",
    z.object({
      ref: z.string().min(1, "Reference is required"),
    }),
  ),
  async (c) => {
    try {
      const { ref } = c.req.valid("param");
      // For now, return empty array since we don't have ref search implemented
      // TODO: Implement ref search in NodeFileService
      const result: any[] = [];
      return successResponse(
        c,
        "Referenced nodes retrieved successfully",
        result,
      );
    } catch (error) {
      console.error("Error retrieving referenced nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve referenced nodes",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

// GET /citations/:citation/nodes - Get nodes by citation
searchRouter.get(
  "/citations/:citation/nodes",
  zValidator(
    "param",
    z.object({
      citation: z.string().min(1, "Citation is required"),
    }),
  ),
  async (c) => {
    try {
      const { citation } = c.req.valid("param");
      // For now, return empty array since we don't have citation search implemented
      // TODO: Implement citation search in NodeFileService
      const result: any[] = [];
      return successResponse(
        c,
        "Citation nodes retrieved successfully",
        result,
      );
    } catch (error) {
      console.error("Error retrieving citation nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve citation nodes",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

// GET /:query - Search nodes (define parameter routes after specific routes)
searchRouter.get("/:query", async (c) => {
  try {
    const query = c.req.param("query");

    // Handle empty query - return 400 as expected by tests
    if (!query || query.trim() === '') {
      return c.json({
        status: "error",
        message: "Search query cannot be empty",
        error: "Search query parameter is required",
        timestamp: new Date().toISOString(),
      }, 400);
    }

    const results = nodeFileService.searchNodes(query);
    
    // Return flattened structure expected by tests
    return c.json({
      status: "success",
      message: "Search completed successfully",
      query,
      results,
      total_count: results.length,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error searching nodes:", error);
    return c.json({
      status: "error",
      message: "Failed to search nodes",
      error: error instanceof Error ? error.message : "File system error",
      timestamp: new Date().toISOString(),
    }, 500);
  }
});

// GET / - Handle empty search query or get all tags
searchRouter.get("/", async (c) => {
  // Check if this is a search request with empty query (from /search/)
  const path = c.req.path;
  if (path === "/search/" || path === "/api/search/") {
    // Handle empty search query - return 400 as expected by tests
    return c.json({
      status: "error",
      message: "Search query cannot be empty",
      error: "Search query parameter is required",
      timestamp: new Date().toISOString(),
    }, 400);
  }
  
  // Otherwise, return all tags
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

export { searchRouter };