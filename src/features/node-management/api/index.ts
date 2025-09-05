import { Hono } from "hono";
import { z } from "zod";
import { EmacsClient } from "@/shared/api/emacs-client";
import { NodeFileService, ValidationError, NotFoundError } from "@/shared/services/node-file-service";
import {
  errorResponse,
  notFoundResponse,
  successResponse,
} from "@/shared/lib/response";
import {
  CreateNodeRequestSchema,
  NodeIdParamSchema,
  UpdateNodeRequestSchema,
} from "@/shared/lib/schemas";
import { customValidator } from "@/shared/lib/validation";

const nodeRouter = new Hono();
const emacsClient = new EmacsClient();

// Initialize Node File Service with org-roam directory
// Use the same path that the test configuration uses
const orgRoamDir = process.env.ORG_ROAM_DIR || "/Users/suzumiyaaoba/ghq/github.com/SuzumiyaAoba/md-roam-server/tmp/org-roam";
const nodeFileService = new NodeFileService(orgRoamDir);

// POST /nodes - Create new node
nodeRouter.post("/", customValidator("json", CreateNodeRequestSchema), async (c) => {
  try {
    const nodeData = (c as any).validatedData?.json;

    // Create node using file service
    const createdNode = nodeFileService.createNode(nodeData);

    return c.json({
      status: "success",
      message: "Node created successfully",
      ...createdNode,
      timestamp: new Date().toISOString(),
    }, 201);
  } catch (error) {
    console.error("Error creating node:", error);
    
    if (error instanceof ValidationError) {
      return c.json({
        status: "error",
        message: error.message,
        error: error.errors?.join(", ") || error.message,
        timestamp: new Date().toISOString(),
      }, 400);
    }
    
    return errorResponse(
      c,
      "Failed to create node",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// PUT /nodes/:id - Update node
nodeRouter.put(
  "/:id",
  customValidator("param", NodeIdParamSchema),
  customValidator("json", UpdateNodeRequestSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const updateData = (c as any).validatedData?.json;

      // Update node using file service
      const updatedNode = nodeFileService.updateNode(nodeId, updateData);

      return c.json({
        status: "success",
        message: "Node updated successfully",
        ...updatedNode,
        note: "File updated successfully. For database operations, use POST /sync to update database.",
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error updating node:", error);
      
      if (error instanceof ValidationError) {
        return c.json({
          status: "error",
          message: error.message,
          error: error.errors?.join(", ") || error.message,
          timestamp: new Date().toISOString(),
        }, 400);
      }
      
      if (error instanceof NotFoundError) {
        return c.json({
          status: "error",
          message: error.message,
          timestamp: new Date().toISOString(),
        }, 404);
      }
      
      return errorResponse(
        c,
        "Failed to update node",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

// DELETE /nodes/:id - Delete node
nodeRouter.delete("/:id", customValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = (c as any).validatedData?.param;

    // Delete node using file service
    nodeFileService.deleteNode(nodeId);

    return successResponse(
      c,
      "Node deleted successfully",
      { id: nodeId },
    );
  } catch (error) {
    console.error("Error deleting node:", error);
    
    if (error instanceof ValidationError) {
      return c.json({
        status: "error",
        message: error.message,
        error: error.errors?.join(", ") || error.message,
        timestamp: new Date().toISOString(),
      }, 400);
    }
    
    if (error instanceof NotFoundError) {
      return c.json({
        status: "error",
        message: error.message,
        timestamp: new Date().toISOString(),
      }, 404);
    }
    
    return errorResponse(
      c,
      "Failed to delete node",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// POST /nodes/:id/tags - Add tag to node
nodeRouter.post(
  "/:id/tags",
  customValidator("param", NodeIdParamSchema),
  customValidator(
    "json",
    z.object({
      tag: z.string().min(1, "Tag is required and cannot be empty"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const { tag } = (c as any).validatedData?.json;

      // Delegate tag addition to Emacs server
      const result = await emacsClient.addTagToNode(nodeId, tag);

      if (result.status === "error") {
        return errorResponse(
          c,
          result.message || "Failed to add tag",
          result.error,
          400,
        );
      }

      return successResponse(
        c,
        result.message || "Tag added successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error adding tag:", error);
      return errorResponse(
        c,
        "Failed to add tag",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// DELETE /nodes/:id/tags/:tag - Remove tag from node
nodeRouter.delete(
  "/:id/tags/:tag",
  customValidator(
    "param",
    NodeIdParamSchema.extend({
      tag: z.string().min(1, "Tag is required"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId, tag } = (c as any).validatedData?.param;

      // Delegate tag removal to Emacs server
      const result = await emacsClient.removeTagFromNode(
        nodeId,
        decodeURIComponent(tag),
      );

      if (result.status === "error") {
        return errorResponse(
          c,
          result.message || "Failed to remove tag",
          result.error,
          400,
        );
      }

      return successResponse(
        c,
        result.message || "Tag removed successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error removing tag:", error);
      return errorResponse(
        c,
        "Failed to remove tag",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// POST /nodes/:id/categories - Add category to node
nodeRouter.post(
  "/:id/categories",
  customValidator("param", NodeIdParamSchema),
  customValidator(
    "json",
    z.object({
      category: z.string().min(1, "Category is required and cannot be empty"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const { category } = (c as any).validatedData?.json;

      // Delegate category addition to Emacs server
      const result = await emacsClient.addCategoryToNode(nodeId, category);

      if (result.status === "error") {
        return errorResponse(
          c,
          result.message || "Failed to add category",
          result.error,
          400,
        );
      }

      return successResponse(
        c,
        result.message || "Category added successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error adding category:", error);
      return errorResponse(
        c,
        "Failed to add category",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// DELETE /nodes/:id/categories/:category - Remove category from node
nodeRouter.delete(
  "/:id/categories/:category",
  customValidator(
    "param",
    NodeIdParamSchema.extend({
      category: z.string().min(1, "Category is required"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId, category } = (c as any).validatedData?.param;

      // Delegate category removal to Emacs server
      const result = await emacsClient.removeCategoryFromNode(
        nodeId,
        decodeURIComponent(category),
      );

      if (result.status === "error") {
        return errorResponse(
          c,
          result.message || "Failed to remove category",
          result.error,
          400,
        );
      }

      return successResponse(
        c,
        result.message || "Category removed successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error removing category:", error);
      return errorResponse(
        c,
        "Failed to remove category",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// GET requests are handled by file service for now
nodeRouter.get("/", async (c) => {
  try {
    const nodes = nodeFileService.getAllNodes();
    return successResponse(
      c,
      "Nodes retrieved successfully",
      nodes,
    );
  } catch (error) {
    console.error("Error retrieving nodes:", error);
    return errorResponse(
      c,
      "Failed to retrieve nodes",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

nodeRouter.get("/:id", customValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = (c as any).validatedData?.param;
    const node = nodeFileService.getNode(nodeId);

    if (!node) {
      return notFoundResponse(c, "Node");
    }

    return c.json({
      status: "success",
      message: "Node retrieved successfully",
      ...node,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error retrieving node:", error);
    return errorResponse(
      c,
      "Failed to retrieve node",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

nodeRouter.get(
  "/:id/content",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const content = nodeFileService.getNodeContent(nodeId);

      if (!content) {
        return notFoundResponse(c, "Node");
      }

      return c.json({
        status: "success",
        message: "Node content retrieved successfully",
        content,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error retrieving node content:", error);
      return errorResponse(
        c,
        "Failed to retrieve node content",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

// Additional GET endpoints for complete node operations
nodeRouter.get(
  "/:id/backlinks",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const backlinks = nodeFileService.getNodeBacklinks(nodeId);

      return successResponse(
        c,
        "Node backlinks retrieved successfully",
        backlinks,
      );
    } catch (error) {
      console.error("Error retrieving node backlinks:", error);
      return errorResponse(
        c,
        "Failed to retrieve node backlinks",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

nodeRouter.get(
  "/:id/links",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const links = nodeFileService.getNodeLinks(nodeId);

      return successResponse(
        c,
        "Node links retrieved successfully",
        links,
      );
    } catch (error) {
      console.error("Error retrieving node links:", error);
      return errorResponse(
        c,
        "Failed to retrieve node links",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

nodeRouter.get(
  "/:id/aliases",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const aliases = nodeFileService.getNodeAliases(nodeId);

      return successResponse(
        c,
        "Node aliases retrieved successfully",
        aliases,
      );
    } catch (error) {
      console.error("Error retrieving node aliases:", error);
      return errorResponse(
        c,
        "Failed to retrieve node aliases",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

nodeRouter.get(
  "/:id/refs",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const refs = nodeFileService.getNodeRefs(nodeId);

      return successResponse(
        c,
        "Node refs retrieved successfully",
        refs,
      );
    } catch (error) {
      console.error("Error retrieving node refs:", error);
      return errorResponse(
        c,
        "Failed to retrieve node refs",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

nodeRouter.get(
  "/:id/parse",
  customValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = (c as any).validatedData?.param;
      const parsed = nodeFileService.parseNode(nodeId);

      if (!parsed) {
        return notFoundResponse(c, "Node");
      }

      return successResponse(
        c,
        "Node parsed successfully",
        parsed,
      );
    } catch (error) {
      console.error("Error parsing node:", error);
      return errorResponse(
        c,
        "Failed to parse node",
        error instanceof Error ? error.message : "File system error",
        500,
      );
    }
  },
);

export { nodeRouter };
