import { zValidator } from "@hono/zod-validator";
import { Hono } from "hono";
import { z } from "zod";
import {
  CreateNodeRequestSchema,
  NodeIdParamSchema,
  UpdateNodeRequestSchema,
} from "../schemas";
import { EmacsClient } from "../utils/emacs-client";
import {
  errorResponse,
  notFoundResponse,
  successResponse,
} from "../utils/response";

const nodes = new Hono();
const emacsClient = new EmacsClient();

// POST /nodes - Create new node
nodes.post("/", zValidator("json", CreateNodeRequestSchema), async (c) => {
  try {
    const nodeData = c.req.valid("json");

    // Delegate node creation to Emacs server
    const result = await emacsClient.createNode(nodeData);

    if (result.status === "error") {
      return errorResponse(
        c,
        result.message || "Failed to create node",
        result.error,
        400,
      );
    }

    return successResponse(
      c,
      result.message || "Node created successfully",
      result.data || result,
      201,
    );
  } catch (error) {
    console.error("Error creating node:", error);
    return errorResponse(
      c,
      "Failed to create node",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// PUT /nodes/:id - Update node
nodes.put(
  "/:id",
  zValidator("param", NodeIdParamSchema),
  zValidator("json", UpdateNodeRequestSchema),
  async (c) => {
    try {
      const { id: nodeId } = c.req.valid("param");
      const updateData = c.req.valid("json");

      // Delegate node update to Emacs server
      const result = await emacsClient.updateNode(nodeId, updateData);

      if (result.status === "error") {
        if (result.error_type === "not_found") {
          return notFoundResponse(c, "Node");
        }
        return errorResponse(
          c,
          result.message || "Failed to update node",
          result.error,
          400,
        );
      }

      return successResponse(
        c,
        result.message || "Node updated successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error updating node:", error);
      return errorResponse(
        c,
        "Failed to update node",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// DELETE /nodes/:id - Delete node
nodes.delete("/:id", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");

    // Delegate node deletion to Emacs server
    const result = await emacsClient.deleteNode(nodeId);

    if (result.status === "error") {
      if (result.error_type === "not_found") {
        return notFoundResponse(c, "Node");
      }
      return errorResponse(
        c,
        result.message || "Failed to delete node",
        result.error,
        400,
      );
    }

    return successResponse(
      c,
      result.message || "Node deleted successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error deleting node:", error);
    return errorResponse(
      c,
      "Failed to delete node",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// POST /nodes/:id/tags - Add tag to node
nodes.post(
  "/:id/tags",
  zValidator("param", NodeIdParamSchema),
  zValidator(
    "json",
    z.object({
      tag: z.string().min(1, "Tag is required and cannot be empty"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId } = c.req.valid("param");
      const { tag } = c.req.valid("json");

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
nodes.delete(
  "/:id/tags/:tag",
  zValidator(
    "param",
    NodeIdParamSchema.extend({
      tag: z.string().min(1, "Tag is required"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId, tag } = c.req.valid("param");

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
nodes.post(
  "/:id/categories",
  zValidator("param", NodeIdParamSchema),
  zValidator(
    "json",
    z.object({
      category: z.string().min(1, "Category is required and cannot be empty"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId } = c.req.valid("param");
      const { category } = c.req.valid("json");

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
nodes.delete(
  "/:id/categories/:category",
  zValidator(
    "param",
    NodeIdParamSchema.extend({
      category: z.string().min(1, "Category is required"),
    }),
  ),
  async (c) => {
    try {
      const { id: nodeId, category } = c.req.valid("param");

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

// GET requests are delegated to Emacs server
nodes.get("/", async (c) => {
  try {
    const result = await emacsClient.getNodes();
    return successResponse(
      c,
      "Nodes retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving nodes:", error);
    return errorResponse(
      c,
      "Failed to retrieve nodes",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

nodes.get("/:id", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.getNode(nodeId);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving node:", error);
    return errorResponse(
      c,
      "Failed to retrieve node",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

nodes.get("/:id/content", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.getNodeContent(nodeId);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node content retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving node content:", error);
    return errorResponse(
      c,
      "Failed to retrieve node content",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

// Additional GET endpoints for complete node operations
nodes.get(
  "/:id/backlinks",
  zValidator("param", NodeIdParamSchema),
  async (c) => {
    try {
      const { id: nodeId } = c.req.valid("param");
      const result = await emacsClient.get(`/nodes/${nodeId}/backlinks`);

      if (result.status === "error") {
        return notFoundResponse(c, "Node");
      }

      return successResponse(
        c,
        "Node backlinks retrieved successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error retrieving node backlinks:", error);
      return errorResponse(
        c,
        "Failed to retrieve node backlinks",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

nodes.get("/:id/links", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.get(`/nodes/${nodeId}/links`);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node links retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving node links:", error);
    return errorResponse(
      c,
      "Failed to retrieve node links",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

nodes.get("/:id/aliases", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.get(`/nodes/${nodeId}/aliases`);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node aliases retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving node aliases:", error);
    return errorResponse(
      c,
      "Failed to retrieve node aliases",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

nodes.get("/:id/refs", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.get(`/nodes/${nodeId}/refs`);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node refs retrieved successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error retrieving node refs:", error);
    return errorResponse(
      c,
      "Failed to retrieve node refs",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

nodes.get("/:id/parse", zValidator("param", NodeIdParamSchema), async (c) => {
  try {
    const { id: nodeId } = c.req.valid("param");
    const result = await emacsClient.get(`/nodes/${nodeId}/parse`);

    if (result.status === "error") {
      return notFoundResponse(c, "Node");
    }

    return successResponse(
      c,
      "Node parsed successfully",
      result.data || result,
    );
  } catch (error) {
    console.error("Error parsing node:", error);
    return errorResponse(
      c,
      "Failed to parse node",
      error instanceof Error ? error.message : "Emacs server unavailable",
      503,
    );
  }
});

export default nodes;
