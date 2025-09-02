import { Hono } from "hono";
import { validator } from "hono/validator";
import type { CreateNodeRequest, UpdateNodeRequest } from "../types";
import { EmacsClient } from "../utils/emacs-client";
import {
  errorResponse,
  notFoundResponse,
  successResponse,
  validationErrorResponse,
} from "../utils/response";

const nodes = new Hono();
const emacsClient = new EmacsClient();

// POST /nodes - Create new node
nodes.post(
  "/",
  validator("json", (value, c) => {
    const { title, content, file_type, category, tags, aliases, refs } =
      value as CreateNodeRequest;

    if (!title || typeof title !== "string" || title.trim() === "") {
      return validationErrorResponse(
        c,
        "Title is required and cannot be empty",
      );
    }

    if (file_type && !["md", "org"].includes(file_type)) {
      return validationErrorResponse(
        c,
        'file_type must be either "md" or "org"',
      );
    }

    return {
      title: title.trim(),
      content: content || "",
      file_type: file_type || "md",
      category,
      tags: Array.isArray(tags) ? tags : [],
      aliases: Array.isArray(aliases) ? aliases : [],
      refs: Array.isArray(refs) ? refs : [],
    };
  }),
  async (c) => {
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
  },
);

// PUT /nodes/:id - Update node
nodes.put(
  "/:id",
  validator("json", (value, c) => {
    const { title, content, category, tags, aliases, refs } =
      value as UpdateNodeRequest;

    if (
      title !== undefined &&
      (typeof title !== "string" || title.trim() === "")
    ) {
      return validationErrorResponse(c, "Title cannot be empty if provided");
    }

    return {
      title: title?.trim(),
      content,
      category,
      tags: Array.isArray(tags) ? tags : undefined,
      aliases: Array.isArray(aliases) ? aliases : undefined,
      refs: Array.isArray(refs) ? refs : undefined,
    };
  }),
  async (c) => {
    try {
      const nodeId = c.req.param("id");
      const updateData = c.req.valid("json");

      if (!nodeId) {
        return validationErrorResponse(c, "Node ID is required");
      }

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
nodes.delete("/:id", async (c) => {
  try {
    const nodeId = c.req.param("id");

    if (!nodeId) {
      return validationErrorResponse(c, "Node ID is required");
    }

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
  validator("json", (value, c) => {
    const { tag } = value as { tag?: string };

    if (!tag || typeof tag !== "string" || tag.trim() === "") {
      return validationErrorResponse(c, "Tag is required and cannot be empty");
    }

    return { tag: tag.trim() };
  }),
  async (c) => {
    try {
      const nodeId = c.req.param("id");
      const { tag } = c.req.valid("json");

      if (!nodeId) {
        return validationErrorResponse(c, "Node ID is required");
      }

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
nodes.delete("/:id/tags/:tag", async (c) => {
  try {
    const nodeId = c.req.param("id");
    const tag = c.req.param("tag");

    if (!nodeId) {
      return validationErrorResponse(c, "Node ID is required");
    }

    if (!tag) {
      return validationErrorResponse(c, "Tag is required");
    }

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
});

// POST /nodes/:id/categories - Add category to node
nodes.post(
  "/:id/categories",
  validator("json", (value, c) => {
    const { category } = value as { category?: string };

    if (!category || typeof category !== "string" || category.trim() === "") {
      return validationErrorResponse(
        c,
        "Category is required and cannot be empty",
      );
    }

    return { category: category.trim() };
  }),
  async (c) => {
    try {
      const nodeId = c.req.param("id");
      const { category } = c.req.valid("json");

      if (!nodeId) {
        return validationErrorResponse(c, "Node ID is required");
      }

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
nodes.delete("/:id/categories/:category", async (c) => {
  try {
    const nodeId = c.req.param("id");
    const category = c.req.param("category");

    if (!nodeId) {
      return validationErrorResponse(c, "Node ID is required");
    }

    if (!category) {
      return validationErrorResponse(c, "Category is required");
    }

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
});

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

nodes.get("/:id", async (c) => {
  try {
    const nodeId = c.req.param("id");
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

nodes.get("/:id/content", async (c) => {
  try {
    const nodeId = c.req.param("id");
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

export default nodes;
