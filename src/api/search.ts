import { zValidator } from "@hono/zod-validator";
import { Hono } from "hono";
import { z } from "zod";
import { EmacsClient } from "../utils/emacs-client";
import { errorResponse, successResponse } from "../utils/response";

const search = new Hono();
const emacsClient = new EmacsClient();

// GET /search/:query - Search nodes
search.get(
  "/:query",
  zValidator(
    "param",
    z.object({
      query: z.string().min(1, "Search query is required"),
    }),
  ),
  async (c) => {
    try {
      const { query } = c.req.valid("param");

      const result = await emacsClient.get(
        `/search/${encodeURIComponent(query)}`,
      );
      return successResponse(
        c,
        "Search completed successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error searching nodes:", error);
      return errorResponse(
        c,
        "Failed to search nodes",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// GET / - Get all tags (when mounted at /tags)
search.get("/", async (c) => {
  try {
    const result = await emacsClient.get("/tags");
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

// GET /tags/:tag/nodes - Get nodes by tag
search.get(
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
      const result = await emacsClient.get(
        `/tags/${encodeURIComponent(tag)}/nodes`,
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
  },
);

// GET /aliases/:alias/nodes - Get nodes by alias
search.get(
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
      const result = await emacsClient.get(
        `/aliases/${encodeURIComponent(alias)}/nodes`,
      );
      return successResponse(
        c,
        "Aliased nodes retrieved successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error retrieving aliased nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve aliased nodes",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// GET /refs/:ref/nodes - Get nodes by reference
search.get(
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
      const result = await emacsClient.get(
        `/refs/${encodeURIComponent(ref)}/nodes`,
      );
      return successResponse(
        c,
        "Referenced nodes retrieved successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error retrieving referenced nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve referenced nodes",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

// GET /citations/:citation/nodes - Get nodes by citation
search.get(
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
      const result = await emacsClient.get(
        `/citations/${encodeURIComponent(citation)}/nodes`,
      );
      return successResponse(
        c,
        "Citation nodes retrieved successfully",
        result.data || result,
      );
    } catch (error) {
      console.error("Error retrieving citation nodes:", error);
      return errorResponse(
        c,
        "Failed to retrieve citation nodes",
        error instanceof Error ? error.message : "Emacs server unavailable",
        503,
      );
    }
  },
);

export default search;
