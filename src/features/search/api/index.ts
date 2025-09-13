import { zValidator } from "@hono/zod-validator";
import { Hono } from "hono";
import { z } from "zod";
import { errorResponse, successResponse } from "@/shared/lib/response";
import { 
  FullTextSearchRequestSchema,
  FuzzySearchRequestSchema,
  PhraseSearchRequestSchema,
  FieldSearchRequestSchema,
  SuggestionsRequestSchema,
  HighlightSearchRequestSchema,
} from "@/shared/lib/schemas";
import { NodeFileService } from "@/shared/services/node-file-service";
import { AdvancedSearchService } from "@/shared/services/advanced-search-service";

const searchRouter = new Hono();
const orgRoamDir = process.env["ORG_ROAM_DIRECTORY"] || "./tmp/org-roam";
const nodeFileService = new NodeFileService(orgRoamDir);
const advancedSearchService = new AdvancedSearchService(orgRoamDir);

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
      const { alias: _alias } = c.req.valid("param");
      // For now, return empty array since we don't have alias search implemented
      // TODO: Implement alias search in NodeFileService
      const result: unknown[] = [];
      return successResponse(c, "Aliased nodes retrieved successfully", result);
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
      const { ref: _ref } = c.req.valid("param");
      // For now, return empty array since we don't have ref search implemented
      // TODO: Implement ref search in NodeFileService
      const result: unknown[] = [];
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
      const { citation: _citation } = c.req.valid("param");
      // For now, return empty array since we don't have citation search implemented
      // TODO: Implement citation search in NodeFileService
      const result: unknown[] = [];
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

// POST /fulltext - Advanced full-text search using ripgrep
searchRouter.post(
  "/fulltext",
  zValidator("json", FullTextSearchRequestSchema),
  async (c) => {
    try {
      const {
        query,
        caseSensitive,
        regex,
        contextLines,
        fileTypes,
        maxResults,
      } = c.req.valid("json");

      const searchResults = await nodeFileService.fullTextSearch(query.trim(), {
        caseSensitive,
        regex,
        contextLines,
        fileTypes,
        maxResults,
      });

      return c.json({
        status: "success",
        message: "Full-text search completed successfully",
        ...searchResults,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error in full-text search:", error);
      return errorResponse(
        c,
        "Full-text search failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// POST /fuzzy - Fuzzy search using Levenshtein distance
searchRouter.post(
  "/fuzzy",
  zValidator("json", FuzzySearchRequestSchema),
  async (c) => {
    try {
      const request = c.req.valid("json");
      const { results, totalResults, searchTime } = 
        await advancedSearchService.fuzzySearch(request);

      return c.json({
        status: "success",
        message: "Fuzzy search completed successfully",
        results,
        totalResults,
        query: request.query,
        threshold: request.threshold,
        searchTime,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error in fuzzy search:", error);
      return errorResponse(
        c,
        "Fuzzy search failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// POST /phrase - Phrase search for exact matches
searchRouter.post(
  "/phrase",
  zValidator("json", PhraseSearchRequestSchema),
  async (c) => {
    try {
      const request = c.req.valid("json");
      const { results, totalResults, searchTime } = 
        await advancedSearchService.phraseSearch(request);

      return c.json({
        status: "success",
        message: "Phrase search completed successfully",
        results,
        totalResults,
        phrase: request.phrase,
        searchTime,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error in phrase search:", error);
      return errorResponse(
        c,
        "Phrase search failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// POST /field - Field-specific search
searchRouter.post(
  "/field",
  zValidator("json", FieldSearchRequestSchema),
  async (c) => {
    try {
      const request = c.req.valid("json");
      const { results, totalResults, searchTime } = 
        await advancedSearchService.fieldSearch(request);

      return c.json({
        status: "success",
        message: "Field search completed successfully",
        results,
        totalResults,
        query: request.query,
        field: request.field,
        searchTime,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error in field search:", error);
      return errorResponse(
        c,
        "Field search failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// POST /suggestions - Generate search suggestions
searchRouter.post(
  "/suggestions",
  zValidator("json", SuggestionsRequestSchema),
  async (c) => {
    try {
      const request = c.req.valid("json");
      const { suggestions, searchTime } = 
        await advancedSearchService.generateSuggestions(request);

      return c.json({
        status: "success",
        message: "Search suggestions generated successfully",
        suggestions,
        query: request.query,
        field: request.field,
        searchTime,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error generating suggestions:", error);
      return errorResponse(
        c,
        "Suggestion generation failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// GET /suggestions/:query - Simple suggestion endpoint
searchRouter.get("/suggestions/:query", async (c) => {
  try {
    const query = c.req.param("query");

    if (!query || query.trim() === "") {
      return errorResponse(
        c,
        "Query is required",
        "Query parameter cannot be empty",
        400,
      );
    }

    // Parse query parameters
    const field = (c.req.query("field") as "title" | "content" | "tags" | "category") || "title";
    const maxSuggestions = Math.max(
      1,
      Math.min(parseInt(c.req.query("limit") || "10", 10), 20),
    );

    const { suggestions, searchTime } = 
      await advancedSearchService.generateSuggestions({
        query: query.trim(),
        field,
        maxSuggestions,
      });

    return c.json({
      status: "success",
      message: "Search suggestions generated successfully",
      suggestions,
      query: query.trim(),
      field,
      searchTime,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error generating suggestions:", error);
    return errorResponse(
      c,
      "Suggestion generation failed",
      error instanceof Error ? error.message : "Search engine error",
      500,
    );
  }
});

// POST /highlight - Search with highlight information
searchRouter.post(
  "/highlight",
  zValidator("json", HighlightSearchRequestSchema),
  async (c) => {
    try {
      const request = c.req.valid("json");
      const { results, totalResults, searchTime } = 
        await advancedSearchService.highlightSearch(request);

      return c.json({
        status: "success",
        message: "Highlight search completed successfully",
        results,
        totalResults,
        query: request.query,
        searchTime,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error("Error in highlight search:", error);
      return errorResponse(
        c,
        "Highlight search failed",
        error instanceof Error ? error.message : "Search engine error",
        500,
      );
    }
  },
);

// GET /fulltext/:query - Simple full-text search with GET
searchRouter.get("/fulltext/:query", async (c) => {
  try {
    const query = c.req.param("query");

    if (!query || query.trim() === "") {
      return errorResponse(
        c,
        "Search query is required",
        "Query parameter cannot be empty",
        400,
      );
    }

    // Parse query parameters
    const caseSensitive = c.req.query("case") === "true";
    const contextLines = Math.max(
      0,
      Math.min(parseInt(c.req.query("context") || "0", 10), 10),
    );
    const maxResults = Math.max(
      1,
      Math.min(parseInt(c.req.query("limit") || "100", 10), 1000),
    );
    const fileTypes = c.req.query("types")?.split(",").filter(Boolean) || [];

    const searchResults = await nodeFileService.fullTextSearch(query.trim(), {
      caseSensitive,
      contextLines,
      fileTypes,
      maxResults,
    });

    return c.json({
      status: "success",
      message: "Full-text search completed successfully",
      ...searchResults,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error in full-text search:", error);
    return errorResponse(
      c,
      "Full-text search failed",
      error instanceof Error ? error.message : "Search engine error",
      500,
    );
  }
});

// GET /:query - Search nodes (define parameter routes after specific routes)
searchRouter.get("/:query", async (c) => {
  try {
    const query = c.req.param("query");

    // Handle empty query - return 400 as expected by tests
    if (!query || query.trim() === "") {
      return c.json(
        {
          status: "error",
          message: "Search query cannot be empty",
          error: "Search query parameter is required",
          timestamp: new Date().toISOString(),
        },
        400,
      );
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
    return c.json(
      {
        status: "error",
        message: "Failed to search nodes",
        error: error instanceof Error ? error.message : "File system error",
        timestamp: new Date().toISOString(),
      },
      500,
    );
  }
});

// GET / - Handle empty search query or get all tags
searchRouter.get("/", async (c) => {
  // Check if this is a search request with empty query (from /search/)
  const path = c.req.path;
  if (path === "/search/" || path === "/api/search/") {
    // Handle empty search query - return 400 as expected by tests
    return c.json(
      {
        status: "error",
        message: "Search query cannot be empty",
        error: "Search query parameter is required",
        timestamp: new Date().toISOString(),
      },
      400,
    );
  }

  // Otherwise, return all tags
  try {
    const result = nodeFileService.getAllTags();
    return successResponse(c, "Tags retrieved successfully", result);
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
