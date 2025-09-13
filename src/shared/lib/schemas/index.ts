import { z } from "zod";

// Base response schemas
export const BaseResponseSchema = z.object({
  status: z.enum(["success", "error"]),
  message: z.string(),
  timestamp: z.string(),
});

export const SuccessResponseSchema = <T extends z.ZodTypeAny>(dataSchema: T) =>
  BaseResponseSchema.extend({
    status: z.literal("success"),
    data: dataSchema.optional(),
  });

export const ErrorResponseSchema = BaseResponseSchema.extend({
  status: z.literal("error"),
  error_code: z.string().optional(),
  details: z.record(z.string(), z.unknown()).optional(),
});

// Node schemas
export const NodeMetadataSchema = z.object({
  tags: z.array(z.string()).optional(),
  aliases: z.array(z.string()).optional(),
  refs: z.array(z.string()).optional(),
  category: z.string().optional(),
});

export const NodeSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  file_type: z.enum(["md", "org"]),
  path: z.string(),
  mtime: z.string().optional(),
  size: z.number().optional(),
});

export const NodeContentSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  content: z.string(),
  metadata: NodeMetadataSchema,
});

// Request schemas
export const CreateNodeRequestSchema = z.object({
  title: z.string().min(1, "Title is required"),
  content: z.string().default(""),
  tags: z.array(z.string()).optional(),
  aliases: z.array(z.string()).optional(),
  refs: z.array(z.string()).optional(),
  category: z.string().optional(),
  file_type: z.enum(["md", "org"]).default("md"),
});

export const UpdateNodeRequestSchema = z.object({
  title: z.string().min(1).optional(),
  content: z.string().optional(),
  tags: z.array(z.string()).optional(),
  aliases: z.array(z.string()).optional(),
  refs: z.array(z.string()).optional(),
  category: z.string().optional(),
});

// Path parameter schemas
export const NodeIdParamSchema = z.object({
  id: z.string().min(1, "Node ID is required"),
});

export const TagNameParamSchema = z.object({
  tag: z.string().min(1, "Tag name is required"),
});

// Query parameter schemas
export const SearchQuerySchema = z.object({
  query: z.string().optional(),
  limit: z.coerce.number().int().positive().max(1000).default(50),
  offset: z.coerce.number().int().min(0).default(0),
});

export const PaginationQuerySchema = z.object({
  limit: z.coerce.number().int().positive().max(1000).default(50),
  offset: z.coerce.number().int().min(0).default(0),
});

// File schemas
export const FileInfoSchema = z.object({
  id: z.string(),
  file: z.string(),
  title: z.string(),
  mtime: z.string(),
  size: z.number(),
  path: z.string(),
});

export const RawFileSchema = z.object({
  file: z.string(),
  path: z.string(),
  size: z.number(),
  mtime: z.string(),
});

// Search schemas
export const SearchResultSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  content_preview: z.string().optional(),
  score: z.number().optional(),
});

export const SearchResponseSchema = SuccessResponseSchema(
  z.object({
    query: z.string(),
    results: z.array(SearchResultSchema),
    total_count: z.number(),
  }),
);

// Full-text search schemas
export const FullTextSearchRequestSchema = z.object({
  query: z.string().min(1, "Search query is required"),
  caseSensitive: z.boolean().default(false),
  regex: z.boolean().default(false),
  contextLines: z.number().int().min(0).max(10).default(0),
  fileTypes: z.array(z.string()).default([]),
  maxResults: z.number().int().min(1).max(1000).default(100),
});

// Advanced search schemas
export const FuzzySearchRequestSchema = z.object({
  query: z.string().min(1, "Search query is required"),
  threshold: z.number().min(0).max(1).default(0.6),
  maxResults: z.number().int().min(1).max(1000).default(50),
  fields: z.array(z.enum(["title", "content", "tags"])).default(["title", "content"]),
});

export const PhraseSearchRequestSchema = z.object({
  phrase: z.string().min(1, "Search phrase is required"),
  caseSensitive: z.boolean().default(false),
  fields: z.array(z.enum(["title", "content", "tags"])).default(["title", "content"]),
  maxResults: z.number().int().min(1).max(1000).default(50),
});

export const FieldSearchRequestSchema = z.object({
  query: z.string().min(1, "Search query is required"),
  field: z.enum(["title", "content", "tags", "category", "aliases", "refs"]),
  caseSensitive: z.boolean().default(false),
  exact: z.boolean().default(false),
  maxResults: z.number().int().min(1).max(1000).default(50),
});

export const SuggestionsRequestSchema = z.object({
  query: z.string().min(1, "Query string is required"),
  field: z.enum(["title", "content", "tags", "category"]).default("title"),
  maxSuggestions: z.number().int().min(1).max(20).default(10),
});

export const HighlightSearchRequestSchema = z.object({
  query: z.string().min(1, "Search query is required"),
  caseSensitive: z.boolean().default(false),
  highlightTag: z.string().default("mark"),
  maxResults: z.number().int().min(1).max(1000).default(50),
  snippetLength: z.number().int().min(50).max(500).default(200),
});

export const FullTextSearchMatchSchema = z.object({
  file: z.string(),
  nodeId: z.string().optional(),
  title: z.string().optional(),
  line: z.number(),
  content: z.string(),
  context: z
    .object({
      before: z.array(z.string()),
      after: z.array(z.string()),
    })
    .optional(),
});

export const FullTextSearchResponseSchema = SuccessResponseSchema(
  z.object({
    matches: z.array(FullTextSearchMatchSchema),
    totalMatches: z.number(),
    query: z.string(),
    searchTime: z.number(),
  }),
);

// Advanced search response schemas
export const FuzzySearchResultSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  score: z.number().min(0).max(1),
  matches: z.array(z.object({
    field: z.string(),
    value: z.string(),
    distance: z.number(),
  })),
});

export const PhraseSearchResultSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  matches: z.array(z.object({
    field: z.string(),
    snippet: z.string(),
    position: z.number(),
  })),
});

export const FieldSearchResultSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  field: z.string(),
  value: z.string(),
  matchType: z.enum(["exact", "partial", "fuzzy"]),
});

export const SuggestionSchema = z.object({
  text: z.string(),
  type: z.enum(["title", "content", "tag", "category"]),
  nodeId: z.string().optional(),
  score: z.number().optional(),
});

export const HighlightSearchResultSchema = z.object({
  id: z.string(),
  title: z.string(),
  file: z.string(),
  snippet: z.string(),
  highlights: z.array(z.object({
    field: z.string(),
    text: z.string(),
    positions: z.array(z.object({
      start: z.number(),
      end: z.number(),
    })),
  })),
});

export const FuzzySearchResponseSchema = SuccessResponseSchema(
  z.object({
    results: z.array(FuzzySearchResultSchema),
    totalResults: z.number(),
    query: z.string(),
    threshold: z.number(),
    searchTime: z.number(),
  }),
);

export const PhraseSearchResponseSchema = SuccessResponseSchema(
  z.object({
    results: z.array(PhraseSearchResultSchema),
    totalResults: z.number(),
    phrase: z.string(),
    searchTime: z.number(),
  }),
);

export const FieldSearchResponseSchema = SuccessResponseSchema(
  z.object({
    results: z.array(FieldSearchResultSchema),
    totalResults: z.number(),
    query: z.string(),
    field: z.string(),
    searchTime: z.number(),
  }),
);

export const SuggestionsResponseSchema = SuccessResponseSchema(
  z.object({
    suggestions: z.array(SuggestionSchema),
    query: z.string(),
    field: z.string(),
    searchTime: z.number(),
  }),
);

export const HighlightSearchResponseSchema = SuccessResponseSchema(
  z.object({
    results: z.array(HighlightSearchResultSchema),
    totalResults: z.number(),
    query: z.string(),
    searchTime: z.number(),
  }),
);

// Statistics schemas
export const DatabaseStatsSchema = z.object({
  total_nodes: z.number(),
  total_files: z.number(),
  total_tags: z.number(),
  total_aliases: z.number(),
  total_refs: z.number(),
  total_citations: z.number(),
  total_links: z.number(),
  database_size: z.string(),
  last_sync: z.string(),
});

// Tag schemas
export const TagInfoSchema = z.object({
  tag: z.string(),
  count: z.number(),
  node_ids: z.array(z.string()),
});

export const TagsResponseSchema = SuccessResponseSchema(
  z.object({
    tags: z.array(TagInfoSchema),
    total_count: z.number(),
  }),
);

// Link schemas
export const LinkSchema = z.object({
  source: z.string(),
  target: z.string(),
  type: z.string(),
});

export const LinksResponseSchema = SuccessResponseSchema(
  z.object({
    node_id: z.string(),
    links: z.array(LinkSchema),
    total_count: z.number(),
  }),
);

// Server info schemas
export const ServerInfoSchema = z.object({
  version: z.string(),
  server_port: z.number(),
  org_roam_directory: z.string(),
  endpoints: z.array(z.string()),
});

// Response schemas for specific endpoints
export const NodesResponseSchema = SuccessResponseSchema(z.array(NodeSchema));
export const NodeResponseSchema = SuccessResponseSchema(NodeSchema);
export const NodeContentResponseSchema =
  SuccessResponseSchema(NodeContentSchema);
export const FilesResponseSchema = SuccessResponseSchema(
  z.object({
    files: z.array(FileInfoSchema),
  }),
);
export const RawFilesResponseSchema = SuccessResponseSchema(
  z.object({
    files: z.array(RawFileSchema),
  }),
);
export const StatsResponseSchema = SuccessResponseSchema(DatabaseStatsSchema);
export const ServerInfoResponseSchema = SuccessResponseSchema(ServerInfoSchema);

// Type inference helpers
export type CreateNodeRequest = z.infer<typeof CreateNodeRequestSchema>;
export type UpdateNodeRequest = z.infer<typeof UpdateNodeRequestSchema>;
export type NodeIdParam = z.infer<typeof NodeIdParamSchema>;
export type TagNameParam = z.infer<typeof TagNameParamSchema>;
export type SearchQuery = z.infer<typeof SearchQuerySchema>;
export type PaginationQuery = z.infer<typeof PaginationQuerySchema>;
export type Node = z.infer<typeof NodeSchema>;
export type NodeContent = z.infer<typeof NodeContentSchema>;
export type NodeMetadata = z.infer<typeof NodeMetadataSchema>;
export type FileInfo = z.infer<typeof FileInfoSchema>;
export type RawFile = z.infer<typeof RawFileSchema>;
export type SearchResult = z.infer<typeof SearchResultSchema>;
export type DatabaseStats = z.infer<typeof DatabaseStatsSchema>;
export type TagInfo = z.infer<typeof TagInfoSchema>;
export type Link = z.infer<typeof LinkSchema>;
export type ServerInfo = z.infer<typeof ServerInfoSchema>;
export type FullTextSearchRequest = z.infer<typeof FullTextSearchRequestSchema>;
export type FullTextSearchMatch = z.infer<typeof FullTextSearchMatchSchema>;
export type FullTextSearchResponse = z.infer<
  typeof FullTextSearchResponseSchema
>;

// Advanced search type exports
export type FuzzySearchRequest = z.infer<typeof FuzzySearchRequestSchema>;
export type PhraseSearchRequest = z.infer<typeof PhraseSearchRequestSchema>;
export type FieldSearchRequest = z.infer<typeof FieldSearchRequestSchema>;
export type SuggestionsRequest = z.infer<typeof SuggestionsRequestSchema>;
export type HighlightSearchRequest = z.infer<typeof HighlightSearchRequestSchema>;

export type FuzzySearchResult = z.infer<typeof FuzzySearchResultSchema>;
export type PhraseSearchResult = z.infer<typeof PhraseSearchResultSchema>;
export type FieldSearchResult = z.infer<typeof FieldSearchResultSchema>;
export type Suggestion = z.infer<typeof SuggestionSchema>;
export type HighlightSearchResult = z.infer<typeof HighlightSearchResultSchema>;

export type FuzzySearchResponse = z.infer<typeof FuzzySearchResponseSchema>;
export type PhraseSearchResponse = z.infer<typeof PhraseSearchResponseSchema>;
export type FieldSearchResponse = z.infer<typeof FieldSearchResponseSchema>;
export type SuggestionsResponse = z.infer<typeof SuggestionsResponseSchema>;
export type HighlightSearchResponse = z.infer<typeof HighlightSearchResponseSchema>;
