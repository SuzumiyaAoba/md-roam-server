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
  ui_port: z.number(),
  ui_enabled: z.boolean(),
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
