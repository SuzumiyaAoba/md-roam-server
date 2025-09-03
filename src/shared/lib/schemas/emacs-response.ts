import { z } from "zod";
import {
  FileInfoSchema,
  NodeSchema,
  SearchResultSchema,
  TagInfoSchema,
} from "./index";

// Base Emacs server response schema
export const EmacsResponseSchema = z.object({
  status: z.enum(["success", "error"]),
  message: z.string(),
  timestamp: z.string().optional(),
  error: z.string().optional(),
  error_type: z.string().optional(),
  data: z.unknown().optional(),
});

export const EmacsNodesResponseSchema = EmacsResponseSchema.extend({
  data: z.array(NodeSchema).optional(),
});

export const EmacsNodeResponseSchema = EmacsResponseSchema.extend({
  data: NodeSchema.optional(),
});

export const EmacsNodeContentResponseSchema = EmacsResponseSchema.extend({
  data: z
    .object({
      id: z.string(),
      title: z.string(),
      content: z.string(),
      file_path: z.string(),
      file_type: z.string(),
    })
    .optional(),
});

export const EmacsTagsResponseSchema = EmacsResponseSchema.extend({
  data: z.array(TagInfoSchema).optional(),
});

export const EmacsFilesResponseSchema = EmacsResponseSchema.extend({
  data: z.array(FileInfoSchema).optional(),
});

export const EmacsSearchResponseSchema = EmacsResponseSchema.extend({
  data: SearchResultSchema.optional(),
});

export const EmacsStatsResponseSchema = EmacsResponseSchema.extend({
  data: z
    .object({
      node_count: z.number(),
      file_count: z.number(),
      tag_count: z.number(),
      link_count: z.number(),
      database_size: z.number().optional(),
      org_roam_directory: z.string(),
      server_uptime: z.string().optional(),
    })
    .optional(),
});

export const EmacsConfigResponseSchema = EmacsResponseSchema.extend({
  data: z
    .object({
      org_roam_directory: z.string(),
      org_roam_db_location: z.string(),
      server_port: z.number(),
      server_host: z.string(),
    })
    .optional(),
});

export const EmacsSyncResponseSchema = EmacsResponseSchema.extend({
  data: z
    .object({
      synced_files: z.number(),
      added_nodes: z.number(),
      updated_nodes: z.number(),
      removed_nodes: z.number(),
    })
    .optional(),
});

export const EmacsLinksResponseSchema = EmacsResponseSchema.extend({
  data: z
    .array(
      z.object({
        id: z.string(),
        title: z.string(),
        file_path: z.string(),
        link_type: z.string().optional(),
      }),
    )
    .optional(),
});

export const EmacsAliasesResponseSchema = EmacsResponseSchema.extend({
  data: z.array(z.string()).optional(),
});

export const EmacsRefsResponseSchema = EmacsResponseSchema.extend({
  data: z.array(z.string()).optional(),
});

export const EmacsParsedNodeResponseSchema = EmacsResponseSchema.extend({
  data: z
    .object({
      id: z.string(),
      title: z.string(),
      file_path: z.string(),
      file_type: z.string(),
      metadata: z.record(z.string(), z.unknown()),
      body: z.string(),
    })
    .optional(),
});

// Generic schemas for endpoints that return node collections
export const EmacsTaggedNodesResponseSchema = EmacsResponseSchema.extend({
  data: z
    .array(
      z.object({
        id: z.string(),
        title: z.string(),
      }),
    )
    .optional(),
});

export const EmacsAliasedNodesResponseSchema = EmacsTaggedNodesResponseSchema;
export const EmacsReferencedNodesResponseSchema =
  EmacsTaggedNodesResponseSchema;
export const EmacsCitationNodesResponseSchema = EmacsTaggedNodesResponseSchema;

// Type exports
export type EmacsResponse = z.infer<typeof EmacsResponseSchema>;
export type EmacsNodesResponse = z.infer<typeof EmacsNodesResponseSchema>;
export type EmacsNodeResponse = z.infer<typeof EmacsNodeResponseSchema>;
export type EmacsNodeContentResponse = z.infer<
  typeof EmacsNodeContentResponseSchema
>;
export type EmacsTagsResponse = z.infer<typeof EmacsTagsResponseSchema>;
export type EmacsFilesResponse = z.infer<typeof EmacsFilesResponseSchema>;
export type EmacsSearchResponse = z.infer<typeof EmacsSearchResponseSchema>;
export type EmacsStatsResponse = z.infer<typeof EmacsStatsResponseSchema>;
export type EmacsConfigResponse = z.infer<typeof EmacsConfigResponseSchema>;
export type EmacsSyncResponse = z.infer<typeof EmacsSyncResponseSchema>;
export type EmacsLinksResponse = z.infer<typeof EmacsLinksResponseSchema>;
export type EmacsAliasesResponse = z.infer<typeof EmacsAliasesResponseSchema>;
export type EmacsRefsResponse = z.infer<typeof EmacsRefsResponseSchema>;
export type EmacsParsedNodeResponse = z.infer<
  typeof EmacsParsedNodeResponseSchema
>;
export type EmacsTaggedNodesResponse = z.infer<
  typeof EmacsTaggedNodesResponseSchema
>;
export type EmacsAliasedNodesResponse = z.infer<
  typeof EmacsAliasedNodesResponseSchema
>;
export type EmacsReferencedNodesResponse = z.infer<
  typeof EmacsReferencedNodesResponseSchema
>;
export type EmacsCitationNodesResponse = z.infer<
  typeof EmacsCitationNodesResponseSchema
>;
