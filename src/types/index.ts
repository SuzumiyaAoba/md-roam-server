// Re-export types from schemas for backward compatibility
export type {
  CreateNodeRequest,
  DatabaseStats,
  FileInfo,
  Link,
  Node,
  NodeContent,
  NodeIdParam,
  NodeMetadata,
  PaginationQuery,
  RawFile,
  SearchQuery,
  SearchResult,
  ServerInfo,
  TagInfo,
  TagNameParam,
  UpdateNodeRequest,
} from "../schemas";

// Legacy type aliases for backward compatibility
export type ApiResponse<T = unknown> = {
  status: "success" | "error";
  message: string;
  timestamp: string;
} & (
  | {
      status: "success";
      data?: T;
    }
  | {
      status: "error";
      error?: string;
    }
);

export type NodeSearchResult = {
  nodes: Node[];
  count: number;
  query?: string;
};
