// Core API Response Types
export interface BaseResponse {
  status: "success" | "error";
  message: string;
  timestamp: string;
}

export interface SuccessResponse<T = unknown> extends BaseResponse {
  status: "success";
  data?: T;
}

export interface ErrorResponse extends BaseResponse {
  status: "error";
  error_code?: string;
  details?: Record<string, unknown>;
}

// Node Types
export interface Node {
  id: string;
  title: string;
  file: string;
  file_type: "md" | "org";
  path: string;
  mtime?: string;
  size?: number;
}

export interface NodeContent {
  id: string;
  title: string;
  file: string;
  content: string;
  metadata: NodeMetadata;
}

export interface NodeMetadata {
  tags?: string[];
  aliases?: string[];
  refs?: string[];
  category?: string;
  [key: string]: unknown;
}

export interface CreateNodePayload {
  title: string;
  content?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
  category?: string;
  file_type?: "md" | "org";
}

export interface UpdateNodePayload extends Partial<CreateNodePayload> {
  title?: string;
}

// File Types
export interface FileInfo {
  id: string;
  file: string;
  title: string;
  mtime: string;
  size: number;
  path: string;
}

export interface RawFile {
  file: string;
  path: string;
  size: number;
  mtime: string;
}

// Search Types
export interface SearchResult {
  id: string;
  title: string;
  file: string;
  content_preview?: string;
  score?: number;
}

export interface SearchResponse extends SuccessResponse {
  query: string;
  results: SearchResult[];
  total_count: number;
}

// Statistics Types
export interface DatabaseStats {
  total_nodes: number;
  total_files: number;
  total_tags: number;
  total_aliases: number;
  total_refs: number;
  total_citations: number;
  total_links: number;
  database_size: string;
  last_sync: string;
}

// Tag Types
export interface TagInfo {
  tag: string;
  count: number;
  node_ids: string[];
}

export interface TagsResponse extends SuccessResponse {
  tags: TagInfo[];
  total_count: number;
}

// Link Types
export interface Link {
  source: string;
  target: string;
  type: string;
}

export interface LinksResponse extends SuccessResponse {
  node_id: string;
  links: Link[];
  total_count: number;
}

// Server Info Types
export interface ServerInfo {
  version: string;
  server_port: number;
  ui_port: number;
  ui_enabled: boolean;
  org_roam_directory: string;
  endpoints: string[];
}

// Test Utility Types
export interface TestContext {
  testNodes: Node[];
  cleanup: () => Promise<void>;
}

export interface TestScenario {
  name: string;
  setup?: () => Promise<void>;
  test: () => Promise<void>;
  cleanup?: () => Promise<void>;
}

// Configuration Types
export interface TestConfig {
  SERVER_URL: string;
  UI_URL: string;
  TIMEOUT: number;
  STARTUP_DELAY: number;
}

// Assertion Helpers
export interface ExpectedResponse<T = unknown> {
  status?: number;
  contentType?: string | RegExp;
  body?: Partial<T>;
  headers?: Record<string, string | RegExp>;
}

export interface TestData {
  valid: {
    nodes: CreateNodePayload[];
    updates: UpdateNodePayload[];
  };
  invalid: {
    nodes: Partial<CreateNodePayload>[];
    updates: Partial<UpdateNodePayload>[];
  };
}
