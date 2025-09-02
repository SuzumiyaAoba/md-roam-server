export interface Node {
  id: string;
  title: string;
  content?: string;
  file?: string;
  file_type: "md" | "org";
  path?: string;
  level?: number;
  category?: string;
  tags: string[];
  aliases: string[];
  refs: string[];
  created_at?: string;
  updated_at?: string;
}

export interface CreateNodeRequest {
  title: string;
  content?: string;
  file_type?: "md" | "org";
  category?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
}

export interface UpdateNodeRequest {
  title?: string;
  content?: string;
  category?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
}

export interface ApiResponse<T = any> {
  status: "success" | "error";
  message: string;
  timestamp: string;
  data?: T;
  error?: string;
}

export interface NodeSearchResult {
  nodes: Node[];
  count: number;
  query?: string;
}
