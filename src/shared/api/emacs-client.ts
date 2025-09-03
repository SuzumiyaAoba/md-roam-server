/**
 * Emacs Server Client - Communicates with the Emacs org-roam server
 * GET requests are handled by Emacs server on port 8080
 * POST/PUT/DELETE requests are handled by this Hono API server
 */

import { z } from "zod";
import {
  type EmacsAliasesResponse,
  EmacsAliasesResponseSchema,
  type EmacsConfigResponse,
  EmacsConfigResponseSchema,
  type EmacsFilesResponse,
  EmacsFilesResponseSchema,
  type EmacsLinksResponse,
  EmacsLinksResponseSchema,
  type EmacsNodeContentResponse,
  EmacsNodeContentResponseSchema,
  type EmacsNodeResponse,
  EmacsNodeResponseSchema,
  type EmacsNodesResponse,
  EmacsNodesResponseSchema,
  type EmacsParsedNodeResponse,
  EmacsParsedNodeResponseSchema,
  type EmacsRefsResponse,
  EmacsRefsResponseSchema,
  type EmacsResponse,
  EmacsResponseSchema,
  type EmacsSearchResponse,
  EmacsSearchResponseSchema,
  type EmacsStatsResponse,
  EmacsStatsResponseSchema,
  type EmacsSyncResponse,
  EmacsSyncResponseSchema,
  type EmacsTagsResponse,
  EmacsTagsResponseSchema,
} from "@/shared/lib/schemas/emacs-response";

const EMACS_SERVER_URL =
  process.env["EMACS_SERVER_URL"] || "http://localhost:8080";

export class EmacsClient {
  private baseUrl: string;

  constructor(baseUrl = EMACS_SERVER_URL) {
    this.baseUrl = baseUrl;
  }

  private async request<T extends z.ZodType>(
    path: string,
    options: RequestInit,
    schema: T,
  ): Promise<z.infer<T>> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        ...options,
        headers: {
          "Content-Type": "application/json",
          ...options.headers,
        },
      });

      if (!response.ok) {
        throw new Error(
          `Emacs server error: ${response.status} ${response.statusText}`,
        );
      }

      const rawData = await response.json();
      return schema.parse(rawData);
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error("Emacs server response validation error:", error.issues);
        throw new Error(`Invalid response format: ${error.message}`);
      }
      console.error("Error communicating with Emacs server:", error);
      throw error;
    }
  }

  async get<T extends z.ZodType>(
    path: string,
    schema?: T,
  ): Promise<z.infer<T>> {
    const resolvedSchema = schema ?? (EmacsResponseSchema as unknown as T);
    return this.request(path, { method: "GET" }, resolvedSchema);
  }

  async post<T extends z.ZodType>(
    path: string,
    data: unknown,
    schema?: T,
  ): Promise<z.infer<T>> {
    const resolvedSchema = schema ?? (EmacsResponseSchema as unknown as T);
    return this.request(
      path,
      {
        method: "POST",
        body: JSON.stringify(data),
      },
      resolvedSchema,
    );
  }

  async put<T extends z.ZodType>(
    path: string,
    data: unknown,
    schema?: T,
  ): Promise<z.infer<T>> {
    const resolvedSchema = schema ?? (EmacsResponseSchema as unknown as T);
    return this.request(
      path,
      {
        method: "PUT",
        body: JSON.stringify(data),
      },
      resolvedSchema,
    );
  }

  async delete<T extends z.ZodType>(
    path: string,
    schema?: T,
  ): Promise<z.infer<T>> {
    const resolvedSchema = schema ?? (EmacsResponseSchema as unknown as T);
    return this.request(path, { method: "DELETE" }, resolvedSchema);
  }

  async checkHealth(): Promise<boolean> {
    try {
      const response = await this.get("/stats", EmacsStatsResponseSchema);
      return response.status === "success";
    } catch {
      return false;
    }
  }

  // Node retrieval methods (delegated to Emacs)
  async getNodes(): Promise<EmacsNodesResponse> {
    return this.get("/nodes", EmacsNodesResponseSchema);
  }

  async getNode(id: string): Promise<EmacsNodeResponse> {
    return this.get(`/nodes/${id}`, EmacsNodeResponseSchema);
  }

  async getNodeContent(id: string): Promise<EmacsNodeContentResponse> {
    return this.get(`/nodes/${id}/content`, EmacsNodeContentResponseSchema);
  }

  async searchNodes(query: string): Promise<EmacsSearchResponse> {
    return this.get(
      `/search/${encodeURIComponent(query)}`,
      EmacsSearchResponseSchema,
    );
  }

  async getStats(): Promise<EmacsStatsResponse> {
    return this.get("/stats", EmacsStatsResponseSchema);
  }

  async getFiles(): Promise<EmacsFilesResponse> {
    return this.get("/files", EmacsFilesResponseSchema);
  }

  async getTags(): Promise<EmacsTagsResponse> {
    return this.get("/tags", EmacsTagsResponseSchema);
  }

  async getConfig(): Promise<EmacsConfigResponse> {
    return this.get("/config", EmacsConfigResponseSchema);
  }

  async getNodeBacklinks(id: string): Promise<EmacsLinksResponse> {
    return this.get(`/nodes/${id}/backlinks`, EmacsLinksResponseSchema);
  }

  async getNodeLinks(id: string): Promise<EmacsLinksResponse> {
    return this.get(`/nodes/${id}/links`, EmacsLinksResponseSchema);
  }

  async getNodeAliases(id: string): Promise<EmacsAliasesResponse> {
    return this.get(`/nodes/${id}/aliases`, EmacsAliasesResponseSchema);
  }

  async getNodeRefs(id: string): Promise<EmacsRefsResponse> {
    return this.get(`/nodes/${id}/refs`, EmacsRefsResponseSchema);
  }

  async parseNode(id: string): Promise<EmacsParsedNodeResponse> {
    return this.get(`/nodes/${id}/parse`, EmacsParsedNodeResponseSchema);
  }

  // Node mutation methods (delegated to Emacs)
  async createNode(data: unknown): Promise<EmacsNodeResponse> {
    return this.post("/nodes", data, EmacsNodeResponseSchema);
  }

  async updateNode(id: string, data: unknown): Promise<EmacsNodeResponse> {
    return this.put(`/nodes/${id}`, data, EmacsNodeResponseSchema);
  }

  async deleteNode(id: string): Promise<EmacsResponse> {
    return this.delete(`/nodes/${id}`, EmacsResponseSchema);
  }

  async addTagToNode(id: string, tag: string): Promise<EmacsResponse> {
    return this.post(`/nodes/${id}/tags`, { tag }, EmacsResponseSchema);
  }

  async removeTagFromNode(id: string, tag: string): Promise<EmacsResponse> {
    return this.delete(`/nodes/${id}/tags/${tag}`, EmacsResponseSchema);
  }

  async addCategoryToNode(
    id: string,
    category: string,
  ): Promise<EmacsResponse> {
    return this.post(
      `/nodes/${id}/categories`,
      { category },
      EmacsResponseSchema,
    );
  }

  async removeCategoryFromNode(
    id: string,
    category: string,
  ): Promise<EmacsResponse> {
    return this.delete(
      `/nodes/${id}/categories/${category}`,
      EmacsResponseSchema,
    );
  }

  async syncDatabase(): Promise<EmacsSyncResponse> {
    return this.post("/sync", {}, EmacsSyncResponseSchema);
  }
}
