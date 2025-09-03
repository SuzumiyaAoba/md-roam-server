/**
 * Emacs Server Client - Communicates with the Emacs org-roam server
 * GET requests are handled by Emacs server on port 8080
 * POST/PUT/DELETE requests are handled by this Hono API server
 */

const EMACS_SERVER_URL =
  process.env.EMACS_SERVER_URL || "http://localhost:8080";

export class EmacsClient {
  private baseUrl: string;

  constructor(baseUrl = EMACS_SERVER_URL) {
    this.baseUrl = baseUrl;
  }

  async get(path: string): Promise<unknown> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        method: "GET",
        headers: {
          "Content-Type": "application/json",
        },
      });

      if (!response.ok) {
        throw new Error(
          `Emacs server error: ${response.status} ${response.statusText}`,
        );
      }

      return await response.json();
    } catch (error) {
      console.error("Error communicating with Emacs server:", error);
      throw error;
    }
  }

  async post(path: string, data: unknown): Promise<unknown> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(data),
      });

      if (!response.ok) {
        throw new Error(
          `Emacs server error: ${response.status} ${response.statusText}`,
        );
      }

      return await response.json();
    } catch (error) {
      console.error("Error communicating with Emacs server:", error);
      throw error;
    }
  }

  async put(path: string, data: unknown): Promise<unknown> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(data),
      });

      if (!response.ok) {
        throw new Error(
          `Emacs server error: ${response.status} ${response.statusText}`,
        );
      }

      return await response.json();
    } catch (error) {
      console.error("Error communicating with Emacs server:", error);
      throw error;
    }
  }

  async delete(path: string): Promise<unknown> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        method: "DELETE",
        headers: {
          "Content-Type": "application/json",
        },
      });

      if (!response.ok) {
        throw new Error(
          `Emacs server error: ${response.status} ${response.statusText}`,
        );
      }

      return await response.json();
    } catch (error) {
      console.error("Error communicating with Emacs server:", error);
      throw error;
    }
  }

  async checkHealth(): Promise<boolean> {
    try {
      const response = await this.get("/stats");
      return (response as { status: string }).status === "success";
    } catch {
      return false;
    }
  }

  // Node retrieval methods (delegated to Emacs)
  async getNodes() {
    return this.get("/nodes");
  }

  async getNode(id: string) {
    return this.get(`/nodes/${id}`);
  }

  async getNodeContent(id: string) {
    return this.get(`/nodes/${id}/content`);
  }

  async searchNodes(query: string) {
    return this.get(`/search/${encodeURIComponent(query)}`);
  }

  async getStats() {
    return this.get("/stats");
  }

  // Node mutation methods (delegated to Emacs)
  async createNode(data: unknown) {
    return this.post("/nodes", data);
  }

  async updateNode(id: string, data: unknown) {
    return this.put(`/nodes/${id}`, data);
  }

  async deleteNode(id: string) {
    return this.delete(`/nodes/${id}`);
  }

  async addTagToNode(id: string, tag: string) {
    return this.post(`/nodes/${id}/tags`, { tag });
  }

  async removeTagFromNode(id: string, tag: string) {
    return this.delete(`/nodes/${id}/tags/${tag}`);
  }

  async addCategoryToNode(id: string, category: string) {
    return this.post(`/nodes/${id}/categories`, { category });
  }

  async removeCategoryFromNode(id: string, category: string) {
    return this.delete(`/nodes/${id}/categories/${category}`);
  }
}
