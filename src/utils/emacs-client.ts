/**
 * Emacs Server Client - Communicates with the Emacs org-roam server
 * GET requests are handled by Emacs server on port 8080
 * POST/PUT/DELETE requests are handled by this Hono API server
 */

const EMACS_SERVER_URL = process.env.EMACS_SERVER_URL || 'http://localhost:8080'

export class EmacsClient {
  private baseUrl: string

  constructor(baseUrl = EMACS_SERVER_URL) {
    this.baseUrl = baseUrl
  }

  async get(path: string): Promise<any> {
    try {
      const response = await fetch(`${this.baseUrl}${path}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      })

      if (!response.ok) {
        throw new Error(`Emacs server error: ${response.status} ${response.statusText}`)
      }

      return await response.json()
    } catch (error) {
      console.error('Error communicating with Emacs server:', error)
      throw error
    }
  }

  async checkHealth(): Promise<boolean> {
    try {
      const response = await this.get('/stats')
      return response.status === 'success'
    } catch {
      return false
    }
  }

  // Node retrieval methods (delegated to Emacs)
  async getNodes() {
    return this.get('/nodes')
  }

  async getNode(id: string) {
    return this.get(`/nodes/${id}`)
  }

  async getNodeContent(id: string) {
    return this.get(`/nodes/${id}/content`)
  }

  async searchNodes(query: string) {
    return this.get(`/search/${encodeURIComponent(query)}`)
  }

  async getStats() {
    return this.get('/stats')
  }
}
