import { Hono } from 'hono'
import { validator } from 'hono/validator'
import type { CreateNodeRequest, UpdateNodeRequest } from '../types'
import { EmacsClient } from '../utils/emacs-client'
import {
  errorResponse,
  notFoundResponse,
  successResponse,
  validationErrorResponse,
} from '../utils/response'

const nodes = new Hono()
const emacsClient = new EmacsClient()

// POST /nodes - Create new node
nodes.post(
  '/',
  validator('json', (value, c) => {
    const { title, content, file_type, category, tags, aliases, refs } = value as CreateNodeRequest

    if (!title || typeof title !== 'string' || title.trim() === '') {
      return validationErrorResponse(c, 'Title is required and cannot be empty')
    }

    if (file_type && !['md', 'org'].includes(file_type)) {
      return validationErrorResponse(c, 'file_type must be either "md" or "org"')
    }

    return {
      title: title.trim(),
      content: content || '',
      file_type: file_type || 'md',
      category,
      tags: Array.isArray(tags) ? tags : [],
      aliases: Array.isArray(aliases) ? aliases : [],
      refs: Array.isArray(refs) ? refs : [],
    }
  }),
  async (c) => {
    try {
      const nodeData = c.req.valid('json')

      // TODO: Implement node creation logic
      // For now, return a mock response
      const mockNode = {
        id: `node-${Date.now()}`,
        title: nodeData.title,
        content: nodeData.content,
        file_type: nodeData.file_type,
        category: nodeData.category,
        tags: nodeData.tags,
        aliases: nodeData.aliases,
        refs: nodeData.refs,
        created_at: new Date().toISOString(),
      }

      return successResponse(c, 'Node created successfully', mockNode, 201)
    } catch (error) {
      console.error('Error creating node:', error)
      return errorResponse(
        c,
        'Failed to create node',
        error instanceof Error ? error.message : 'Unknown error',
        500,
      )
    }
  },
)

// PUT /nodes/:id - Update node
nodes.put(
  '/:id',
  validator('json', (value, c) => {
    const { title, content, category, tags, aliases, refs } = value as UpdateNodeRequest

    if (title !== undefined && (typeof title !== 'string' || title.trim() === '')) {
      return validationErrorResponse(c, 'Title cannot be empty if provided')
    }

    return {
      title: title?.trim(),
      content,
      category,
      tags: Array.isArray(tags) ? tags : undefined,
      aliases: Array.isArray(aliases) ? aliases : undefined,
      refs: Array.isArray(refs) ? refs : undefined,
    }
  }),
  async (c) => {
    try {
      const nodeId = c.req.param('id')
      const updateData = c.req.valid('json')

      if (!nodeId) {
        return validationErrorResponse(c, 'Node ID is required')
      }

      // TODO: Implement node update logic
      // For now, return a mock response
      const mockNode = {
        id: nodeId,
        title: updateData.title || 'Updated Node',
        content: updateData.content || 'Updated content',
        category: updateData.category,
        tags: updateData.tags || [],
        aliases: updateData.aliases || [],
        refs: updateData.refs || [],
        updated_at: new Date().toISOString(),
      }

      return successResponse(c, 'Node updated successfully', mockNode)
    } catch (error) {
      console.error('Error updating node:', error)
      return errorResponse(
        c,
        'Failed to update node',
        error instanceof Error ? error.message : 'Unknown error',
        500,
      )
    }
  },
)

// DELETE /nodes/:id - Delete node
nodes.delete('/:id', async (c) => {
  try {
    const nodeId = c.req.param('id')

    if (!nodeId) {
      return validationErrorResponse(c, 'Node ID is required')
    }

    // TODO: Implement node deletion logic
    // For now, return a mock response

    return successResponse(c, 'Node deleted successfully', {
      id: nodeId,
      deleted_at: new Date().toISOString(),
    })
  } catch (error) {
    console.error('Error deleting node:', error)
    return errorResponse(
      c,
      'Failed to delete node',
      error instanceof Error ? error.message : 'Unknown error',
      500,
    )
  }
})

// GET requests are delegated to Emacs server
nodes.get('/', async (c) => {
  try {
    const result = await emacsClient.getNodes()
    return successResponse(c, 'Nodes retrieved successfully', result.data || result)
  } catch (error) {
    console.error('Error retrieving nodes:', error)
    return errorResponse(
      c,
      'Failed to retrieve nodes',
      error instanceof Error ? error.message : 'Emacs server unavailable',
      503,
    )
  }
})

nodes.get('/:id', async (c) => {
  try {
    const nodeId = c.req.param('id')
    const result = await emacsClient.getNode(nodeId)

    if (result.status === 'error') {
      return notFoundResponse(c, 'Node')
    }

    return successResponse(c, 'Node retrieved successfully', result.data || result)
  } catch (error) {
    console.error('Error retrieving node:', error)
    return errorResponse(
      c,
      'Failed to retrieve node',
      error instanceof Error ? error.message : 'Emacs server unavailable',
      503,
    )
  }
})

nodes.get('/:id/content', async (c) => {
  try {
    const nodeId = c.req.param('id')
    const result = await emacsClient.getNodeContent(nodeId)

    if (result.status === 'error') {
      return notFoundResponse(c, 'Node')
    }

    return successResponse(c, 'Node content retrieved successfully', result.data || result)
  } catch (error) {
    console.error('Error retrieving node content:', error)
    return errorResponse(
      c,
      'Failed to retrieve node content',
      error instanceof Error ? error.message : 'Emacs server unavailable',
      503,
    )
  }
})

export default nodes
