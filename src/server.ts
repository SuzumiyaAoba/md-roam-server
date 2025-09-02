import { serve } from '@hono/node-server'
import { Hono } from 'hono'
import { cors } from 'hono/cors'
import { logger } from 'hono/logger'
import { prettyJSON } from 'hono/pretty-json'
import nodesRoutes from './api/nodes'

const app = new Hono()

// Middleware
app.use(logger())
app.use(prettyJSON())
app.use(
  '*',
  cors({
    origin: ['http://localhost:3000', 'http://localhost:35901'],
    allowHeaders: ['Content-Type', 'Authorization'],
    allowMethods: ['GET', 'POST', 'PUT', 'DELETE'],
  }),
)

// Health check
app.get('/health', (c) => {
  return c.json({
    status: 'ok',
    timestamp: new Date().toISOString(),
    service: 'md-roam-server-api',
  })
})

// API Routes
app.route('/api/nodes', nodesRoutes)

// Default route
app.get('/', (c) => {
  return c.json({
    message: 'MD-Roam Server API',
    version: '1.0.0',
    endpoints: {
      health: '/health',
      nodes: '/api/nodes',
    },
  })
})

const port = process.env.API_PORT ? Number.parseInt(process.env.API_PORT, 10) : 3001

console.log(`🚀 MD-Roam Server API starting on port ${port}`)

serve({
  fetch: app.fetch,
  port,
})
