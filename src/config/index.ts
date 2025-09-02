export const config = {
  api: {
    port: process.env.API_PORT ? Number.parseInt(process.env.API_PORT, 10) : 3001,
  },
  emacs: {
    serverUrl: process.env.EMACS_SERVER_URL || 'http://localhost:8080',
  },
  environment: process.env.NODE_ENV || 'development',
} as const
