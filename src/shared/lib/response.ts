import type { Context } from "hono";

export function successResponse(
  c: Context,
  message: string,
  data?: unknown,
  status = 200,
) {
  return c.json(
    {
      status: "success" as const,
      message,
      data,
      timestamp: new Date().toISOString(),
    },
    status as Parameters<typeof c.json>[1],
  );
}

export function errorResponse(
  c: Context,
  message: string,
  error?: string,
  status = 400,
) {
  return c.json(
    {
      status: "error" as const,
      message,
      error,
      timestamp: new Date().toISOString(),
    },
    status as Parameters<typeof c.json>[1],
  );
}

export function notFoundResponse(c: Context, resource: string) {
  return c.json(
    {
      status: "error" as const,
      message: `${resource} not found`,
      error: "RESOURCE_NOT_FOUND",
      timestamp: new Date().toISOString(),
    },
    404,
  );
}
