import type { Context } from "hono";

export function successResponse<T>(
  c: Context,
  message: string,
  data?: T,
  status = 200,
): Response {
  const response = {
    status: "success" as const,
    message,
    timestamp: new Date().toISOString(),
    data,
  };
  return c.json(response, status as Parameters<typeof c.json>[1]);
}

export function errorResponse(
  c: Context,
  message: string,
  error?: string,
  status = 400,
): Response {
  const response = {
    status: "error" as const,
    message,
    timestamp: new Date().toISOString(),
    error,
  };
  return c.json(response, status as Parameters<typeof c.json>[1]);
}

export function notFoundResponse(c: Context, resource = "Resource"): Response {
  return errorResponse(c, `${resource} not found`, undefined, 404);
}

export function validationErrorResponse(c: Context, error: string): Response {
  return errorResponse(c, "Validation failed", error, 422);
}
