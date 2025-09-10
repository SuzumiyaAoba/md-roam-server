import type { Context, MiddlewareHandler } from "hono";
import type { z } from "zod";

export function customValidator<T extends z.ZodTypeAny>(
  target: "json" | "param" | "query",
  schema: T,
): MiddlewareHandler {
  return async (c: Context, next) => {
    let data: unknown;

    try {
      switch (target) {
        case "json":
          data = await c.req.json();
          break;
        case "param":
          data = c.req.param();
          break;
        case "query":
          data = c.req.query();
          break;
      }
    } catch (_error) {
      return c.json(
        {
          status: "error",
          message: "Invalid request format",
          error: "Request body must be valid JSON",
          timestamp: new Date().toISOString(),
        },
        400,
      );
    }

    const result = schema.safeParse(data);

    if (!result.success) {
      // Create a more descriptive error message with field names
      const errorDetails = result.error.issues
        .map((issue) => {
          const field = issue.path?.[0] || "field";
          return `${String(field)}: ${issue.message}`;
        })
        .join(", ");

      return c.json(
        {
          status: "error",
          message: `Validation failed: ${errorDetails}`,
          error: errorDetails,
          timestamp: new Date().toISOString(),
        },
        400,
      );
    }

    // Store the validated data in the context (manually)
    // biome-ignore lint/suspicious/noExplicitAny: Required for Hono context extension
    (c as any).validatedData = (c as any).validatedData || {};
    // biome-ignore lint/suspicious/noExplicitAny: Required for Hono context extension
    (c as any).validatedData[target] = result.data;

    await next();
  };
}
