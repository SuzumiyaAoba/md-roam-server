import { Hono } from "hono";
import { NodeFileService } from "@/shared/services/node-file-service";
import { errorResponse, successResponse } from "@/shared/lib/response";

const fileRouter = new Hono();
const nodeFileService = new NodeFileService(
  process.env.ORG_ROAM_DIRECTORY || "./tmp/org-roam"
);

// GET /files - Get all files with database info
fileRouter.get("/", async (c) => {
  try {
    const nodes = nodeFileService.getAllNodes();
    const fs = require('fs');
    
    // Enhance nodes with file system stats
    const filesWithStats = nodes.map(node => {
      try {
        const stats = fs.statSync(node.path);
        return {
          id: node.id,
          file: node.file,
          title: node.title,
          path: node.path,
          size: stats.size,
          mtime: stats.mtime.toISOString()
        };
      } catch (error) {
        // Return node without stats if file can't be read
        return {
          id: node.id,
          file: node.file,
          title: node.title,
          path: node.path
        };
      }
    });
    
    return c.json({
      status: "success",
      message: "Files retrieved successfully",
      files: filesWithStats,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error retrieving files:", error);
    return errorResponse(
      c,
      "Failed to retrieve files",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

// GET /raw - Get raw file system info
fileRouter.get("/raw", async (c) => {
  try {
    // Get raw file system information without database data
    const result = nodeFileService.getRawFiles();
    return c.json({
      status: "success",
      message: "Raw files retrieved successfully", 
      files: result,
      timestamp: new Date().toISOString(),
    });
  } catch (error) {
    console.error("Error retrieving raw files:", error);
    return errorResponse(
      c,
      "Failed to retrieve raw files",
      error instanceof Error ? error.message : "File system error",
      500,
    );
  }
});

export { fileRouter };
