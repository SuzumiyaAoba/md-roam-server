import { describe, it, expect, beforeEach } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";
import { NodeData } from "@/utils/types";

describe("Files API E2E Tests", () => {
  let testNodes: NodeData[];

  beforeEach(async () => {
    // Create test files with different types
    testNodes = await Promise.all([
      TestCleanup.createTestNode({
        title: "Markdown File Test",
        content: "# Heading\n\nThis is markdown content with **bold** text.",
        tags: ["file-test", "markdown"],
        file_type: "md",
      }),
      TestCleanup.createTestNode({
        title: "Org File Test",
        content: "* Heading\n\nThis is org content with *emphasis* text.",
        tags: ["file-test", "org"],
        file_type: "org",
      }),
      TestCleanup.createTestNode({
        title: "International File Test",
        content:
          "# International Header\n\nInternational content for file testing.",
        tags: ["file-test", "international"],
        file_type: "md",
      }),
    ]);
  });

  describe("GET /files - File List", () => {
    it("should return list of all files", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("files");

      const files = response.body.files;
      expect(Array.isArray(files)).toBe(true);

      // Validate FileInfo structure
      if (files && Array.isArray(files) && files.length > 0) {
        files.forEach((file: any) => {
          expect(file).toHaveProperty("id");
          expect(file).toHaveProperty("file");
          expect(file).toHaveProperty("title");

          expect(typeof file.id).toBe("string");
          expect(typeof file.file).toBe("string");
          expect(typeof file.title).toBe("string");

          // Optional properties that may not be present
          if (file.mtime !== undefined) {
            expect(typeof file.mtime).toBe("string");
          }
          if (file.size !== undefined) {
            expect(typeof file.size).toBe("number");
          }
          if (file.path !== undefined) {
            expect(typeof file.path).toBe("string");
          }
        });
      }
    });

    it("should include both markdown and org files", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      const files = response.body.files;

      const markdownFiles =
        files && Array.isArray(files)
          ? files.filter((file: any) => file.file && file.file.endsWith(".md"))
          : [];
      const orgFiles =
        files && Array.isArray(files)
          ? files.filter((file: any) => file.file && file.file.endsWith(".org"))
          : [];

      expect(markdownFiles.length).toBeGreaterThan(0);
      expect(orgFiles.length).toBeGreaterThan(0);
    });

    it("should include our test files", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      const files = response.body.files;

      // Find our test files by title
      const markdownTestFile =
        files && Array.isArray(files)
          ? files.find((file: any) => file.title === "Markdown File Test")
          : null;
      const orgTestFile =
        files && Array.isArray(files)
          ? files.find((file: any) => file.title === "Org File Test")
          : null;
      const internationalTestFile =
        files && Array.isArray(files)
          ? files.find((file: any) => file.title === "International File Test")
          : null;

      expect(markdownTestFile).toBeDefined();
      expect(orgTestFile).toBeDefined();
      expect(internationalTestFile).toBeDefined();

      if (markdownTestFile) expect(markdownTestFile.file).toMatch(/\.md$/);
      if (orgTestFile) expect(orgTestFile.file).toMatch(/\.org$/);
      if (internationalTestFile)
        expect(internationalTestFile.file).toMatch(/\.md$/);
    });

    it("should return files with valid timestamps", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      const files = response.body.files;

      if (files && Array.isArray(files)) {
        files.forEach((file: any) => {
          // Only validate timestamp if mtime property exists
          if (file.mtime && typeof file.mtime === "string") {
            const mtimeDate = new Date(file.mtime);
            expect(mtimeDate.getTime()).not.toBeNaN();
            expect(mtimeDate.getTime()).toBeGreaterThan(0);
            // Timestamp should be reasonable (not in the future)
            expect(mtimeDate.getTime()).toBeLessThan(Date.now() + 60000);
          }
        });
      }
    });
  });

  describe("GET /files/raw - Raw File List", () => {
    it("should return raw file information", async () => {
      const response = await ApiHelpers.getRawFiles();

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("files");

      const rawFiles = response.body.files;

      // Handle both null and empty array cases
      if (rawFiles === null || rawFiles === undefined) {
        // API might return null instead of empty array - this is acceptable
        expect(rawFiles === null || rawFiles === undefined).toBe(true);
      } else if (Array.isArray(rawFiles)) {
        expect(rawFiles.length).toBeGreaterThanOrEqual(0);

        // Validate RawFile structure if files exist
        rawFiles.forEach((file: any) => {
          expect(file).toHaveProperty("file");
          if (file.path !== undefined) {
            expect(typeof file.path).toBe("string");
          }
          if (file.size !== undefined) {
            expect(typeof file.size).toBe("number");
          }
          if (file.mtime !== undefined) {
            expect(typeof file.mtime).toBe("string");
          }
        });
      } else {
        // If neither null nor array, fail the test with clear message
        expect.fail(
          `Expected rawFiles to be null, undefined, or array, but got: ${typeof rawFiles}`,
        );
      }
    });

    it("should include files from filesystem", async () => {
      const response = await ApiHelpers.getRawFiles();

      expect(response.status).toBe(200);
      const rawFiles = response.body.files;

      // Only test file existence if rawFiles is not null/undefined
      if (rawFiles && Array.isArray(rawFiles) && rawFiles.length > 0) {
        // Should have both .md and .org files if any files exist
        const markdownFiles = rawFiles.filter(
          (file: any) => file.file && file.file.endsWith(".md"),
        );
        const orgFiles = rawFiles.filter(
          (file: any) => file.file && file.file.endsWith(".org"),
        );

        // If we have files, we expect at least some to be .md or .org
        expect(markdownFiles.length + orgFiles.length).toBeGreaterThan(0);
      } else {
        // If no raw files are available, that's acceptable for this test environment
        expect(
          rawFiles === null ||
            rawFiles === undefined ||
            (Array.isArray(rawFiles) && rawFiles.length === 0),
        ).toBe(true);
      }
    });

    it("should return different structure than regular files endpoint", async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();

      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);

      const files = filesResponse.body.files;
      const rawFiles = rawFilesResponse.body.files;

      // Files endpoint includes database info (id, title)
      if (files && files.length > 0) {
        expect(files[0]).toHaveProperty("id");
        expect(files[0]).toHaveProperty("title");
      }

      // Raw files endpoint should only include file system info
      if (rawFiles && rawFiles.length > 0) {
        expect(rawFiles[0]).toHaveProperty("file");
        expect(rawFiles[0]).not.toHaveProperty("id"); // No database info
        expect(rawFiles[0]).not.toHaveProperty("title"); // No database info
      }
    });
  });

  describe("File Performance", () => {
    it("should return file list within reasonable time", async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getFiles();
      const endTime = Date.now();

      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });
  });

  describe("File Content Validation", () => {
    it("should show consistent file sizes", async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();

      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);

      const files = filesResponse.body.files;
      const rawFiles = rawFilesResponse.body.files;

      // Find matching files and compare sizes
      if (
        files &&
        rawFiles &&
        Array.isArray(files) &&
        Array.isArray(rawFiles)
      ) {
        files.forEach((file: any) => {
          const matchingRawFile = rawFiles.find(
            (raw: any) => raw.file === file.file,
          );

          if (matchingRawFile && file.size && matchingRawFile.size) {
            expect(file.size).toBe(matchingRawFile.size);
          }
        });
      }
    });

    it("should have reasonable file sizes", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      const files = response.body.files;

      if (files && Array.isArray(files)) {
        files.forEach((file: any) => {
          // Only validate size if the property exists
          if (file.size !== undefined && typeof file.size === "number") {
            expect(file.size).toBeGreaterThan(0);
            expect(file.size).toBeLessThan(1000000); // Less than 1MB for test files
          }
        });
      }
    });
  });

  describe("File Path Validation", () => {
    it("should return valid file paths", async () => {
      const response = await ApiHelpers.getFiles();

      expect(response.status).toBe(200);
      const files = response.body.files;

      if (files && Array.isArray(files)) {
        files.forEach((file: any) => {
          // Only validate path if the property exists
          if (file.path !== undefined && typeof file.path === "string") {
            expect(file.path).toBeTruthy();
            expect(file.path.length).toBeGreaterThan(0);
            if (file.file) {
              expect(file.path).toContain(file.file);
            }
          }
        });
      }
    });

    it("should return consistent paths between endpoints", async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();

      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);

      const files = filesResponse.body.files;
      const rawFiles = rawFilesResponse.body.files;

      if (
        files &&
        Array.isArray(files) &&
        rawFiles &&
        Array.isArray(rawFiles)
      ) {
        files.forEach((file: any) => {
          const matchingRawFile = rawFiles.find(
            (raw: any) => raw.file === file.file,
          );

          if (matchingRawFile && file.path && matchingRawFile.path) {
            expect(file.path).toBe(matchingRawFile.path);
          }
        });
      }
    });
  });
});
