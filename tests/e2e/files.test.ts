import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';
import { NodeData, FileInfo, RawFile } from '@/utils/types';

describe('Files API E2E Tests', () => {
  let testNodes: NodeData[];

  beforeEach(async () => {
    // Create test files with different types
    testNodes = await Promise.all([
      TestCleanup.createTestNode({
        title: 'Markdown File Test',
        content: '# Heading\n\nThis is markdown content with **bold** text.',
        tags: ['file-test', 'markdown'],
        file_type: 'md'
      }),
      TestCleanup.createTestNode({
        title: 'Org File Test',
        content: '* Heading\n\nThis is org content with *emphasis* text.',
        tags: ['file-test', 'org'],
        file_type: 'org'
      }),
      TestCleanup.createTestNode({
        title: 'Japanese File Test',
        content: '# 日本語ヘッダー\n\n日本語コンテンツのテストです。',
        tags: ['file-test', '日本語'],
        file_type: 'md'
      })
    ]);
  });

  describe('GET /files - File List', () => {
    it('should return list of all files', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('data');
      
      const files = response.body.data;
      expect(Array.isArray(files)).toBe(true);
      expect(files.length).toBeGreaterThanOrEqual(3); // At least our test files
      
      // Validate FileInfo structure
      files.forEach((file: FileInfo) => {
        expect(file).toHaveProperty('id');
        expect(file).toHaveProperty('file');
        expect(file).toHaveProperty('title');
        expect(file).toHaveProperty('mtime');
        expect(file).toHaveProperty('size');
        expect(file).toHaveProperty('path');
        
        expect(typeof file.id).toBe('string');
        expect(typeof file.file).toBe('string');
        expect(typeof file.title).toBe('string');
        expect(typeof file.mtime).toBe('string');
        expect(typeof file.size).toBe('number');
        expect(typeof file.path).toBe('string');
      });
    });

    it('should include both markdown and org files', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      const files = response.body.data;
      
      const markdownFiles = files.filter((file: FileInfo) => file.file.endsWith('.md'));
      const orgFiles = files.filter((file: FileInfo) => file.file.endsWith('.org'));
      
      expect(markdownFiles.length).toBeGreaterThan(0);
      expect(orgFiles.length).toBeGreaterThan(0);
    });

    it('should include our test files', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      const files = response.body.data;
      
      // Find our test files by title
      const markdownTestFile = files.find((file: FileInfo) => 
        file.title === 'Markdown File Test'
      );
      const orgTestFile = files.find((file: FileInfo) => 
        file.title === 'Org File Test'
      );
      const japaneseTestFile = files.find((file: FileInfo) => 
        file.title === 'Japanese File Test'
      );
      
      expect(markdownTestFile).toBeDefined();
      expect(orgTestFile).toBeDefined();
      expect(japaneseTestFile).toBeDefined();
      
      expect(markdownTestFile.file).toMatch(/\.md$/);
      expect(orgTestFile.file).toMatch(/\.org$/);
      expect(japaneseTestFile.file).toMatch(/\.md$/);
    });

    it('should return files with valid timestamps', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      const files = response.body.data;
      
      files.forEach((file: FileInfo) => {
        const mtimeDate = new Date(file.mtime);
        expect(mtimeDate.getTime()).not.toBeNaN();
        expect(mtimeDate.getTime()).toBeGreaterThan(0);
      });
    });
  });

  describe('GET /files/raw - Raw File List', () => {
    it('should return raw file information', async () => {
      const response = await ApiHelpers.getRawFiles();
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('status', 'success');
      expect(response.body).toHaveProperty('data');
      
      const rawFiles = response.body.data;
      expect(Array.isArray(rawFiles)).toBe(true);
      expect(rawFiles.length).toBeGreaterThanOrEqual(3);
      
      // Validate RawFile structure
      rawFiles.forEach((file: RawFile) => {
        expect(file).toHaveProperty('file');
        expect(file).toHaveProperty('path');
        expect(file).toHaveProperty('size');
        expect(file).toHaveProperty('mtime');
        
        expect(typeof file.file).toBe('string');
        expect(typeof file.path).toBe('string');
        expect(typeof file.size).toBe('number');
        expect(typeof file.mtime).toBe('string');
      });
    });

    it('should include files from filesystem', async () => {
      const response = await ApiHelpers.getRawFiles();
      
      expect(response.status).toBe(200);
      const rawFiles = response.body.data;
      
      // Should have both .md and .org files
      const markdownFiles = rawFiles.filter((file: RawFile) => file.file.endsWith('.md'));
      const orgFiles = rawFiles.filter((file: RawFile) => file.file.endsWith('.org'));
      
      expect(markdownFiles.length).toBeGreaterThan(0);
      expect(orgFiles.length).toBeGreaterThan(0);
    });

    it('should return different structure than regular files endpoint', async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();
      
      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);
      
      const files = filesResponse.body.data;
      const rawFiles = rawFilesResponse.body.data;
      
      // Files endpoint includes database info (id, title)
      expect(files[0]).toHaveProperty('id');
      expect(files[0]).toHaveProperty('title');
      
      // Raw files endpoint is filesystem-only
      expect(rawFiles[0]).not.toHaveProperty('id');
      expect(rawFiles[0]).not.toHaveProperty('title');
      expect(rawFiles[0]).toHaveProperty('file');
      expect(rawFiles[0]).toHaveProperty('path');
    });
  });

  describe('File Performance', () => {
    it('should return file list within reasonable time', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getFiles();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });

    it('should return raw file list within reasonable time', async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getRawFiles();
      const endTime = Date.now();
      
      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });
  });

  describe('File Content Validation', () => {
    it('should show consistent file sizes', async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();
      
      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);
      
      const files = filesResponse.body.data;
      const rawFiles = rawFilesResponse.body.data;
      
      // Find matching files and compare sizes
      files.forEach((file: FileInfo) => {
        const matchingRawFile = rawFiles.find((raw: RawFile) => 
          raw.file === file.file
        );
        
        if (matchingRawFile) {
          expect(file.size).toBe(matchingRawFile.size);
        }
      });
    });

    it('should have reasonable file sizes', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      const files = response.body.data;
      
      files.forEach((file: FileInfo) => {
        expect(file.size).toBeGreaterThan(0);
        expect(file.size).toBeLessThan(1000000); // Less than 1MB for test files
      });
    });
  });

  describe('File Path Validation', () => {
    it('should return valid file paths', async () => {
      const response = await ApiHelpers.getFiles();
      
      expect(response.status).toBe(200);
      const files = response.body.data;
      
      files.forEach((file: FileInfo) => {
        expect(file.path).toBeTruthy();
        expect(file.path).toContain(file.file);
        expect(file.path.length).toBeGreaterThan(0);
      });
    });

    it('should return consistent paths between endpoints', async () => {
      const filesResponse = await ApiHelpers.getFiles();
      const rawFilesResponse = await ApiHelpers.getRawFiles();
      
      expect(filesResponse.status).toBe(200);
      expect(rawFilesResponse.status).toBe(200);
      
      const files = filesResponse.body.data;
      const rawFiles = rawFilesResponse.body.data;
      
      files.forEach((file: FileInfo) => {
        const matchingRawFile = rawFiles.find((raw: RawFile) => 
          raw.file === file.file
        );
        
        if (matchingRawFile) {
          expect(file.path).toBe(matchingRawFile.path);
        }
      });
    });
  });
});