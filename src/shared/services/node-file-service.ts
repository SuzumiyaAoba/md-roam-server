/**
 * Node File Service - Handles direct file operations for node creation/update/deletion
 * This service creates and manages org-roam files without relying on Emacs server
 */

import { existsSync, mkdirSync, writeFileSync, readFileSync, unlinkSync } from "node:fs";
import { join } from "node:path";
import { z } from "zod";
import { CreateNodeRequestSchema, UpdateNodeRequestSchema } from "@/shared/lib/schemas";

// Custom error types for proper HTTP status code handling
export class ValidationError extends Error {
  constructor(message: string, public errors?: string[]) {
    super(message);
    this.name = 'ValidationError';
  }
}

export class NotFoundError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'NotFoundError';
  }
}

// Types
export interface CreateNodeData {
  title: string;
  content?: string;
  tags?: string[];
  aliases?: string[];
  refs?: string[];
  category?: string;
  file_type?: "md" | "org";
}

export interface NodeInfo {
  id: string;
  title: string;
  file: string;
  file_type: "md" | "org";
  path: string;
}

export class NodeFileService {
  private orgRoamDir: string;

  constructor(orgRoamDir: string) {
    this.orgRoamDir = orgRoamDir;
    this.ensureDirectoryExists();
  }

  private ensureDirectoryExists(): void {
    if (!existsSync(this.orgRoamDir)) {
      mkdirSync(this.orgRoamDir, { recursive: true });
    }
  }

  private sanitizeNodeData(data: CreateNodeData): CreateNodeData {
    const sanitize = (str: string): string => {
      // Basic XSS prevention: remove script tags and dangerous HTML
      return str
        .replace(/<script[^>]*>[\s\S]*?<\/script>/gi, '')
        .replace(/<[^>]*script[^>]*>/gi, '')
        .replace(/javascript:/gi, '')
        .replace(/on\w+\s*=/gi, '');
    };

    const sanitizeArray = (arr?: string[]): string[] | undefined => {
      return arr?.map(item => sanitize(item));
    };

    return {
      ...data,
      title: sanitize(data.title),
      content: data.content ? sanitize(data.content) : data.content,
      tags: sanitizeArray(data.tags),
      aliases: sanitizeArray(data.aliases),
      refs: sanitizeArray(data.refs),
      category: data.category ? sanitize(data.category) : data.category
    };
  }

  private sanitizePartialNodeData(data: Partial<CreateNodeData>): Partial<CreateNodeData> {
    const sanitize = (str: string): string => {
      // Basic XSS prevention: remove script tags and dangerous HTML
      return str
        .replace(/<script[^>]*>[\s\S]*?<\/script>/gi, '')
        .replace(/<[^>]*script[^>]*>/gi, '')
        .replace(/javascript:/gi, '')
        .replace(/on\w+\s*=/gi, '');
    };

    const sanitizeArray = (arr?: string[]): string[] | undefined => {
      return arr?.map(item => sanitize(item));
    };

    const result: Partial<CreateNodeData> = { ...data };
    
    if (data.title !== undefined) {
      result.title = sanitize(data.title);
    }
    if (data.content !== undefined) {
      result.content = data.content ? sanitize(data.content) : data.content;
    }
    if (data.tags !== undefined) {
      result.tags = sanitizeArray(data.tags);
    }
    if (data.aliases !== undefined) {
      result.aliases = sanitizeArray(data.aliases);
    }
    if (data.refs !== undefined) {
      result.refs = sanitizeArray(data.refs);
    }
    if (data.category !== undefined) {
      result.category = data.category ? sanitize(data.category) : data.category;
    }

    return result;
  }

  private generateId(): string {
    // Generate UUID v4 with crypto for better uniqueness
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID().toUpperCase();
    }
    
    // Fallback UUID v4 generation with high-precision timestamp for uniqueness
    const timestamp = Date.now().toString(16);
    const random = Math.random().toString(16).substr(2, 8);
    const uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c, i) {
      // Use timestamp for first few characters to ensure uniqueness
      if (i < 8) {
        return timestamp[i % timestamp.length] || '0';
      }
      const r = Math.random() * 16 | 0;
      const v = c == 'x' ? r : (r & 0x3 | 0x8);
      return v.toString(16);
    });
    return uuid.toUpperCase();
  }

  private generateFileName(id: string, fileType: "md" | "org"): string {
    const timestamp = new Date().toISOString().slice(0, 19).replace(/[-:T]/g, '').slice(0, 14);
    return `${timestamp}-${id}.${fileType}`;
  }

  private createMarkdownContent(data: CreateNodeData, id: string): string {
    const frontMatter = [`---`, `title: "${data.title}"`];
    
    if (data.tags && data.tags.length > 0) {
      frontMatter.push(`tags:`);
      data.tags.forEach(tag => {
        frontMatter.push(`  - "${tag}"`);
      });
    }
    
    if (data.aliases && data.aliases.length > 0) {
      frontMatter.push(`roam_aliases:`);
      data.aliases.forEach(alias => {
        frontMatter.push(`  - "${alias}"`);
      });
    }
    
    if (data.refs && data.refs.length > 0) {
      frontMatter.push(`roam_refs:`);
      data.refs.forEach(ref => {
        frontMatter.push(`  - "${ref}"`);
      });
    }
    
    if (data.category) {
      frontMatter.push(`category: "${data.category}"`);
    }
    
    frontMatter.push(`id: "${id}"`);
    frontMatter.push(`---`);
    frontMatter.push(``);
    
    if (data.content) {
      frontMatter.push(data.content);
    }
    
    return frontMatter.join('\n');
  }

  private createOrgContent(data: CreateNodeData, id: string): string {
    const lines = [
      `:PROPERTIES:`,
      `:ID: ${id}`,
      `:END:`,
      `#+title: ${data.title}`
    ];
    
    if (data.tags && data.tags.length > 0) {
      lines.push(`#+filetags: ${data.tags.join(' ')}`);
    }
    
    if (data.category) {
      lines.push(`#+category: ${data.category}`);
    }
    
    if (data.aliases && data.aliases.length > 0) {
      data.aliases.forEach(alias => {
        lines.push(`#+roam_alias: ${alias}`);
      });
    }
    
    if (data.refs && data.refs.length > 0) {
      data.refs.forEach(ref => {
        lines.push(`#+roam_refs: ${ref}`);
      });
    }
    
    lines.push('');
    
    if (data.content) {
      lines.push(data.content);
    }
    
    return lines.join('\n');
  }

  createNode(data: CreateNodeData): NodeInfo {
    // Validate input data
    const validationResult = CreateNodeRequestSchema.safeParse(data);
    if (!validationResult.success) {
      const errors = validationResult.error.issues.map((err: any) => err.message);
      throw new ValidationError("Validation failed", errors);
    }
    
    // Sanitize input to prevent XSS
    const sanitizedData = this.sanitizeNodeData(validationResult.data);
    
    const id = this.generateId();
    const fileType = sanitizedData.file_type || "md";
    const fileName = this.generateFileName(id, fileType);
    const filePath = join(this.orgRoamDir, fileName);
    
    try {
      let content: string;
      if (fileType === "md") {
        content = this.createMarkdownContent(sanitizedData, id);
      } else {
        content = this.createOrgContent(sanitizedData, id);
      }
      
      writeFileSync(filePath, content, 'utf8');
      
      return {
        id,
        title: sanitizedData.title,
        file: fileName,
        file_type: fileType,
        path: filePath
      };
    } catch (error) {
      throw new Error(`Failed to create node file: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  updateNode(id: string, data: Partial<CreateNodeData>): NodeInfo | null {
    // Validate node ID
    if (!id || typeof id !== 'string' || id.trim() === '') {
      throw new ValidationError("Node ID is required");
    }
    
    // Validate update data
    const validationResult = UpdateNodeRequestSchema.safeParse(data);
    if (!validationResult.success) {
      const errors = validationResult.error.issues.map((err: any) => err.message);
      throw new ValidationError("Validation failed", errors);
    }
    
    // Find existing file by ID
    const existingFile = this.findFileById(id);
    if (!existingFile) {
      throw new NotFoundError(`Node with ID '${id}' not found`);
    }
    
    try {
      // Sanitize update data to prevent XSS
      const sanitizedData = this.sanitizePartialNodeData(validationResult.data);
      
      // Read existing content
      const content = readFileSync(existingFile.path, 'utf8');
      const updatedData = this.mergeNodeData(content, sanitizedData, existingFile.file_type);
      
      // Create updated content
      let newContent: string;
      if (existingFile.file_type === "md") {
        newContent = this.createMarkdownContent(updatedData, id);
      } else {
        newContent = this.createOrgContent(updatedData, id);
      }
      
      writeFileSync(existingFile.path, newContent, 'utf8');
      
      return {
        ...existingFile,
        title: updatedData.title
      };
    } catch (error) {
      if (error instanceof ValidationError || error instanceof NotFoundError) {
        throw error;
      }
      throw new Error(`Failed to update node: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  deleteNode(id: string): boolean {
    // Validate node ID
    if (!id || typeof id !== 'string' || id.trim() === '') {
      throw new ValidationError("Node ID is required");
    }
    
    const existingFile = this.findFileById(id);
    if (!existingFile) {
      throw new NotFoundError(`Node with ID '${id}' not found`);
    }
    
    try {
      unlinkSync(existingFile.path);
      return true;
    } catch (error) {
      throw new Error(`Failed to delete node: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  private findFileById(id: string): NodeInfo | null {
    const fs = require('fs');
    const files = fs.readdirSync(this.orgRoamDir);
    
    for (const file of files) {
      if (!file.endsWith('.md') && !file.endsWith('.org')) continue;
      
      const filePath = join(this.orgRoamDir, file);
      const content = readFileSync(filePath, 'utf8');
      
      // Check if file contains this ID
      const hasId = content.includes(id);
      if (hasId) {
        const fileType = file.endsWith('.md') ? 'md' : 'org';
        const title = this.extractTitle(content, fileType);
        
        return {
          id,
          title,
          file,
          file_type: fileType as "md" | "org",
          path: filePath
        };
      }
    }
    
    return null;
  }

  private extractTitle(content: string, fileType: "md" | "org"): string {
    if (fileType === "md") {
      const match = content.match(/^title:\s*"?([^"]+)"?$/m);
      return match ? match[1] : "Untitled";
    } else {
      const match = content.match(/^#\+title:\s*(.+)$/m);
      return match ? match[1] : "Untitled";
    }
  }

  private mergeNodeData(content: string, updates: Partial<CreateNodeData>, fileType: "md" | "org"): CreateNodeData {
    // Parse existing data from content
    const existing = this.parseNodeData(content, fileType);
    
    // Merge with updates
    return {
      title: updates.title || existing.title,
      content: updates.content !== undefined ? updates.content : existing.content,
      tags: updates.tags || existing.tags,
      aliases: updates.aliases || existing.aliases,
      refs: updates.refs || existing.refs,
      category: updates.category || existing.category,
      file_type: fileType
    };
  }

  private parseNodeData(content: string, fileType: "md" | "org"): CreateNodeData {
    if (fileType === "md") {
      return this.parseMarkdownData(content);
    } else {
      return this.parseOrgData(content);
    }
  }

  private parseMarkdownData(content: string): CreateNodeData {
    const lines = content.split('\n');
    const data: CreateNodeData = { title: "Untitled", file_type: "md" };
    
    let inFrontMatter = false;
    let contentStart = 0;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      if (line === '---') {
        if (!inFrontMatter) {
          inFrontMatter = true;
          continue;
        } else {
          contentStart = i + 1;
          break;
        }
      }
      
      if (inFrontMatter) {
        if (line.startsWith('title:')) {
          data.title = line.replace('title:', '').trim().replace(/^"(.*)"$/, '$1');
        } else if (line.startsWith('category:')) {
          data.category = line.replace('category:', '').trim().replace(/^"(.*)"$/, '$1');
        }
        // Note: tags, aliases, refs are arrays - would need more complex parsing
      }
    }
    
    if (contentStart < lines.length) {
      data.content = lines.slice(contentStart).join('\n').trim();
    }
    
    return data;
  }

  private parseOrgData(content: string): CreateNodeData {
    const lines = content.split('\n');
    const data: CreateNodeData = { title: "Untitled", file_type: "org" };
    
    let contentStart = 0;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      if (line.startsWith('#+title:')) {
        data.title = line.replace('#+title:', '').trim();
      } else if (line.startsWith('#+category:')) {
        data.category = line.replace('#+category:', '').trim();
      } else if (line.startsWith('#+filetags:')) {
        data.tags = line.replace('#+filetags:', '').trim().split(/\s+/);
      } else if (line === '' && i > 0 && !line.startsWith('#') && !line.startsWith(':')) {
        contentStart = i + 1;
        break;
      }
    }
    
    if (contentStart < lines.length) {
      data.content = lines.slice(contentStart).join('\n').trim();
    }
    
    return data;
  }

  getAllNodes(): NodeInfo[] {
    const fs = require('fs');
    const files = fs.readdirSync(this.orgRoamDir);
    const nodes: NodeInfo[] = [];
    
    for (const file of files) {
      if (!file.endsWith('.md') && !file.endsWith('.org')) continue;
      
      try {
        const filePath = join(this.orgRoamDir, file);
        const content = readFileSync(filePath, 'utf8');
        const fileType = file.endsWith('.md') ? 'md' : 'org';
        
        // Extract ID and title
        let id: string | null = null;
        if (fileType === 'md') {
          const idMatch = content.match(/^id:\s*"?([^"]+)"?$/m);
          id = idMatch ? idMatch[1] : null;
        } else {
          const idMatch = content.match(/^:ID:\s*(.+)$/m);
          id = idMatch ? idMatch[1] : null;
        }
        
        if (id) {
          const title = this.extractTitle(content, fileType);
          nodes.push({
            id,
            title,
            file,
            file_type: fileType as "md" | "org",
            path: filePath
          });
        }
      } catch (error) {
        console.error(`Error reading file ${file}:`, error);
      }
    }
    
    return nodes;
  }

  getNode(id: string): NodeInfo | null {
    return this.findFileById(id);
  }

  // Search functionality
  searchNodes(query: string): NodeInfo[] {
    const allNodes = this.getAllNodes();
    const lowercaseQuery = query.toLowerCase();
    
    return allNodes.filter(node => {
      // Search in title
      if (node.title.toLowerCase().includes(lowercaseQuery)) {
        return true;
      }
      
      // Search in file content
      try {
        const content = readFileSync(node.path, 'utf8');
        return content.toLowerCase().includes(lowercaseQuery);
      } catch {
        return false;
      }
    });
  }

  // Get all tags from all nodes
  getAllTags(): { tag: string; count: number; node_ids: string[] }[] {
    const allNodes = this.getAllNodes();
    const tagMap = new Map<string, { count: number; node_ids: string[] }>();
    
    for (const node of allNodes) {
      try {
        const content = readFileSync(node.path, 'utf8');
        const tags = this.extractTagsFromContent(content, node.file_type);
        
        for (const tag of tags) {
          if (!tagMap.has(tag)) {
            tagMap.set(tag, { count: 0, node_ids: [] });
          }
          const tagInfo = tagMap.get(tag)!;
          tagInfo.count += 1;
          tagInfo.node_ids.push(node.id);
        }
      } catch {
        // Skip files that can't be read
      }
    }
    
    return Array.from(tagMap.entries()).map(([tag, info]) => ({
      tag,
      count: info.count,
      node_ids: info.node_ids
    }));
  }

  // Get nodes by tag
  getNodesByTag(tag: string): NodeInfo[] {
    const allNodes = this.getAllNodes();
    
    return allNodes.filter(node => {
      try {
        const content = readFileSync(node.path, 'utf8');
        const tags = this.extractTagsFromContent(content, node.file_type);
        return tags.includes(tag);
      } catch {
        return false;
      }
    });
  }

  // Get statistics
  getStats(): {
    total_nodes: number;
    total_tags: number;
    total_files: number;
    file_types: { md: number; org: number };
  } {
    const allNodes = this.getAllNodes();
    const allTags = this.getAllTags();
    
    const fileTypeCounts = allNodes.reduce(
      (acc, node) => {
        acc[node.file_type] += 1;
        return acc;
      },
      { md: 0, org: 0 }
    );
    
    return {
      total_nodes: allNodes.length,
      total_tags: allTags.length,
      total_files: allNodes.length,
      file_types: fileTypeCounts
    };
  }

  private extractTagsFromContent(content: string, fileType: "md" | "org"): string[] {
    const tags: string[] = [];
    
    if (fileType === "md") {
      // Extract from YAML front matter
      const yamlMatch = content.match(/^---\n([\s\S]*?)\n---/);
      if (yamlMatch) {
        const yamlContent = yamlMatch[1];
        const tagsMatch = yamlContent.match(/^tags:\s*\n((?:\s*-\s*.+\n)*)/m);
        if (tagsMatch) {
          const tagLines = tagsMatch[1].trim().split('\n');
          for (const line of tagLines) {
            const tagMatch = line.match(/^\s*-\s*"?([^"]+)"?$/);
            if (tagMatch) {
              tags.push(tagMatch[1].trim());
            }
          }
        }
      }
    } else {
      // Extract from org-mode #+filetags:
      const filetagsMatch = content.match(/^#\+filetags:\s*(.+)$/m);
      if (filetagsMatch) {
        const tagString = filetagsMatch[1].trim();
        tags.push(...tagString.split(/\s+/));
      }
    }
    
    return tags;
  }

  // Additional node operations needed for API endpoints
  getNodeContent(id: string): string | null {
    const node = this.findFileById(id);
    if (!node) {
      return null;
    }
    
    try {
      return readFileSync(node.path, 'utf8');
    } catch {
      return null;
    }
  }

  getNodeLinks(id: string): string[] {
    const content = this.getNodeContent(id);
    if (!content) {
      return [];
    }
    
    // Extract basic links - simplified implementation for now
    const links: string[] = [];
    const linkRegex = /\[\[([^\]]+)\]\]/g;
    let match;
    
    while ((match = linkRegex.exec(content)) !== null) {
      links.push(match[1]);
    }
    
    return links;
  }

  getNodeBacklinks(id: string): NodeInfo[] {
    // For now, return empty array
    // TODO: Implement proper backlink search by scanning all files for references to this ID
    return [];
  }

  getNodeAliases(id: string): string[] {
    const node = this.findFileById(id);
    if (!node) {
      return [];
    }
    
    try {
      const content = readFileSync(node.path, 'utf8');
      const parsed = this.parseNodeData(content, node.file_type);
      return parsed.aliases || [];
    } catch {
      return [];
    }
  }

  getNodeRefs(id: string): string[] {
    const node = this.findFileById(id);
    if (!node) {
      return [];
    }
    
    try {
      const content = readFileSync(node.path, 'utf8');
      const parsed = this.parseNodeData(content, node.file_type);
      return parsed.refs || [];
    } catch {
      return [];
    }
  }

  parseNode(id: string): { metadata: any; content: string } | null {
    const node = this.findFileById(id);
    if (!node) {
      return null;
    }
    
    try {
      const fullContent = readFileSync(node.path, 'utf8');
      const parsed = this.parseNodeData(fullContent, node.file_type);
      
      return {
        metadata: {
          id: id,
          title: parsed.title,
          tags: parsed.tags || [],
          aliases: parsed.aliases || [],
          refs: parsed.refs || [],
          category: parsed.category,
          file_type: node.file_type,
          file: node.file,
          path: node.path
        },
        content: parsed.content || ""
      };
    } catch {
      return null;
    }
  }
}