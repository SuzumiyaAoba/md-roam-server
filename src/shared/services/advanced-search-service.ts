import { readdir, readFile } from "node:fs/promises";
import path from "node:path";
import type {
  FieldSearchRequest,
  FieldSearchResult,
  FuzzySearchRequest,
  FuzzySearchResult,
  HighlightSearchRequest,
  HighlightSearchResult,
  NodeContent,
  NodeMetadata,
  PhraseSearchRequest,
  PhraseSearchResult,
  Suggestion,
  SuggestionsRequest,
} from "../lib/schemas/index.js";

export class AdvancedSearchService {
  constructor(private readonly orgRoamDir: string) {}

  // Levenshtein distance calculation for fuzzy search
  private calculateLevenshteinDistance(str1: string, str2: string): number {
    const matrix: number[][] = [];
    const len1 = str1.length;
    const len2 = str2.length;

    // Initialize matrix
    for (let i = 0; i <= len1; i++) {
      matrix[i] = [i];
    }
    for (let j = 0; j <= len2; j++) {
      matrix[0]![j] = j;
    }

    // Fill matrix
    for (let i = 1; i <= len1; i++) {
      for (let j = 1; j <= len2; j++) {
        const cost = str1[i - 1] === str2[j - 1] ? 0 : 1;
        matrix[i]![j] = Math.min(
          matrix[i - 1]![j]! + 1, // deletion
          matrix[i]![j - 1]! + 1, // insertion
          matrix[i - 1]![j - 1]! + cost, // substitution
        );
      }
    }

    return matrix[len1]![len2]!;
  }

  // Calculate similarity score (0-1, where 1 is exact match)
  private calculateSimilarity(str1: string, str2: string): number {
    const maxLength = Math.max(str1.length, str2.length);
    if (maxLength === 0) return 1;

    const distance = this.calculateLevenshteinDistance(
      str1.toLowerCase(),
      str2.toLowerCase(),
    );
    return (maxLength - distance) / maxLength;
  }

  // Japanese text normalization for better matching
  private normalizeJapaneseText(text: string): string {
    // Convert full-width to half-width characters
    const halfWidth = text.replace(/[\uff01-\uff5e]/g, (char) =>
      String.fromCharCode(char.charCodeAt(0) - 0xfee0),
    );

    // Convert hiragana to katakana for broader matching
    const katakana = halfWidth.replace(/[\u3041-\u3096]/g, (char) =>
      String.fromCharCode(char.charCodeAt(0) + 0x60),
    );

    return katakana.toLowerCase().trim();
  }

  // Get all nodes with their content
  private async getAllNodes(): Promise<NodeContent[]> {
    const files = await readdir(this.orgRoamDir);
    const nodes: NodeContent[] = [];

    for (const file of files) {
      if (!file.endsWith(".md") && !file.endsWith(".org")) continue;

      const filePath = path.join(this.orgRoamDir, file);
      try {
        const content = await readFile(filePath, "utf-8");
        const node = this.parseNodeContent(file, filePath, content);
        if (node) nodes.push(node);
      } catch (_error) {}
    }

    return nodes;
  }

  // Parse node content from file
  private parseNodeContent(
    file: string,
    filePath: string,
    content: string,
  ): NodeContent | null {
    const isMarkdown = file.endsWith(".md");

    if (isMarkdown) {
      return this.parseMarkdownNode(file, filePath, content);
    } else {
      return this.parseOrgNode(file, filePath, content);
    }
  }

  private parseMarkdownNode(
    file: string,
    _filePath: string,
    content: string,
  ): NodeContent | null {
    const frontMatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
    if (!frontMatterMatch) return null;

    const frontMatter = frontMatterMatch[1];
    const bodyContent = frontMatterMatch[2];

    if (!frontMatter) return null;

    // Parse YAML front matter manually
    const lines = frontMatter.split("\n");
    const metadata: NodeMetadata = { tags: [], aliases: [], refs: [] };
    let id = "";
    let title = "";

    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed.startsWith("id:")) {
        id = trimmed.substring(3).trim().replace(/['"]/g, "");
      } else if (trimmed.startsWith("title:")) {
        title = trimmed.substring(6).trim().replace(/['"]/g, "");
      } else if (trimmed.startsWith("tags:")) {
        // Handle array format
        const tagsMatch = line.match(/tags:\s*\[(.*?)\]/);
        if (tagsMatch?.[1]) {
          metadata.tags = tagsMatch[1]
            .split(",")
            .map((t) => t.trim().replace(/['"]/g, ""));
        }
      } else if (trimmed.startsWith("roam_aliases:")) {
        const aliasesMatch = line.match(/roam_aliases:\s*\[(.*?)\]/);
        if (aliasesMatch?.[1]) {
          metadata.aliases = aliasesMatch[1]
            .split(",")
            .map((a) => a.trim().replace(/['"]/g, ""));
        }
      } else if (trimmed.startsWith("category:")) {
        metadata.category = trimmed.substring(9).trim().replace(/['"]/g, "");
      }
    }

    if (!id || !title) return null;

    return {
      id,
      title,
      file,
      content: bodyContent || "",
      metadata,
    };
  }

  private parseOrgNode(
    file: string,
    _filePath: string,
    content: string,
  ): NodeContent | null {
    const lines = content.split("\n");
    let id = "";
    let title = "";
    const metadata: NodeMetadata = { tags: [], aliases: [], refs: [] };
    let bodyStartIndex = 0;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]?.trim();

      if (!line) continue;

      if (line.startsWith(":ID:")) {
        id = line.substring(4).trim();
      } else if (line.startsWith("#+title:")) {
        title = line.substring(8).trim();
      } else if (line.startsWith("#+category:")) {
        metadata.category = line.substring(11).trim();
      } else if (line.startsWith("#+filetags:")) {
        metadata.tags = line.substring(11).trim().split(/\s+/);
      } else if (line === ":END:") {
        bodyStartIndex = i + 1;
        break;
      }
    }

    if (!id || !title) return null;

    const bodyContent = lines.slice(bodyStartIndex).join("\n");

    return {
      id,
      title,
      file,
      content: bodyContent || "",
      metadata,
    };
  }

  // Fuzzy search implementation
  async fuzzySearch(request: FuzzySearchRequest): Promise<{
    results: FuzzySearchResult[];
    totalResults: number;
    searchTime: number;
  }> {
    const startTime = Date.now();
    const nodes = await this.getAllNodes();
    const results: FuzzySearchResult[] = [];

    const normalizedQuery = this.normalizeJapaneseText(request.query);

    for (const node of nodes) {
      const matches: Array<{ field: string; value: string; distance: number }> =
        [];
      let bestScore = 0;

      // Search in specified fields
      for (const field of request.fields) {
        let fieldValue = "";

        switch (field) {
          case "title":
            fieldValue = node.title;
            break;
          case "content":
            fieldValue = node.content;
            break;
          case "tags":
            fieldValue = (node.metadata.tags || []).join(" ");
            break;
        }

        if (fieldValue) {
          const normalizedValue = this.normalizeJapaneseText(fieldValue);
          const similarity = this.calculateSimilarity(
            normalizedQuery,
            normalizedValue,
          );

          if (similarity >= request.threshold) {
            const distance = this.calculateLevenshteinDistance(
              normalizedQuery,
              normalizedValue,
            );
            matches.push({ field, value: fieldValue, distance });
            bestScore = Math.max(bestScore, similarity);
          }
        }
      }

      if (matches.length > 0) {
        results.push({
          id: node.id,
          title: node.title,
          file: node.file,
          score: bestScore,
          matches,
        });
      }
    }

    // Sort by score (best matches first)
    results.sort((a, b) => b.score - a.score);

    const searchTime = Date.now() - startTime;
    const limitedResults = results.slice(0, request.maxResults);

    return {
      results: limitedResults,
      totalResults: results.length,
      searchTime,
    };
  }

  // Phrase search implementation
  async phraseSearch(request: PhraseSearchRequest): Promise<{
    results: PhraseSearchResult[];
    totalResults: number;
    searchTime: number;
  }> {
    const startTime = Date.now();
    const nodes = await this.getAllNodes();
    const results: PhraseSearchResult[] = [];

    const searchPhrase = request.caseSensitive
      ? request.phrase
      : request.phrase.toLowerCase();

    for (const node of nodes) {
      const matches: Array<{
        field: string;
        snippet: string;
        position: number;
      }> = [];

      for (const field of request.fields) {
        let fieldValue = "";

        switch (field) {
          case "title":
            fieldValue = node.title;
            break;
          case "content":
            fieldValue = node.content;
            break;
          case "tags":
            fieldValue = (node.metadata.tags || []).join(" ");
            break;
        }

        if (fieldValue) {
          const searchText = request.caseSensitive
            ? fieldValue
            : fieldValue.toLowerCase();
          const position = searchText.indexOf(searchPhrase);

          if (position !== -1) {
            // Create snippet around the match
            const snippetStart = Math.max(0, position - 50);
            const snippetEnd = Math.min(
              fieldValue.length,
              position + searchPhrase.length + 50,
            );
            const snippet = fieldValue.substring(snippetStart, snippetEnd);

            matches.push({ field, snippet, position });
          }
        }
      }

      if (matches.length > 0) {
        results.push({
          id: node.id,
          title: node.title,
          file: node.file,
          matches,
        });
      }
    }

    const searchTime = Date.now() - startTime;
    const limitedResults = results.slice(0, request.maxResults);

    return {
      results: limitedResults,
      totalResults: results.length,
      searchTime,
    };
  }

  // Field-specific search implementation
  async fieldSearch(request: FieldSearchRequest): Promise<{
    results: FieldSearchResult[];
    totalResults: number;
    searchTime: number;
  }> {
    const startTime = Date.now();
    const nodes = await this.getAllNodes();
    const results: FieldSearchResult[] = [];

    const searchQuery = request.caseSensitive
      ? request.query
      : request.query.toLowerCase();

    for (const node of nodes) {
      let fieldValue = "";

      switch (request.field) {
        case "title":
          fieldValue = node.title;
          break;
        case "content":
          fieldValue = node.content;
          break;
        case "tags":
          fieldValue = (node.metadata.tags || []).join(" ");
          break;
        case "category":
          fieldValue = node.metadata.category || "";
          break;
        case "aliases":
          fieldValue = (node.metadata.aliases || []).join(" ");
          break;
        case "refs":
          fieldValue = (node.metadata.refs || []).join(" ");
          break;
      }

      if (fieldValue) {
        const searchText = request.caseSensitive
          ? fieldValue
          : fieldValue.toLowerCase();
        let isMatch = false;
        let matchType: "exact" | "partial" | "fuzzy" = "partial";

        if (request.exact) {
          isMatch = searchText === searchQuery;
          matchType = "exact";
        } else {
          isMatch = searchText.includes(searchQuery);
          if (!isMatch) {
            // Try fuzzy matching as fallback
            const similarity = this.calculateSimilarity(
              searchQuery,
              searchText,
            );
            if (similarity >= 0.7) {
              isMatch = true;
              matchType = "fuzzy";
            }
          }
        }

        if (isMatch) {
          results.push({
            id: node.id,
            title: node.title,
            file: node.file,
            field: request.field,
            value: fieldValue,
            matchType,
          });
        }
      }
    }

    const searchTime = Date.now() - startTime;
    const limitedResults = results.slice(0, request.maxResults);

    return {
      results: limitedResults,
      totalResults: results.length,
      searchTime,
    };
  }

  // Generate search suggestions
  async generateSuggestions(request: SuggestionsRequest): Promise<{
    suggestions: Suggestion[];
    searchTime: number;
  }> {
    const startTime = Date.now();
    const nodes = await this.getAllNodes();
    const suggestions: Suggestion[] = [];
    const suggestionSet = new Set<string>();

    const queryLower = request.query.toLowerCase();

    for (const node of nodes) {
      let candidates: string[] = [];

      switch (request.field) {
        case "title":
          candidates = [node.title];
          break;
        case "content":
          // Extract words from content for suggestions
          candidates = node.content
            .split(/\s+/)
            .filter(
              (word) =>
                word.length > 2 && word.toLowerCase().includes(queryLower),
            );
          break;
        case "tags":
          candidates = node.metadata.tags || [];
          break;
        case "category":
          if (node.metadata.category) {
            candidates = [node.metadata.category];
          }
          break;
      }

      for (const candidate of candidates) {
        const candidateLower = candidate.toLowerCase();
        if (
          candidateLower.includes(queryLower) &&
          !suggestionSet.has(candidateLower)
        ) {
          suggestionSet.add(candidateLower);

          // Calculate relevance score
          const similarity = this.calculateSimilarity(
            queryLower,
            candidateLower,
          );

          suggestions.push({
            text: candidate,
            type: request.field === "tags" ? "tag" : request.field,
            nodeId: node.id,
            score: similarity,
          });
        }
      }
    }

    // Sort by relevance score
    suggestions.sort((a, b) => (b.score || 0) - (a.score || 0));

    const searchTime = Date.now() - startTime;
    const limitedSuggestions = suggestions.slice(0, request.maxSuggestions);

    return {
      suggestions: limitedSuggestions,
      searchTime,
    };
  }

  // Search with highlight information
  async highlightSearch(request: HighlightSearchRequest): Promise<{
    results: HighlightSearchResult[];
    totalResults: number;
    searchTime: number;
  }> {
    const startTime = Date.now();
    const nodes = await this.getAllNodes();
    const results: HighlightSearchResult[] = [];

    const searchQuery = request.caseSensitive
      ? request.query
      : request.query.toLowerCase();

    for (const node of nodes) {
      const highlights: Array<{
        field: string;
        text: string;
        positions: Array<{ start: number; end: number }>;
      }> = [];

      const fields = [
        { name: "title", value: node.title },
        { name: "content", value: node.content },
        { name: "tags", value: (node.metadata.tags || []).join(" ") },
      ];

      for (const field of fields) {
        if (!field.value) continue;

        const searchText = request.caseSensitive
          ? field.value
          : field.value.toLowerCase();
        const positions: Array<{ start: number; end: number }> = [];

        let index = 0;
        let nextIndex = searchText.indexOf(searchQuery, index);
        while (nextIndex !== -1) {
          positions.push({
            start: nextIndex,
            end: nextIndex + searchQuery.length,
          });
          index = nextIndex + searchQuery.length;
          nextIndex = searchText.indexOf(searchQuery, index);
        }

        if (positions.length > 0) {
          highlights.push({
            field: field.name,
            text: field.value,
            positions,
          });
        }
      }

      if (highlights.length > 0) {
        // Create snippet with highlights
        const contentHighlight = highlights.find((h) => h.field === "content");
        let snippet = "";

        if (contentHighlight && contentHighlight.positions.length > 0) {
          const firstMatch = contentHighlight.positions[0];
          if (firstMatch) {
            const start = Math.max(
              0,
              firstMatch.start - request.snippetLength / 2,
            );
            const end = Math.min(
              contentHighlight.text.length,
              firstMatch.end + request.snippetLength / 2,
            );
            snippet = contentHighlight.text.substring(start, end);
          }
        } else {
          snippet = node.content.substring(0, request.snippetLength);
        }

        results.push({
          id: node.id,
          title: node.title,
          file: node.file,
          snippet,
          highlights,
        });
      }
    }

    const searchTime = Date.now() - startTime;
    const limitedResults = results.slice(0, request.maxResults);

    return {
      results: limitedResults,
      totalResults: results.length,
      searchTime,
    };
  }
}
