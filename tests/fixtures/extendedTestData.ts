import type { CreateNodePayload, UpdateNodePayload } from "@/utils/types";

// Extended test data with comprehensive coverage
export const EXTENDED_TEST_NODES: {
  japanese: CreateNodePayload[];
  edgeCases: CreateNodePayload[];
  performance: CreateNodePayload[];
  security: CreateNodePayload[];
  formatting: CreateNodePayload[];
  metadata: CreateNodePayload[];
} = {
  // Japanese and Unicode content tests
  japanese: [
    {
      title: "日本語Markdownテスト",
      content:
        "# 日本語ヘッダー\n\nこれは日本語のMarkdownファイルです。\n\n- 箇条書き項目1\n- 箇条書き項目2\n\n**太字** と *斜体* のテスト。",
      tags: ["日本語", "マークダウン", "テスト"],
      aliases: ["Japanese MD Test", "日本語テストファイル"],
      category: "japanese",
      file_type: "md",
    },
    {
      title: "日本語Orgファイルテスト",
      content:
        "* 日本語ヘッダー\n\nこれは日本語のOrgファイルです。\n\n- リスト項目1\n- リスト項目2\n\n*強調* と /斜体/ のテスト。",
      tags: ["日本語", "org-mode", "テスト"],
      aliases: ["Japanese Org Test", "日本語Orgファイル"],
      category: "japanese",
      file_type: "org",
    },
    {
      title: "Unicode Emoji Test 📝",
      content:
        "Testing emoji support: 🚀 🎯 📊 💡\n\nMixed content: English and 日本語 with emojis 🌟",
      tags: ["emoji", "unicode", "mixed"],
      file_type: "md",
    },
    {
      title: "中文测试文档",
      content: "# 中文标题\n\n这是中文内容的测试。包含简体中文字符。",
      tags: ["中文", "测试", "chinese"],
      file_type: "md",
    },
  ],

  // Edge cases and boundary conditions
  edgeCases: [
    {
      title:
        "Very Long Title That Exceeds Normal Length Expectations And Tests System Limits For Title Processing And Display",
      content: "Short content with very long title.",
      file_type: "md",
    },
    {
      title: "Empty Content Test",
      content: "",
      file_type: "md",
    },
    {
      title: "Single Character",
      content: "A",
      file_type: "md",
    },
    {
      title: "Special Characters !@#$%^&*()_+-=[]{}|;:,.<>?",
      content:
        "Testing special characters in title and content: !@#$%^&*()_+-=[]{}|;:'\"<>?/",
      file_type: "md",
    },
    {
      title: "Whitespace\\nNewlines\\tTabs Test",
      content:
        "Content with various whitespace:\n\n- Line breaks\n\t- Tabs\n   - Spaces\n\r\n- Windows line endings",
      file_type: "md",
    },
    {
      title: "Large Content Test",
      content:
        "Large content: " +
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ".repeat(100),
      file_type: "md",
    },
    {
      title: "Org Mode Complex Structure",
      content:
        '* Level 1 Heading\n** Level 2 Heading\n*** Level 3 Heading\n\n:PROPERTIES:\n:CUSTOM_ID: test-id\n:END:\n\n#+BEGIN_SRC javascript\nconst test = "code";\n#+END_SRC\n\n#+RESULTS:\n: output',
      file_type: "org",
    },
  ],

  // Performance testing data
  performance: [
    {
      title: "Performance Test 1K Tags",
      content: "Testing with many tags",
      tags: Array.from({ length: 1000 }, (_, i) => `tag-${i}`),
      file_type: "md",
    },
    {
      title: "Performance Test Large Aliases",
      content: "Testing with many aliases",
      aliases: Array.from({ length: 500 }, (_, i) => `Alias Number ${i + 1}`),
      file_type: "md",
    },
    {
      title: "Performance Test Many Refs",
      content: "Testing with many references",
      refs: Array.from({ length: 100 }, (_, i) => `https://example-${i}.com`),
      file_type: "md",
    },
  ],

  // Security test cases
  security: [
    {
      title: 'Script Injection Test <script>alert("xss")</script>',
      content: 'Content with script tags: <script>alert("test")</script>',
      file_type: "md",
    },
    {
      title: "Path Traversal Test",
      content: "Testing path traversal: ../../../etc/passwd",
      file_type: "md",
    },
    {
      title: "SQL Injection Test",
      content: "Content with SQL: '; DROP TABLE nodes; --",
      tags: ["'; DROP TABLE tags; --"],
      file_type: "md",
    },
    {
      title: "Command Injection Test",
      content: "Content with commands: `rm -rf /`; $(rm -rf /)",
      file_type: "md",
    },
  ],

  // Complex formatting tests
  formatting: [
    {
      title: "Markdown Kitchen Sink",
      content: `# H1 Header
## H2 Header
### H3 Header

**Bold text** and *italic text* and ***bold italic***.

\`inline code\` and code blocks:

\`\`\`javascript
function test() {
  console.log("Hello, world!");
}
\`\`\`

> Blockquote
> Multiple lines

- Unordered list item 1
- Unordered list item 2
  - Nested item

1. Ordered list item 1
2. Ordered list item 2

| Table | Header |
|-------|--------|
| Cell 1| Cell 2 |

[Link](https://example.com) and ![Image](https://example.com/image.png)

---

Horizontal rule above.`,
      file_type: "md",
    },
    {
      title: "Org Mode Kitchen Sink",
      content: `* TODO Main Task [1/3]
** DONE Subtask 1
** TODO Subtask 2
** TODO Subtask 3

/Italic/, *bold*, =verbatim=, ~code~, +strikethrough+.

#+BEGIN_EXAMPLE
Example block content
#+END_EXAMPLE

#+BEGIN_QUOTE
This is a quote block
#+END_QUOTE

- List item 1
- List item 2
  - Nested item

1. Numbered item 1
2. Numbered item 2

| Column 1 | Column 2 |
|----------|----------|
| Cell A   | Cell B   |

[[https://example.com][External Link]]

#+CAPTION: Table caption
#+NAME: table-name`,
      file_type: "org",
    },
  ],

  // Metadata complexity tests
  metadata: [
    {
      title: "Complex Metadata Test",
      content: "Testing various metadata combinations",
      tags: ["meta", "test", "complex", "データ", "тест", "测试"],
      aliases: [
        "Complex Meta",
        "Metadata Test",
        "メタデータテスト",
        "Тест метаданных",
      ],
      refs: [
        "https://example.com/path/to/resource",
        "https://研究.jp/論文",
        "https://тест.рф/данные",
        "@citation:author2023",
        "doi:10.1000/test.doi",
      ],
      category: "metadata-testing",
      file_type: "md",
    },
    {
      title: "Minimal Metadata",
      content: "Only required fields",
      file_type: "org",
    },
    {
      title: "Mixed Language Metadata",
      content: "Content mixing multiple languages",
      tags: ["English", "日本語", "中文", "Español", "Français", "Deutsch"],
      aliases: ["Multi-Lang", "多言語", "多语言", "Multilingüe"],
      file_type: "md",
    },
  ],
};

// Extended update test cases
export const EXTENDED_TEST_UPDATES: {
  partial: UpdateNodePayload[];
  complete: UpdateNodePayload[];
  edgeCases: UpdateNodePayload[];
  invalid: Partial<UpdateNodePayload>[];
} = {
  // Partial updates
  partial: [
    { title: "Updated Title Only" },
    { content: "Updated content only" },
    { tags: ["new-tag-1", "new-tag-2"] },
    { aliases: ["New Alias"] },
    { refs: ["https://new-reference.com"] },
    { category: "updated-category" },
  ],

  // Complete updates
  complete: [
    {
      title: "Completely Updated Node",
      content: "All fields updated with new content",
      tags: ["updated", "complete", "new"],
      aliases: ["Updated Node", "Complete Update"],
      refs: ["https://updated-ref.com"],
      category: "fully-updated",
    },
    {
      title: "完全更新されたノード",
      content: "日本語で完全に更新された内容です。",
      tags: ["更新済み", "完全", "新規"],
      aliases: ["更新されたノード"],
      category: "japanese-update",
    },
  ],

  // Edge case updates
  edgeCases: [
    { title: "" }, // Empty title (should fail)
    { content: `Very long content: ${"Long text. ".repeat(1000)}` },
    { tags: [] }, // Empty array
    { tags: Array.from({ length: 100 }, (_, i) => `bulk-tag-${i}`) },
    { title: "Special chars: !@#$%^&*()_+-=[]{}|;:,.<>?" },
  ],

  // Invalid updates
  invalid: [
    { title: "" },
    { file_type: "invalid" as unknown },
    { tags: "not-an-array" as unknown },
    { aliases: 123 as unknown },
  ],
};

// Performance testing utilities
export const PERFORMANCE_TEST_SCENARIOS = {
  // Bulk node creation
  bulkCreation: (count: number, fileType: "md" | "org" = "md") =>
    Array.from({ length: count }, (_, i) => ({
      title: `Bulk Node ${i + 1}`,
      content: `Content for bulk node ${i + 1} created at ${new Date().toISOString()}`,
      tags: [`bulk-${Math.floor(i / 10)}`, `batch-${i % 5}`, fileType],
      category: "bulk-testing",
      file_type: fileType,
    })),

  // Concurrent operations
  concurrentUpdates: (baseTitle: string, count: number) =>
    Array.from({ length: count }, (_, i) => ({
      title: `${baseTitle} - Update ${i + 1}`,
      content: `Concurrent update ${i + 1} at ${Date.now()}`,
      tags: [`concurrent-${i}`],
    })),

  // Memory stress test
  largeContentNodes: (count: number) =>
    Array.from({ length: count }, (_, i) => ({
      title: `Large Content Node ${i + 1}`,
      content: `Large content block: ${"Lorem ipsum dolor sit amet. ".repeat(10000)}`,
      tags: [`large-${i}`],
      file_type: "md" as const,
    })),
};

// Search test queries with expected patterns
export const EXTENDED_SEARCH_QUERIES = [
  // Basic searches
  { query: "test", expectedResults: "multiple" },
  { query: "markdown", expectedResults: "multiple" },
  { query: "org", expectedResults: "multiple" },

  // Japanese searches
  { query: "日本語", expectedResults: "multiple" },
  { query: "テスト", expectedResults: "some" },
  { query: "マークダウン", expectedResults: "specific" },

  // Unicode searches
  { query: "📝", expectedResults: "specific" },
  { query: "emoji", expectedResults: "some" },
  { query: "中文", expectedResults: "specific" },

  // Special character searches
  { query: "!@#$", expectedResults: "specific" },
  { query: "script", expectedResults: "some" },

  // Complex searches
  { query: "performance test", expectedResults: "multiple" },
  { query: "kitchen sink", expectedResults: "some" },

  // Edge cases
  { query: "", expectedResults: "all_or_none" },
  { query: "nonexistent-unique-string-12345", expectedResults: "none" },
  { query: "a", expectedResults: "many" }, // Single character
  { query: "the and or", expectedResults: "many" }, // Common words
];

// Error scenarios for testing
export const ERROR_TEST_SCENARIOS = {
  invalidJson: [
    '{"title": "Test"', // Incomplete JSON
    '{"title": }', // Invalid syntax
    "not json at all",
    "",
  ],

  malformedRequests: [
    { title: null },
    { title: 123 },
    { title: ["array"] },
    { content: null },
    { tags: "string-instead-of-array" },
    { file_type: "invalid" },
  ],

  resourceLimits: [
    { title: "x".repeat(10000) }, // Very long title
    { content: "x".repeat(1000000) }, // Very long content
    { tags: Array(10000).fill("tag") }, // Too many tags
  ],
};
