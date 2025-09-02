import type { CreateNodePayload, UpdateNodePayload } from "@/utils/types";

export const TEST_NODES: {
  markdown: CreateNodePayload[];
  org: CreateNodePayload[];
  invalid: Partial<CreateNodePayload>[];
} = {
  markdown: [
    {
      title: "Simple Markdown Note",
      content: "This is a basic markdown note for testing.",
      file_type: "md",
    },
    {
      title: "Rich Markdown Note",
      content:
        "# Heading\n\nThis is a **markdown** note with *formatting* and [links](https://example.com).",
      tags: ["markdown", "test", "rich"],
      aliases: ["Rich MD", "Markdown Test"],
      refs: ["https://example.com", "https://github.com"],
      category: "testing",
      file_type: "md",
    },
    {
      title: "International Markdown Note",
      content:
        "# International Test\n\nThis is an international markdown note for testing.\n\n- List item 1\n- List item 2",
      tags: ["international", "test", "markdown"],
      aliases: ["International Test", "Global Note"],
      category: "international",
      file_type: "md",
    },
  ],

  org: [
    {
      title: "Simple Org Note",
      content: "This is a basic org-mode note for testing.",
      file_type: "org",
    },
    {
      title: "Rich Org Note",
      content:
        "* Heading\n\nThis is an *org-mode* note with /formatting/ and [[https://example.com][links]].",
      tags: ["org", "test", "rich"],
      aliases: ["Rich Org", "Org Test"],
      refs: ["https://orgmode.org"],
      category: "testing",
      file_type: "org",
    },
    {
      title: "International Org Note",
      content:
        "* International Test\n\nThis is an international org note for testing.\n\n- List item 1\n- List item 2",
      tags: ["international", "test", "org"],
      category: "international",
      file_type: "org",
    },
  ],

  invalid: [
    {
      // Missing title
      content: "Note without title",
      file_type: "md",
    },
    {
      title: "", // Empty title
      content: "Note with empty title",
      file_type: "md",
    },
    {
      title: "Invalid file type",
      content: "Note with invalid file type",
      file_type: "invalid" as any,
    },
  ],
};

export const TEST_UPDATES: {
  valid: UpdateNodePayload[];
  invalid: Partial<UpdateNodePayload>[];
} = {
  valid: [
    {
      title: "Updated Title",
    },
    {
      content: "Updated content only",
    },
    {
      title: "Updated Note",
      content: "Both title and content updated",
      tags: ["updated", "modified"],
    },
    {
      tags: ["new-tag", "another-tag"],
      category: "updated-category",
    },
  ],

  invalid: [
    {
      title: "", // Empty title
    },
    {
      file_type: "invalid" as any,
    },
  ],
};

export const TEST_SEARCH_QUERIES = [
  "markdown",
  "test",
  "日本語",
  "heading",
  "nonexistent",
  "", // Empty search
  "case-SENSITIVE-Test", // Case sensitivity test
];

export const TEST_SCENARIOS = {
  nodeCreation: {
    description: "Node creation with different file types and metadata",
    data: TEST_NODES,
  },
  nodeUpdate: {
    description: "Node updates with various field modifications",
    data: TEST_UPDATES,
  },
  fileTypeSelection: {
    description: "File type selection and format validation",
    markdownNode: TEST_NODES.markdown[1],
    orgNode: TEST_NODES.org[1],
  },
  metadataHandling: {
    description: "Metadata processing differences between MD and Org formats",
    richMarkdown: TEST_NODES.markdown[1],
    richOrg: TEST_NODES.org[1],
  },
  internationalization: {
    description: "Unicode and Japanese text handling",
    japanese: {
      markdown: TEST_NODES.markdown[2],
      org: TEST_NODES.org[2],
    },
  },
  errorHandling: {
    description: "Error cases and validation",
    invalidNodes: TEST_NODES.invalid,
    invalidUpdates: TEST_UPDATES.invalid,
  },
};

export const PERFORMANCE_THRESHOLDS = {
  nodeCreation: 5000, // 5 seconds
  nodeRetrieval: 1000, // 1 second
  nodeUpdate: 3000, // 3 seconds
  nodeDeletion: 2000, // 2 seconds
  search: 2000, // 2 seconds
  fileList: 1000, // 1 second
  stats: 500, // 500ms
};

export function generateBulkTestData(
  count: number,
  fileType: "md" | "org" = "md",
): CreateNodePayload[] {
  return Array.from({ length: count }, (_, i) => ({
    title: `Bulk Test Node ${i + 1}`,
    content: `This is bulk test node number ${i + 1} created at ${new Date().toISOString()}`,
    tags: [`bulk-${i + 1}`, "automated", fileType],
    category: "bulk-testing",
    file_type: fileType,
  }));
}

export function createTestDataVariations(
  baseData: CreateNodePayload,
): CreateNodePayload[] {
  return [
    // Base data
    { ...baseData },

    // Without optional fields
    {
      title: `${baseData.title} (minimal)`,
      content: baseData.content,
      file_type: baseData.file_type,
    },

    // With all optional fields
    {
      ...baseData,
      title: `${baseData.title} (maximal)`,
      tags: [...(baseData.tags || []), "maximal", "complete"],
      aliases: [...(baseData.aliases || []), "Max Test"],
      refs: [...(baseData.refs || []), "https://test-ref.com"],
      category: `${baseData.category || "default"}-maximal`,
    },

    // Edge cases
    {
      title: `${baseData.title} (edge-case)`,
      content: `${baseData.content}\n\nSpecial chars: éñ中文🚀`,
      tags: ["edge-case", "unicode", "特殊文字"],
      file_type: baseData.file_type,
    },
  ];
}
