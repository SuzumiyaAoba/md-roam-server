import { describe, expect, it } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe("No Content Change Update Bug Investigation", () => {
  it("should not cause duplication when updating without content changes", async () => {
    console.log("=== TESTING NO-CHANGE UPDATE BUG ===");

    // Step 1: Create initial node
    console.log("Step 1: Creating initial test node...");
    const initialNode = await TestCleanup.createTestNode({
      title: "No Change Test",
      content: "Initial content that should not change",
      tags: ["test", "no-change"],
      category: "test",
      file_type: "org",
    });
    console.log("Initial node created:", initialNode);

    // Step 2: Get initial content
    const initialContent = await ApiHelpers.getNodeContent(initialNode.id);
    expect(initialContent.status).toBe(200);
    const originalContent = initialContent.body.content || "";

    console.log("Initial content:");
    console.log(originalContent);

    // Count initial metadata
    const initialPropertiesCount = (
      originalContent.match(/:PROPERTIES:/g) || []
    ).length;
    const initialEndCount = (originalContent.match(/:END:/g) || []).length;
    const initialTitleCount = (originalContent.match(/^#\+title:/gm) || [])
      .length;

    console.log(
      `Initial counts - PROPERTIES: ${initialPropertiesCount}, :END: ${initialEndCount}, #+title: ${initialTitleCount}`,
    );

    expect(initialPropertiesCount).toBe(1);
    expect(initialEndCount).toBe(1);
    expect(initialTitleCount).toBe(1);

    // Step 3: Update with identical data (no actual changes)
    console.log("Step 3: Updating with identical data (no changes)...");
    const identicalUpdateData = {
      title: "No Change Test",
      content: "Initial content that should not change",
      tags: ["test", "no-change"],
      category: "test",
    };

    const identicalUpdateResponse = await ApiHelpers.updateNode(
      initialNode.id,
      identicalUpdateData,
    );
    console.log(
      "Identical update response status:",
      identicalUpdateResponse.status,
    );
    expect(identicalUpdateResponse.status).toBe(200);

    // Step 4: Check content after identical update
    const afterIdenticalContent = await ApiHelpers.getNodeContent(
      initialNode.id,
    );
    expect(afterIdenticalContent.status).toBe(200);
    const afterIdenticalText = afterIdenticalContent.body.content || "";

    console.log("Content after identical update:");
    console.log(afterIdenticalText);

    // Count metadata after identical update
    const afterIdenticalPropertiesCount = (
      afterIdenticalText.match(/:PROPERTIES:/g) || []
    ).length;
    const afterIdenticalEndCount = (afterIdenticalText.match(/:END:/g) || [])
      .length;
    const afterIdenticalTitleCount = (
      afterIdenticalText.match(/^#\+title:/gm) || []
    ).length;

    console.log(
      `After identical update counts - PROPERTIES: ${afterIdenticalPropertiesCount}, :END: ${afterIdenticalEndCount}, #+title: ${afterIdenticalTitleCount}`,
    );

    if (afterIdenticalPropertiesCount > 1) {
      console.error(
        "BUG DETECTED: PROPERTIES duplication after identical update!",
      );
    }
    if (afterIdenticalTitleCount > 1) {
      console.error(
        "BUG DETECTED: #+title duplication after identical update!",
      );
    }

    expect(afterIdenticalPropertiesCount).toBe(1);
    expect(afterIdenticalEndCount).toBe(1);
    expect(afterIdenticalTitleCount).toBe(1);

    // Step 5: Update with partial data (only title)
    console.log("Step 5: Updating with partial data (only title)...");
    const partialUpdateData = {
      title: "No Change Test",
      // Intentionally omitting content, tags, category
    };

    const partialUpdateResponse = await ApiHelpers.updateNode(
      initialNode.id,
      partialUpdateData,
    );
    console.log(
      "Partial update response status:",
      partialUpdateResponse.status,
    );
    expect(partialUpdateResponse.status).toBe(200);

    // Step 6: Check content after partial update
    const afterPartialContent = await ApiHelpers.getNodeContent(initialNode.id);
    expect(afterPartialContent.status).toBe(200);
    const afterPartialText = afterPartialContent.body.content || "";

    console.log("Content after partial update:");
    console.log(afterPartialText);

    // Count metadata after partial update
    const afterPartialPropertiesCount = (
      afterPartialText.match(/:PROPERTIES:/g) || []
    ).length;
    const afterPartialEndCount = (afterPartialText.match(/:END:/g) || [])
      .length;
    const afterPartialTitleCount = (
      afterPartialText.match(/^#\+title:/gm) || []
    ).length;

    console.log(
      `After partial update counts - PROPERTIES: ${afterPartialPropertiesCount}, :END: ${afterPartialEndCount}, #+title: ${afterPartialTitleCount}`,
    );

    if (afterPartialPropertiesCount > 1) {
      console.error(
        "BUG DETECTED: PROPERTIES duplication after partial update!",
      );
    }
    if (afterPartialTitleCount > 1) {
      console.error("BUG DETECTED: #+title duplication after partial update!");
    }

    expect(afterPartialPropertiesCount).toBe(1);
    expect(afterPartialEndCount).toBe(1);
    expect(afterPartialTitleCount).toBe(1);

    // Step 7: Update with empty data (should preserve existing content)
    console.log("Step 7: Updating with minimal data...");
    const minimalUpdateData = {
      title: "No Change Test Updated",
    };

    const minimalUpdateResponse = await ApiHelpers.updateNode(
      initialNode.id,
      minimalUpdateData,
    );
    console.log(
      "Minimal update response status:",
      minimalUpdateResponse.status,
    );
    expect(minimalUpdateResponse.status).toBe(200);

    // Step 8: Final content check
    const finalContent = await ApiHelpers.getNodeContent(initialNode.id);
    expect(finalContent.status).toBe(200);
    const finalText = finalContent.body.content || "";

    console.log("Final content:");
    console.log(finalText);

    // Count final metadata
    const finalPropertiesCount = (finalText.match(/:PROPERTIES:/g) || [])
      .length;
    const finalEndCount = (finalText.match(/:END:/g) || []).length;
    const finalTitleCount = (finalText.match(/^#\+title:/gm) || []).length;

    console.log(
      `Final counts - PROPERTIES: ${finalPropertiesCount}, :END: ${finalEndCount}, #+title: ${finalTitleCount}`,
    );

    if (finalPropertiesCount > 1) {
      console.error("BUG DETECTED: PROPERTIES duplication in final state!");
    }
    if (finalTitleCount > 1) {
      console.error("BUG DETECTED: #+title duplication in final state!");
    }

    expect(finalPropertiesCount).toBe(1);
    expect(finalEndCount).toBe(1);
    expect(finalTitleCount).toBe(1);

    // Cleanup
    console.log("Cleaning up test node...");
    await ApiHelpers.deleteNode(initialNode.id);

    console.log("=== NO-CHANGE UPDATE TEST COMPLETE ===");
  });

  it("should preserve exact content when no changes are made", async () => {
    console.log("=== TESTING CONTENT PRESERVATION ===");

    // Create node with complex content
    const testNode = await TestCleanup.createTestNode({
      title: "Content Preservation Test",
      content:
        "* Heading 1\n\nSome content here.\n\n** Subheading\n\nMore content.",
      tags: ["preserve", "content"],
      category: "preservation",
      file_type: "org",
    });

    console.log("Complex test node created:", testNode.id);

    // Get initial content
    const initialContent = await ApiHelpers.getNodeContent(testNode.id);
    expect(initialContent.status).toBe(200);
    const originalContent = initialContent.body.content || "";

    console.log("Original content length:", originalContent.length);
    console.log(
      "Original content preview (first 200 chars):",
      originalContent.substring(0, 200),
    );

    // Update with identical data
    const preservationUpdate = {
      title: "Content Preservation Test",
      content:
        "* Heading 1\n\nSome content here.\n\n** Subheading\n\nMore content.",
      tags: ["preserve", "content"],
      category: "preservation",
    };

    const updateResponse = await ApiHelpers.updateNode(
      testNode.id,
      preservationUpdate,
    );
    expect(updateResponse.status).toBe(200);

    // Check content is preserved exactly
    const afterUpdateContent = await ApiHelpers.getNodeContent(testNode.id);
    expect(afterUpdateContent.status).toBe(200);
    const updatedContent = afterUpdateContent.body.content || "";

    console.log("Updated content length:", updatedContent.length);
    console.log(
      "Updated content preview (first 200 chars):",
      updatedContent.substring(0, 200),
    );

    // Verify no duplication in the body content area
    const bodyContentStart = updatedContent.indexOf("* Heading 1");
    const bodyContent = updatedContent.substring(bodyContentStart);

    // Should only have one instance of our heading
    const headingMatches = bodyContent.match(/\* Heading 1/g) || [];
    console.log("Heading 1 occurrences in body:", headingMatches.length);

    expect(headingMatches.length).toBe(1);

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== CONTENT PRESERVATION TEST COMPLETE ===");
  });

  it("should handle rapid sequential updates without duplication", async () => {
    console.log("=== TESTING RAPID SEQUENTIAL UPDATES ===");

    const testNode = await TestCleanup.createTestNode({
      title: "Rapid Update Test",
      content: "Initial rapid test content",
      tags: ["rapid"],
      file_type: "org",
    });

    console.log("Rapid test node created:", testNode.id);

    // Perform 5 rapid sequential updates
    for (let i = 1; i <= 5; i++) {
      console.log(`Performing rapid update ${i}...`);

      const updateData = {
        title: `Rapid Update Test - ${i}`,
        content: `Updated content iteration ${i}`,
        tags: ["rapid", `iteration-${i}`],
      };

      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      expect(response.status).toBe(200);

      // Check for duplication after each update
      const content = await ApiHelpers.getNodeContent(testNode.id);
      const contentText = content.body.content || "";

      const propertiesCount = (contentText.match(/:PROPERTIES:/g) || []).length;
      const titleCount = (contentText.match(/^#\+title:/gm) || []).length;

      console.log(
        `After rapid update ${i} - PROPERTIES: ${propertiesCount}, #+title: ${titleCount}`,
      );

      if (propertiesCount > 1) {
        console.error(
          `BUG DETECTED: PROPERTIES duplication after rapid update ${i}!`,
        );
        console.error("Content preview:", contentText.substring(0, 300));
      }

      expect(propertiesCount).toBe(1);
      expect(titleCount).toBe(1);

      // Small delay to avoid overwhelming the server
      await new Promise((resolve) => setTimeout(resolve, 100));
    }

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== RAPID SEQUENTIAL UPDATES TEST COMPLETE ===");
  });
});
