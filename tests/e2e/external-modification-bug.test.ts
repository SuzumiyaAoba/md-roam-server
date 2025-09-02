import fs from "node:fs";
import { describe, expect, it } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe("External File Modification Bug Investigation", () => {
  it("should handle updates after external file modifications", async () => {
    console.log("=== TESTING EXTERNAL FILE MODIFICATION SCENARIO ===");

    // Create a test node
    const testNode = await TestCleanup.createTestNode({
      title: "External Mod Test",
      content: "Original content for external modification test",
      tags: ["external", "mod"],
      category: "test",
      file_type: "org",
    });

    console.log("Test node created:", testNode.id);
    console.log("File path:", testNode.path);

    // Get the file path from the node response
    const filePath = testNode.path;

    // Read the current file content
    const currentContent = fs.readFileSync(filePath, "utf8");
    console.log("Current file content:");
    console.log(currentContent);

    // Simulate external modification by directly editing the file
    // This mimics what might happen if the file is edited outside the API
    const externallyModifiedContent =
      currentContent +
      "\n\n* Externally Added Section\n\n** Subsection\n\nSome external content.";

    console.log("Simulating external file modification...");
    fs.writeFileSync(filePath, externallyModifiedContent);

    // Now try to update via the API - this might trigger the duplication bug
    console.log("Updating node via API after external modification...");
    const updateData = {
      title: "External Mod Test Updated",
      content: "Updated content after external modification",
      tags: ["external", "mod", "updated"],
      category: "test-updated",
    };

    const updateResponse = await ApiHelpers.updateNode(testNode.id, updateData);
    console.log("Update response status:", updateResponse.status);
    expect(updateResponse.status).toBe(200);

    // Check the file content for duplication
    const afterUpdateContent = await ApiHelpers.getNodeContent(testNode.id);
    expect(afterUpdateContent.status).toBe(200);
    const finalContent = afterUpdateContent.body.content || "";

    console.log("Final file content after API update:");
    console.log(finalContent);

    // Check for duplication
    const propertiesCount = (finalContent.match(/:PROPERTIES:/g) || []).length;
    const endCount = (finalContent.match(/:END:/g) || []).length;
    const titleCount = (finalContent.match(/^#\+title:/gm) || []).length;

    console.log(
      `Final counts - PROPERTIES: ${propertiesCount}, :END: ${endCount}, #+title: ${titleCount}`,
    );

    if (propertiesCount > 1) {
      console.error(
        "BUG DETECTED: PROPERTIES duplication after external modification + API update!",
      );
    }
    if (titleCount > 1) {
      console.error(
        "BUG DETECTED: #+title duplication after external modification + API update!",
      );
    }

    expect(propertiesCount).toBe(1);
    expect(endCount).toBe(1);
    expect(titleCount).toBe(1);

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== EXTERNAL FILE MODIFICATION TEST COMPLETE ===");
  });

  it("should handle database sync after external modifications", async () => {
    console.log("=== TESTING DATABASE SYNC AFTER EXTERNAL CHANGES ===");

    // Create a test node
    const testNode = await TestCleanup.createTestNode({
      title: "DB Sync Test",
      content: "Content for database sync test",
      tags: ["db", "sync"],
      file_type: "org",
    });

    console.log("DB sync test node created:", testNode.id);

    // Get initial content
    const initialContent = await ApiHelpers.getNodeContent(testNode.id);
    expect(initialContent.status).toBe(200);

    // Simulate external modification by adding duplicate metadata manually
    const filePath = testNode.path;
    const duplicatedContent = `:PROPERTIES:
:ID: ${testNode.id}
:END:
#+title: DB Sync Test

:PROPERTIES:
:ID: ${testNode.id}
:END:
#+title: DB Sync Test Duplicate

Content for database sync test

* Additional Content`;

    console.log("Manually creating duplicated metadata in file...");
    fs.writeFileSync(filePath, duplicatedContent);

    // Trigger database sync
    console.log("Triggering database sync...");
    const syncResponse = await ApiHelpers.syncDatabase();
    console.log("Sync response status:", syncResponse.status);

    // Now update via API to see if it fixes the duplication
    console.log("Updating node via API after sync...");
    const updateData = {
      title: "DB Sync Test Fixed",
      content: "Fixed content after database sync",
      tags: ["db", "sync", "fixed"],
    };

    const updateResponse = await ApiHelpers.updateNode(testNode.id, updateData);
    console.log("Update response status:", updateResponse.status);
    expect(updateResponse.status).toBe(200);

    // Check if duplication was fixed
    const afterUpdateContent = await ApiHelpers.getNodeContent(testNode.id);
    expect(afterUpdateContent.status).toBe(200);
    const finalContent = afterUpdateContent.body.content || "";

    console.log("Final content after sync and update:");
    console.log(finalContent);

    const propertiesCount = (finalContent.match(/:PROPERTIES:/g) || []).length;
    const titleCount = (finalContent.match(/^#\+title:/gm) || []).length;

    console.log(
      `After sync+update counts - PROPERTIES: ${propertiesCount}, #+title: ${titleCount}`,
    );

    if (propertiesCount > 1) {
      console.error("BUG: Duplication persists after sync and update!");
    } else {
      console.log("SUCCESS: Duplication was fixed by API update!");
    }

    expect(propertiesCount).toBe(1);
    expect(titleCount).toBe(1);

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== DATABASE SYNC TEST COMPLETE ===");
  });

  it("should detect when existing files have duplication", async () => {
    console.log("=== TESTING EXISTING FILE DUPLICATION DETECTION ===");

    // Get all nodes to find any with potential duplication
    const allNodes = await ApiHelpers.getAllNodes();
    expect(allNodes.status).toBe(200);

    if (!allNodes.body.data || allNodes.body.data.length === 0) {
      console.log("No existing nodes to check for duplication");
      expect(true).toBe(true); // Pass the test
      return;
    }

    console.log(
      `Checking ${allNodes.body.data.length} existing nodes for duplication...`,
    );

    const duplicatedNodes = [];

    for (const node of allNodes.body.data) {
      try {
        const content = await ApiHelpers.getNodeContent(node.id);
        if (content.status === 200 && content.body.content) {
          const contentText = content.body.content;

          const propertiesCount = (contentText.match(/:PROPERTIES:/g) || [])
            .length;
          const titleCount = (contentText.match(/^#\+title:/gm) || []).length;

          if (propertiesCount > 1 || titleCount > 1) {
            console.log(`DUPLICATION FOUND in node ${node.id} (${node.title})`);
            console.log(
              `  - PROPERTIES: ${propertiesCount}, #+title: ${titleCount}`,
            );
            console.log(
              `  - Content preview: ${contentText.substring(0, 200)}...`,
            );

            duplicatedNodes.push({
              id: node.id,
              title: node.title,
              propertiesCount,
              titleCount,
              contentPreview: contentText.substring(0, 200),
            });
          }
        }
      } catch (error) {
        console.warn(`Failed to check node ${node.id}: ${error.message}`);
      }
    }

    if (duplicatedNodes.length > 0) {
      console.log(`Found ${duplicatedNodes.length} nodes with duplication:`);
      duplicatedNodes.forEach((node, index) => {
        console.log(`${index + 1}. ${node.title} (${node.id})`);
        console.log(
          `   PROPERTIES: ${node.propertiesCount}, #+title: ${node.titleCount}`,
        );
      });
    } else {
      console.log("No duplication found in existing nodes");
    }

    // This test is informational - we don't fail it based on existing duplication
    // since that might be historical data
    expect(true).toBe(true);

    console.log("=== EXISTING FILE DUPLICATION CHECK COMPLETE ===");
  });
});
