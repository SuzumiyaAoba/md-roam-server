import { describe, expect, it } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe.skip("Debug File Detection Issues", () => {
  it("should debug why files exist but nodes endpoint returns empty", async () => {
    console.log("=== DEBUGGING FILE DETECTION ISSUE ===");

    // Step 1: Create a test node to ensure we have a file
    console.log("Step 1: Creating test node...");
    const testNode = await TestCleanup.createTestNode({
      title: "Debug File Detection",
      content: "This is a test file to debug detection issues",
      tags: ["debug", "file-detection"],
      file_type: "md",
    });
    console.log("Created test node:", testNode);

    // Step 2: Try to retrieve all nodes immediately
    console.log("Step 2: Attempting to retrieve all nodes...");
    const response = await ApiHelpers.getAllNodes();
    console.log("API Response status:", response.status);
    console.log("API Response body:", JSON.stringify(response.body, null, 2));

    // Step 3: Check if our node is in the response
    if (response.body.data && Array.isArray(response.body.data)) {
      const foundNode = response.body.data.find(
        (node: Record<string, unknown>) => node.id === testNode.id,
      );
      console.log("Node found in response:", foundNode ? "YES" : "NO");
      if (!foundNode) {
        console.error("BUG CONFIRMED: File created but not returned by API");
        console.error("Expected node ID:", testNode.id);
        console.error(
          "All returned node IDs:",
          response.body.data.map((n: Record<string, unknown>) => n.id),
        );
      }
    } else {
      console.error("BUG CONFIRMED: API returned non-array data field");
      console.error("Data field value:", response.body.data);
      console.error("Data field type:", typeof response.body.data);
    }

    // Step 4: Wait and try again (maybe sync is delayed)
    console.log("Step 4: Waiting 2 seconds and trying again...");
    await new Promise((resolve) => setTimeout(resolve, 2000));

    const secondResponse = await ApiHelpers.getAllNodes();
    console.log("Second attempt - Status:", secondResponse.status);
    console.log(
      "Second attempt - Data length:",
      Array.isArray(secondResponse.body.data)
        ? secondResponse.body.data.length
        : "not array",
    );

    if (secondResponse.body.data && Array.isArray(secondResponse.body.data)) {
      const foundNodeSecond = secondResponse.body.data.find(
        (node: Record<string, unknown>) => node.id === testNode.id,
      );
      console.log(
        "Node found in second attempt:",
        foundNodeSecond ? "YES" : "NO",
      );
    }

    // Step 5: Try individual node retrieval
    console.log("Step 5: Trying individual node retrieval...");
    try {
      const individualResponse = await ApiHelpers.getNode(testNode.id);
      console.log("Individual retrieval status:", individualResponse.status);
      console.log(
        "Individual retrieval success:",
        individualResponse.status === 200 ? "YES" : "NO",
      );
    } catch (err) {
      console.error("Individual retrieval failed:", err);
    }

    // Cleanup
    console.log("Cleaning up test node...");
    try {
      await ApiHelpers.deleteNode(testNode.id);
      console.log("Cleanup successful");
    } catch (err) {
      console.error("Cleanup failed:", err);
    }

    console.log("=== DEBUG SESSION COMPLETE ===");

    // The test should always pass - we're just debugging
    expect(true).toBe(true);
  });

  it("should check database synchronization timing", async () => {
    console.log("=== TESTING DATABASE SYNC TIMING ===");

    // Create multiple nodes in sequence
    const nodes = [];
    for (let i = 0; i < 3; i++) {
      console.log(`Creating node ${i + 1}...`);
      const node = await TestCleanup.createTestNode({
        title: `Sync Test Node ${i + 1}`,
        content: `Content for sync test node ${i + 1}`,
        tags: ["sync-test", `node-${i + 1}`],
      });
      nodes.push(node);

      // Check if node appears in list immediately
      const response = await ApiHelpers.getAllNodes();
      const foundCount = nodes.filter(
        (n) =>
          response.body.data &&
          Array.isArray(response.body.data) &&
          response.body.data.find((apiNode: Record<string, unknown>) => apiNode.id === n.id),
      ).length;

      console.log(
        `After creating ${i + 1} nodes, found ${foundCount} in API response`,
      );
    }

    // Final check
    const finalResponse = await ApiHelpers.getAllNodes();
    const finalFoundCount = nodes.filter(
      (n) =>
        finalResponse.body.data &&
        Array.isArray(finalResponse.body.data) &&
        finalResponse.body.data.find((apiNode: Record<string, unknown>) => apiNode.id === n.id),
    ).length;

    console.log(
      `Final check: Created ${nodes.length} nodes, found ${finalFoundCount} in API`,
    );

    if (finalFoundCount !== nodes.length) {
      console.error(
        "SYNC TIMING ISSUE: Not all created nodes appear in API response",
      );
    }

    // Cleanup
    for (const node of nodes) {
      try {
        await ApiHelpers.deleteNode(node.id);
      } catch (err) {
        console.error(`Failed to cleanup node ${node.id}:`, err);
      }
    }

    console.log("=== SYNC TIMING TEST COMPLETE ===");
    expect(true).toBe(true);
  });
});
