import { describe, it, expect, beforeEach } from "vitest";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";

describe("ID Duplication Bug Investigation", () => {
  it("should test for ID duplication during node updates", async () => {
    console.log("=== TESTING ID DUPLICATION BUG ===");

    // Step 1: Create initial node
    console.log("Step 1: Creating initial test node...");
    const initialNode = await TestCleanup.createTestNode({
      title: "ID Duplication Test Initial",
      content: "Initial content for ID duplication test",
      tags: ["test", "id-duplication"],
      file_type: "md",
    });
    console.log("Initial node created:", initialNode);

    // Step 2: Get initial node list to establish baseline
    const initialList = await ApiHelpers.getAllNodes();
    console.log("Initial node count:", initialList.body.data?.length || 0);
    const initialIds = initialList.body.data?.map((n: any) => n.id) || [];
    console.log("Initial node IDs:", initialIds);

    // Step 3: Update the node
    console.log("Step 2: Updating node...");
    const updateData = {
      title: "ID Duplication Test Updated",
      content: "Updated content for ID duplication test",
      tags: ["test", "id-duplication", "updated"],
    };

    const updateResponse = await ApiHelpers.updateNode(
      initialNode.id,
      updateData,
    );
    console.log("Update response status:", updateResponse.status);
    console.log("Update response ID:", updateResponse.body.id);

    // Step 4: Check for ID duplication
    console.log("Step 3: Checking for ID duplication...");
    const afterUpdateList = await ApiHelpers.getAllNodes();
    console.log(
      "After update node count:",
      afterUpdateList.body.data?.length || 0,
    );

    const afterUpdateIds =
      afterUpdateList.body.data?.map((n: any) => n.id) || [];
    console.log("After update node IDs:", afterUpdateIds);

    // Check if the same ID appears multiple times
    const idCounts: Record<string, number> = {};
    afterUpdateIds.forEach((id: string) => {
      idCounts[id] = (idCounts[id] || 0) + 1;
    });

    const duplicatedIds = Object.entries(idCounts).filter(
      ([_, count]) => count > 1,
    );

    if (duplicatedIds.length > 0) {
      console.error("BUG DETECTED: ID DUPLICATION FOUND!");
      console.error("Duplicated IDs:", duplicatedIds);
      duplicatedIds.forEach(([id, count]) => {
        console.error(`ID ${id} appears ${count} times`);
        const nodesWithSameId = afterUpdateList.body.data?.filter(
          (n: any) => n.id === id,
        );
        console.error(`Nodes with ID ${id}:`, nodesWithSameId);
      });
    } else {
      console.log("No ID duplication detected");
    }

    // Step 5: Verify the original node still exists and wasn't duplicated
    const updatedNodes =
      afterUpdateList.body.data?.filter((n: any) => n.id === initialNode.id) ||
      [];
    console.log(
      `Nodes with original ID ${initialNode.id}:`,
      updatedNodes.length,
    );

    if (updatedNodes.length > 1) {
      console.error("BUG DETECTED: Original node ID duplicated!");
      console.error("Duplicate nodes:", updatedNodes);
    } else if (updatedNodes.length === 1) {
      console.log("Original node updated correctly");
      console.log("Updated node data:", updatedNodes[0]);
    } else {
      console.error("BUG DETECTED: Original node disappeared!");
    }

    // Cleanup
    console.log("Cleaning up...");
    try {
      await ApiHelpers.deleteNode(initialNode.id);
      console.log("Cleanup successful");
    } catch (err) {
      console.error("Cleanup failed:", err);
    }

    console.log("=== ID DUPLICATION TEST COMPLETE ===");

    // Test assertions
    expect(duplicatedIds.length).toBe(0); // No ID duplication should occur
    expect(updatedNodes.length).toBe(1); // Original node should exist exactly once
  });

  it("should test for ID duplication with multiple sequential updates", async () => {
    console.log("=== TESTING SEQUENTIAL UPDATES ===");

    // Create a node
    const testNode = await TestCleanup.createTestNode({
      title: "Sequential Update Test",
      content: "Initial content",
      tags: ["test"],
    });

    // Perform multiple updates
    const updateCount = 5;
    for (let i = 1; i <= updateCount; i++) {
      console.log(`Performing update ${i}...`);

      const updateData = {
        title: `Sequential Update Test - Update ${i}`,
        content: `Content after update ${i}`,
        tags: ["test", `update-${i}`],
      };

      const response = await ApiHelpers.updateNode(testNode.id, updateData);
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(testNode.id);

      // Check for duplicates after each update
      const nodeList = await ApiHelpers.getAllNodes();
      const nodesWithSameId =
        nodeList.body.data?.filter((n: any) => n.id === testNode.id) || [];

      if (nodesWithSameId.length > 1) {
        console.error(`BUG DETECTED: ID duplication after update ${i}!`);
        console.error("Duplicate nodes:", nodesWithSameId);
      }

      expect(nodesWithSameId.length).toBe(1);
    }

    // Sync database to reflect all updates
    await ApiHelpers.syncDatabase();

    // Final verification
    const finalList = await ApiHelpers.getAllNodes();
    const finalNodesWithId =
      finalList.body.data?.filter((n: any) => n.id === testNode.id) || [];

    expect(finalNodesWithId.length).toBe(1);
    expect(finalNodesWithId[0].title).toBe(
      `Sequential Update Test - Update ${updateCount}`,
    );

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== SEQUENTIAL UPDATES TEST COMPLETE ===");
  });

  it("should test for ID duplication with concurrent updates", async () => {
    console.log("=== TESTING CONCURRENT UPDATES ===");

    // Create a node
    const testNode = await TestCleanup.createTestNode({
      title: "Concurrent Update Test",
      content: "Initial content",
      tags: ["test"],
    });

    // Perform concurrent updates
    console.log("Performing concurrent updates...");
    const concurrentUpdates = [
      ApiHelpers.updateNode(testNode.id, {
        title: "Concurrent Update 1",
        content: "Content 1",
      }),
      ApiHelpers.updateNode(testNode.id, {
        title: "Concurrent Update 2",
        content: "Content 2",
      }),
      ApiHelpers.updateNode(testNode.id, {
        title: "Concurrent Update 3",
        content: "Content 3",
      }),
    ];

    const responses = await Promise.allSettled(concurrentUpdates);

    // Check responses
    responses.forEach((response, index) => {
      if (response.status === "fulfilled") {
        console.log(
          `Concurrent update ${index + 1} succeeded:`,
          response.value.status,
        );
        expect(response.value.body.id).toBe(testNode.id);
      } else {
        console.log(`Concurrent update ${index + 1} failed:`, response.reason);
      }
    });

    // Check for ID duplication after concurrent updates
    const nodeList = await ApiHelpers.getAllNodes();
    const nodesWithSameId =
      nodeList.body.data?.filter((n: any) => n.id === testNode.id) || [];

    if (nodesWithSameId.length > 1) {
      console.error("BUG DETECTED: ID duplication after concurrent updates!");
      console.error("Duplicate nodes:", nodesWithSameId);
    }

    expect(nodesWithSameId.length).toBe(1);

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== CONCURRENT UPDATES TEST COMPLETE ===");
  });

  it("should detect database sync issues causing apparent duplication", async () => {
    console.log("=== TESTING DATABASE SYNC ISSUES ===");

    // Create node
    const testNode = await TestCleanup.createTestNode({
      title: "Sync Issue Test",
      content: "Testing sync issues",
      tags: ["sync-test"],
    });

    // Update node
    const updateResponse = await ApiHelpers.updateNode(testNode.id, {
      title: "Sync Issue Test Updated",
      content: "Updated to test sync issues",
    });
    expect(updateResponse.status).toBe(200);

    // Multiple rapid calls to get all nodes (may reveal sync timing issues)
    const rapidCalls = await Promise.all([
      ApiHelpers.getAllNodes(),
      ApiHelpers.getAllNodes(),
      ApiHelpers.getAllNodes(),
      ApiHelpers.getAllNodes(),
      ApiHelpers.getAllNodes(),
    ]);

    // Check consistency across all calls
    const allNodeCounts = rapidCalls.map(
      (response) => response.body.data?.length || 0,
    );
    const allNodeIds = rapidCalls.map(
      (response) => response.body.data?.map((n: any) => n.id) || [],
    );

    console.log("Node counts from rapid calls:", allNodeCounts);

    // Check if all calls return consistent results
    const firstCount = allNodeCounts[0];
    const consistentCounts = allNodeCounts.every(
      (count) => count === firstCount,
    );

    if (!consistentCounts) {
      console.error(
        "BUG DETECTED: Inconsistent node counts across rapid API calls",
      );
      console.error("Node counts:", allNodeCounts);
    }

    // Check for our specific node in each response
    const ourNodeCounts = allNodeIds.map(
      (ids) => ids.filter((id: string) => id === testNode.id).length,
    );
    console.log("Our node counts from rapid calls:", ourNodeCounts);

    const consistentOurNode = ourNodeCounts.every((count) => count === 1);
    if (!consistentOurNode) {
      console.error(
        "BUG DETECTED: Inconsistent node presence across rapid API calls",
      );
      console.error("Our node counts:", ourNodeCounts);
    }

    expect(consistentCounts).toBe(true);
    expect(consistentOurNode).toBe(true);

    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);

    console.log("=== DATABASE SYNC TEST COMPLETE ===");
  });
});
