import { beforeEach, describe, expect, it } from "vitest";
import { TEST_NODES, TEST_UPDATES } from "@/fixtures/testData";
import { ApiHelpers, TestCleanup } from "@/utils/apiHelpers";
import type {
  CreateNodeRequest,
  NodeData,
  UpdateNodePayload,
} from "@/utils/types";

describe("Nodes API E2E Tests", () => {
  describe("POST /nodes - Node Creation", () => {
    it("should create a basic markdown node", async () => {
      const nodeData = TEST_NODES.markdown[0];
      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "md",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    it("should create a basic org node", async () => {
      const nodeData = TEST_NODES.org[0];
      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "org",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    it("should create a rich markdown node with all metadata", async () => {
      const nodeData = TEST_NODES.markdown[1];
      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "md",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    it("should create a rich org node with all metadata", async () => {
      const nodeData = TEST_NODES.org[1];
      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "org",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    it("should handle international content correctly", async () => {
      const nodeData = TEST_NODES.markdown[2];
      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "md",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    it("should default to markdown when file_type is not specified", async () => {
      const nodeData = {
        title: "Default File Type Test",
        content: "Testing default file type behavior",
      };

      const response = await ApiHelpers.createNode(nodeData);

      expect(response.status).toBe(201);
      const createdNode = ApiHelpers.expectNodeResponse(response, {
        title: nodeData.title,
        file_type: "md",
      });

      TestCleanup.trackNode(createdNode.id);
    });

    describe("Error Cases", () => {
      it("should reject node creation without title", async () => {
        const nodeData = TEST_NODES.invalid[0] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);

        ApiHelpers.expectErrorResponse(response, 400);
      });

      it("should reject node creation with empty title", async () => {
        const nodeData = TEST_NODES.invalid[1] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);

        ApiHelpers.expectErrorResponse(response, 400);
      });

      it("should reject node creation with invalid file type", async () => {
        const nodeData = TEST_NODES.invalid[2] as CreateNodeRequest;
        const response = await ApiHelpers.createNode(nodeData);

        ApiHelpers.expectErrorResponse(response, 400);
      });
    });
  });

  describe("GET /nodes/:id - Node Retrieval", () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode(TEST_NODES.markdown[1]);
    });

    it("should retrieve existing node by ID", async () => {
      const response = await ApiHelpers.getNode(testNode.id);

      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: testNode.title,
        file_type: testNode.file_type,
      });
    });

    it("should return 404 for non-existent node", async () => {
      const fakeId = "non-existent-id";
      const response = await ApiHelpers.getNode(fakeId);

      ApiHelpers.expectErrorResponse(response, 404);
    });
  });

  describe("PUT /nodes/:id - Node Updates", () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode(TEST_NODES.markdown[1]);
    });

    it("should update node title only", async () => {
      const updateData = TEST_UPDATES.valid[0];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);

      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: updateData.title,
        file_type: testNode.file_type,
      });
    });

    it("should update node content only", async () => {
      const updateData = TEST_UPDATES.valid[1];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);

      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
    });

    it("should update both title and content", async () => {
      const updateData = TEST_UPDATES.valid[2];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);

      expect(response.status).toBe(200);
      ApiHelpers.expectNodeResponse(response, {
        id: testNode.id,
        title: updateData.title,
        file_type: testNode.file_type,
      });
    });

    it("should update tags and category", async () => {
      const updateData = TEST_UPDATES.valid[3];
      const response = await ApiHelpers.updateNode(testNode.id, updateData);

      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);
    });

    describe("Error Cases", () => {
      it("should return 404 for non-existent node", async () => {
        const fakeId = "non-existent-id";
        const updateData = TEST_UPDATES.valid[0];
        const response = await ApiHelpers.updateNode(fakeId, updateData);

        ApiHelpers.expectErrorResponse(response, 404);
      });

      it("should reject update with empty title", async () => {
        const updateData = TEST_UPDATES.invalid[0] as UpdateNodePayload;
        const response = await ApiHelpers.updateNode(testNode.id, updateData);

        ApiHelpers.expectErrorResponse(response, 400);
      });

      it("should reject update with invalid file type", async () => {
        const updateData = TEST_UPDATES.invalid[1] as UpdateNodePayload;
        const response = await ApiHelpers.updateNode(testNode.id, updateData);

        ApiHelpers.expectErrorResponse(response, 400);
      });
    });
  });

  describe("DELETE /nodes/:id - Node Deletion", () => {
    let testNode: NodeData;

    beforeEach(async () => {
      testNode = await TestCleanup.createTestNode();
    });

    it("should delete existing node", async () => {
      const response = await ApiHelpers.deleteNode(testNode.id);

      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);

      // Verify node is actually deleted
      const getResponse = await ApiHelpers.getNode(testNode.id);
      ApiHelpers.expectErrorResponse(getResponse, 404);
    });

    it("should return 404 for non-existent node", async () => {
      const fakeId = "non-existent-id";
      const response = await ApiHelpers.deleteNode(fakeId);

      ApiHelpers.expectErrorResponse(response, 404);
    });
  });

  describe("GET /nodes - All Nodes", () => {
    let createdNodes: NodeData[];

    it("should handle empty database correctly", async () => {
      // This test should be run in isolation or with proper cleanup
      // For now, we'll test the structure without requiring an empty database
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty("status", "success");
      expect(response.body).toHaveProperty("message");
      expect(response.body).toHaveProperty("timestamp");

      // Database should return proper structure regardless of content
      expect(response.body).toHaveProperty("data");
      if (response.body.data === null) {
        // null is acceptable for empty database
        expect(response.body.count).toBe(0);
      } else {
        // or it should be an array
        expect(Array.isArray(response.body.data)).toBe(true);
        if (response.body.count !== undefined) {
          expect(response.body.count).toBe(response.body.data.length);
        }
      }
    });

    it("should detect existing files that are not in database yet", async () => {
      // This test simulates the bug where files exist but aren't returned by the API
      // First get current state
      const initialResponse = await ApiHelpers.getAllNodes();
      expect(initialResponse.status).toBe(200);
      const _initialCount = initialResponse.body.data
        ? initialResponse.body.data.length
        : 0;

      // Create a test file to ensure we have something to detect
      const testNode = await TestCleanup.createTestNode({
        title: "File Detection Test",
        content: "This tests file detection functionality",
        tags: ["file-detection", "test"],
      });

      // Verify the file was created and can be retrieved
      const afterCreateResponse = await ApiHelpers.getAllNodes();
      expect(afterCreateResponse.status).toBe(200);
      expect(afterCreateResponse.body.data).not.toBeNull();
      expect(Array.isArray(afterCreateResponse.body.data)).toBe(true);

      const foundNode = afterCreateResponse.body.data.find(
        (node: Record<string, unknown>) => node.id === testNode.id,
      );
      if (!foundNode) {
        console.error("BUG DETECTED: File exists but not returned by API");
        console.error("Created node:", testNode);
        console.error("API response:", afterCreateResponse.body);

        // This indicates the bug - file exists but API doesn't return it
        expect(foundNode).toBeDefined(
          "File exists but was not returned by Node list API",
        );
      }

      // Clean up
      await ApiHelpers.deleteNode(testNode.id);
    });

    it("should handle org-roam database synchronization properly", async () => {
      // Test to ensure database sync issues don't cause missing nodes
      const testNode = await TestCleanup.createTestNode({
        title: "Database Sync Test",
        content: "Testing database synchronization",
        file_type: "md",
      });

      // Force a sync by calling the API multiple times
      const responses = await Promise.all([
        ApiHelpers.getAllNodes(),
        ApiHelpers.getAllNodes(),
        ApiHelpers.getAllNodes(),
      ]);

      // All responses should be consistent
      responses.forEach((response, index) => {
        expect(response.status).toBe(200);
        expect(Array.isArray(response.body.data)).toBe(true);

        const foundNode = response.body.data.find(
          (node: Record<string, unknown>) => node.id === testNode.id,
        );
        if (!foundNode) {
          console.error(
            `BUG DETECTED: Database sync issue - node missing in response ${index + 1}`,
          );
        }
        expect(foundNode).toBeDefined(
          `Node should be found in response ${index + 1}`,
        );
      });

      // Clean up
      await ApiHelpers.deleteNode(testNode.id);
    });

    it("should handle both md and org files consistently", async () => {
      // Test both file types to ensure the bug isn't format-specific
      const mdNode = await TestCleanup.createTestNode({
        title: "MD File Test",
        content: "# Markdown Test\n\nTesting markdown file detection",
        file_type: "md",
        tags: ["markdown", "test"],
      });

      const orgNode = await TestCleanup.createTestNode({
        title: "Org File Test",
        content: "* Org Test\n\nTesting org file detection",
        file_type: "org",
        tags: ["org", "test"],
      });

      // Check that both files are detected
      const response = await ApiHelpers.getAllNodes();
      expect(response.status).toBe(200);
      expect(Array.isArray(response.body.data)).toBe(true);

      const foundMdNode = response.body.data.find(
        (node: Record<string, unknown>) => node.id === mdNode.id,
      );
      const foundOrgNode = response.body.data.find(
        (node: Record<string, unknown>) => node.id === orgNode.id,
      );

      if (!foundMdNode) {
        console.error("BUG DETECTED: Markdown file not detected by API");
        console.error("Expected MD node:", mdNode);
      }

      if (!foundOrgNode) {
        console.error("BUG DETECTED: Org file not detected by API");
        console.error("Expected Org node:", orgNode);
      }

      expect(foundMdNode).toBeDefined("Markdown file should be detected");
      expect(foundOrgNode).toBeDefined("Org file should be detected");

      // Verify correct file types
      if (foundMdNode) {
        expect(foundMdNode.file_type).toBe("md");
        expect(foundMdNode.file).toMatch(/\.md$/);
      }

      if (foundOrgNode) {
        expect(foundOrgNode.file_type).toBe("org");
        expect(foundOrgNode.file).toMatch(/\.org$/);
      }

      // Clean up
      await ApiHelpers.deleteNode(mdNode.id);
      await ApiHelpers.deleteNode(orgNode.id);
    });

    beforeEach(async () => {
      // Create a few test nodes with known data
      createdNodes = await Promise.all([
        TestCleanup.createTestNode({
          title: "Node List Test 1",
          content: "Content for node 1",
          tags: ["test", "node-list"],
          file_type: "md",
        }),
        TestCleanup.createTestNode({
          title: "Node List Test 2",
          content: "Content for node 2",
          tags: ["test", "node-list"],
          file_type: "md",
        }),
        TestCleanup.createTestNode({
          title: "Node List Test 3 (Org)",
          content: "* Content for org node",
          tags: ["test", "node-list", "org"],
          file_type: "org",
        }),
      ]);
    });

    it("should retrieve all nodes", async () => {
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      ApiHelpers.expectSuccessResponse(response);

      expect(response.body).toHaveProperty("data");
      expect(Array.isArray(response.body.data)).toBe(true);
      expect(response.body.data.length).toBeGreaterThanOrEqual(3);

      // Verify node structure
      response.body.data.forEach((node: Record<string, unknown>) => {
        expect(node).toHaveProperty("id");
        expect(node).toHaveProperty("title");
        expect(node).toHaveProperty("file");
        expect(node).toHaveProperty("file_type");
        expect(node).toHaveProperty("path");

        // Verify data types
        expect(typeof node.id).toBe("string");
        expect(typeof node.title).toBe("string");
        expect(typeof node.file).toBe("string");
        expect(["md", "org"].includes(node.file_type)).toBe(true);
        expect(typeof node.path).toBe("string");
      });
    });

    it("should include all created test nodes", async () => {
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      const allNodes = response.body.data;

      // Find our created nodes in the response
      const foundNodes = createdNodes.map((createdNode) => {
        const foundNode = allNodes.find(
          (node: Record<string, unknown>) => node.id === createdNode.id,
        );
        return { created: createdNode, found: foundNode };
      });

      // All created nodes should be found
      foundNodes.forEach(({ created, found }, index) => {
        expect(found).toBeDefined(
          `Created node ${index + 1} (${created.title}) should be in the list`,
        );
        expect(found.id).toBe(created.id);
        expect(found.title).toBe(created.title);
        expect(found.file_type).toBe(created.file_type);
      });
    });

    it("should return nodes with correct file extensions", async () => {
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      const allNodes = response.body.data;

      // Find our test nodes
      const testNodes = allNodes.filter((node: Record<string, unknown>) =>
        node.title.startsWith("Node List Test"),
      );

      expect(testNodes.length).toBeGreaterThanOrEqual(3);

      // Check file extensions match file types
      testNodes.forEach((node: Record<string, unknown>) => {
        if (node.file_type === "md") {
          expect(node.file).toMatch(/\.md$/);
        } else if (node.file_type === "org") {
          expect(node.file).toMatch(/\.org$/);
        }
      });
    });

    it("should return consistent data across multiple requests", async () => {
      // Make multiple requests
      const [response1, response2, response3] = await Promise.all([
        ApiHelpers.getAllNodes(),
        ApiHelpers.getAllNodes(),
        ApiHelpers.getAllNodes(),
      ]);

      // All should succeed
      expect(response1.status).toBe(200);
      expect(response2.status).toBe(200);
      expect(response3.status).toBe(200);

      const nodes1 = response1.body.data;
      const nodes2 = response2.body.data;
      const nodes3 = response3.body.data;

      // Should return same number of nodes
      expect(nodes1.length).toBe(nodes2.length);
      expect(nodes2.length).toBe(nodes3.length);

      // Should contain same node IDs (order may vary)
      const ids1 = nodes1.map((n: Record<string, unknown>) => n.id).sort();
      const ids2 = nodes2.map((n: Record<string, unknown>) => n.id).sort();
      const ids3 = nodes3.map((n: Record<string, unknown>) => n.id).sort();

      expect(ids1).toEqual(ids2);
      expect(ids2).toEqual(ids3);
    });

    it("should handle large node lists efficiently", async () => {
      const startTime = Date.now();
      const response = await ApiHelpers.getAllNodes();
      const endTime = Date.now();

      expect(response.status).toBe(200);
      expect(endTime - startTime).toBeLessThan(2000); // Should complete within 2 seconds

      // Should handle empty result gracefully
      expect(Array.isArray(response.body.data)).toBe(true);
    });

    it("should return valid paths for all nodes", async () => {
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      const allNodes = response.body.data;

      allNodes.forEach((node: Record<string, unknown>) => {
        expect(node.path).toBeTruthy();
        expect(typeof node.path).toBe("string");
        expect(node.path.length).toBeGreaterThan(0);

        // Path should contain the filename
        expect(node.path).toContain(node.file);
      });
    });

    it("should return proper metadata for mixed file types", async () => {
      const response = await ApiHelpers.getAllNodes();

      expect(response.status).toBe(200);
      const allNodes = response.body.data;

      const markdownNodes = allNodes.filter(
        (node: Record<string, unknown>) => node.file_type === "md",
      );
      const orgNodes = allNodes.filter((node: Record<string, unknown>) => node.file_type === "org");

      // Should have both types from our test data
      expect(markdownNodes.length).toBeGreaterThan(0);
      expect(orgNodes.length).toBeGreaterThan(0);

      // Each type should have proper structure
      markdownNodes.forEach((node: Record<string, unknown>) => {
        expect(node.file).toMatch(/\.md$/);
        expect(node.file_type).toBe("md");
      });

      orgNodes.forEach((node: Record<string, unknown>) => {
        expect(node.file).toMatch(/\.org$/);
        expect(node.file_type).toBe("org");
      });
    });

    it("should handle node list after CRUD operations", async () => {
      // Get initial count
      const initialResponse = await ApiHelpers.getAllNodes();
      expect(initialResponse.status).toBe(200);
      const initialCount = initialResponse.body.data.length;

      // Create a new node
      const newNode = await TestCleanup.createTestNode({
        title: "CRUD Test Node",
        content: "Test content for CRUD operations",
      });

      // Verify count increased
      const afterCreateResponse = await ApiHelpers.getAllNodes();
      expect(afterCreateResponse.status).toBe(200);
      expect(afterCreateResponse.body.data.length).toBe(initialCount + 1);

      // Update the node
      await ApiHelpers.updateNode(newNode.id, {
        title: "Updated CRUD Test Node",
      });

      // Sync database to reflect the update
      await ApiHelpers.syncDatabase();

      // Verify node is still in list with updated title
      const afterUpdateResponse = await ApiHelpers.getAllNodes();
      expect(afterUpdateResponse.status).toBe(200);
      expect(afterUpdateResponse.body.data.length).toBe(initialCount + 1);

      const updatedNode = afterUpdateResponse.body.data.find(
        (node: Record<string, unknown>) => node.id === newNode.id,
      );
      expect(updatedNode).toBeDefined();
      expect(updatedNode.title).toBe("Updated CRUD Test Node");

      // Delete the node
      await ApiHelpers.deleteNode(newNode.id);

      // Verify count decreased
      const afterDeleteResponse = await ApiHelpers.getAllNodes();
      expect(afterDeleteResponse.status).toBe(200);
      expect(afterDeleteResponse.body.data.length).toBe(initialCount);

      // Verify node is no longer in list
      const deletedNodeCheck = afterDeleteResponse.body.data.find(
        (node: Record<string, unknown>) => node.id === newNode.id,
      );
      expect(deletedNodeCheck).toBeUndefined();
    });

    it("should return consistent response structure for empty and populated states", async () => {
      // Get baseline response (may have nodes from beforeEach)
      const initialResponse = await ApiHelpers.getAllNodes();
      expect(initialResponse.status).toBe(200);
      expect(initialResponse.body).toHaveProperty("status", "success");
      expect(initialResponse.body).toHaveProperty("message");
      expect(initialResponse.body).toHaveProperty("timestamp");
      expect(initialResponse.body).toHaveProperty("data");

      // Create a node to ensure populated state
      const tempNode = await TestCleanup.createTestNode({
        title: "Consistency Test Node",
        content: "Testing response structure consistency",
      });

      const populatedResponse = await ApiHelpers.getAllNodes();
      expect(populatedResponse.status).toBe(200);
      expect(populatedResponse.body).toHaveProperty("status", "success");
      expect(populatedResponse.body).toHaveProperty("message");
      expect(populatedResponse.body).toHaveProperty("timestamp");
      expect(populatedResponse.body).toHaveProperty("data");
      expect(Array.isArray(populatedResponse.body.data)).toBe(true);
      expect(populatedResponse.body.data.length).toBeGreaterThan(0);

      // If count property is present, it should be consistent
      if (populatedResponse.body.count !== undefined) {
        expect(populatedResponse.body.count).toBe(
          populatedResponse.body.data.length,
        );
      }

      // Clean up temp node
      await ApiHelpers.deleteNode(tempNode.id);
    });

    it("should validate response structure across different scenarios", async () => {
      // Test with node creation followed by deletion
      const testNode = await TestCleanup.createTestNode({
        title: "Structure Validation Node",
        content: "Testing API response structure validation",
      });

      // Check after creation
      const afterCreateResponse = await ApiHelpers.getAllNodes();
      expect(afterCreateResponse.status).toBe(200);

      // Validate response structure
      expect(afterCreateResponse.body).toHaveProperty("status", "success");
      expect(typeof afterCreateResponse.body.message).toBe("string");
      expect(typeof afterCreateResponse.body.timestamp).toBe("string");
      expect(afterCreateResponse.body).toHaveProperty("data");

      if (afterCreateResponse.body.data !== null) {
        expect(Array.isArray(afterCreateResponse.body.data)).toBe(true);
        afterCreateResponse.body.data.forEach((node: Record<string, unknown>) => {
          expect(node).toHaveProperty("id");
          expect(node).toHaveProperty("title");
          expect(node).toHaveProperty("file");
          expect(node).toHaveProperty("file_type");
          expect(typeof node.id).toBe("string");
          expect(typeof node.title).toBe("string");
          expect(typeof node.file).toBe("string");
          expect(["md", "org"].includes(node.file_type)).toBe(true);
        });
      }

      // Clean up and test deletion
      await ApiHelpers.deleteNode(testNode.id);
    });
  });
});
