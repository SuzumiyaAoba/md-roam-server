import { describe, it, expect, beforeEach } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';

describe('Metadata Duplication Bug Investigation', () => {
  it('should test for PROPERTIES duplication in org files during updates', async () => {
    console.log('=== TESTING ORG PROPERTIES DUPLICATION ===');
    
    // Step 1: Create initial org node
    console.log('Step 1: Creating initial org node...');
    const initialNode = await TestCleanup.createTestNode({
      title: 'Org Properties Test',
      content: 'Initial content for org properties test',
      tags: ['test', 'org'],
      category: 'test',
      file_type: 'org'
    });
    console.log('Initial org node created:', initialNode);
    
    // Step 2: Get the node content to check initial PROPERTIES block
    const initialContent = await ApiHelpers.getNodeContent(initialNode.id);
    console.log('Initial file content status:', initialContent.status);
    if (initialContent.status === 200) {
      const content = initialContent.body.content || '';
      console.log('Initial content preview (first 300 chars):', content.substring(0, 300));
      
      // Count PROPERTIES blocks
      const propertiesMatches = content.match(/:PROPERTIES:/g);
      const endPropertiesMatches = content.match(/:END:/g);
      const titleMatches = content.match(/^#\+title:/gm);
      
      console.log('Initial PROPERTIES count:', propertiesMatches ? propertiesMatches.length : 0);
      console.log('Initial :END: count:', endPropertiesMatches ? endPropertiesMatches.length : 0);
      console.log('Initial #+title count:', titleMatches ? titleMatches.length : 0);
      
      expect(propertiesMatches ? propertiesMatches.length : 0).toBe(1);
      expect(endPropertiesMatches ? endPropertiesMatches.length : 0).toBe(1);
      expect(titleMatches ? titleMatches.length : 0).toBe(1);
    }
    
    // Step 3: Update the node multiple times
    for (let i = 1; i <= 3; i++) {
      console.log(`Step 3.${i}: Performing update ${i}...`);
      
      const updateData = {
        title: `Org Properties Test - Update ${i}`,
        content: `Updated content for org properties test - iteration ${i}`,
        tags: ['test', 'org', `update-${i}`],
        category: `test-update-${i}`
      };
      
      const updateResponse = await ApiHelpers.updateNode(initialNode.id, updateData);
      console.log(`Update ${i} response status:`, updateResponse.status);
      expect(updateResponse.status).toBe(200);
      
      // Check content after each update
      const updatedContent = await ApiHelpers.getNodeContent(initialNode.id);
      if (updatedContent.status === 200) {
        const content = updatedContent.body.content || '';
        console.log(`After update ${i} - content preview (first 400 chars):`, content.substring(0, 400));
        
        // Check for duplicated metadata
        const propertiesMatches = content.match(/:PROPERTIES:/g);
        const endPropertiesMatches = content.match(/:END:/g);
        const titleMatches = content.match(/^#\+title:/gm);
        const categoryMatches = content.match(/^#\+category:/gm);
        
        console.log(`After update ${i} - PROPERTIES count:`, propertiesMatches ? propertiesMatches.length : 0);
        console.log(`After update ${i} - :END: count:`, endPropertiesMatches ? endPropertiesMatches.length : 0);
        console.log(`After update ${i} - #+title count:`, titleMatches ? titleMatches.length : 0);
        console.log(`After update ${i} - #+category count:`, categoryMatches ? categoryMatches.length : 0);
        
        if (propertiesMatches && propertiesMatches.length > 1) {
          console.error(`BUG DETECTED: PROPERTIES duplication after update ${i}!`);
          console.error(`PROPERTIES blocks found: ${propertiesMatches.length}`);
        }
        
        if (titleMatches && titleMatches.length > 1) {
          console.error(`BUG DETECTED: #+title duplication after update ${i}!`);
          console.error(`#+title lines found: ${titleMatches.length}`);
        }
        
        if (categoryMatches && categoryMatches.length > 1) {
          console.error(`BUG DETECTED: #+category duplication after update ${i}!`);
          console.error(`#+category lines found: ${categoryMatches.length}`);
        }
        
        // Test assertions
        expect(propertiesMatches ? propertiesMatches.length : 0).toBe(1);
        expect(endPropertiesMatches ? endPropertiesMatches.length : 0).toBe(1);
        expect(titleMatches ? titleMatches.length : 0).toBe(1);
        expect(categoryMatches ? categoryMatches.length : 0).toBe(1);
      }
    }
    
    // Cleanup
    console.log('Cleaning up test node...');
    try {
      await ApiHelpers.deleteNode(initialNode.id);
      console.log('Cleanup successful');
    } catch (err) {
      console.error('Cleanup failed:', err);
    }
    
    console.log('=== ORG PROPERTIES DUPLICATION TEST COMPLETE ===');
  });

  it('should test for YAML front matter duplication in markdown files during updates', async () => {
    console.log('=== TESTING MARKDOWN YAML DUPLICATION ===');
    
    // Step 1: Create initial markdown node
    console.log('Step 1: Creating initial markdown node...');
    const initialNode = await TestCleanup.createTestNode({
      title: 'Markdown YAML Test',
      content: 'Initial content for markdown YAML test',
      tags: ['test', 'markdown'],
      aliases: ['MD Test'],
      refs: ['https://example.com'],
      category: 'test',
      file_type: 'md'
    });
    console.log('Initial markdown node created:', initialNode);
    
    // Step 2: Check initial YAML front matter
    const initialContent = await ApiHelpers.getNodeContent(initialNode.id);
    console.log('Initial file content status:', initialContent.status);
    if (initialContent.status === 200) {
      const content = initialContent.body.content || '';
      console.log('Initial content preview (first 400 chars):', content.substring(0, 400));
      
      // Count YAML delimiters and metadata
      const yamlStartMatches = content.match(/^---$/gm);
      const titleMatches = content.match(/^title:/gm);
      const idMatches = content.match(/^id:/gm);
      const categoryMatches = content.match(/^category:/gm);
      
      console.log('Initial YAML start (---) count:', yamlStartMatches ? yamlStartMatches.length : 0);
      console.log('Initial title: count:', titleMatches ? titleMatches.length : 0);
      console.log('Initial id: count:', idMatches ? idMatches.length : 0);
      console.log('Initial category: count:', categoryMatches ? categoryMatches.length : 0);
      
      expect(yamlStartMatches ? yamlStartMatches.length : 0).toBe(2); // Opening and closing ---
      expect(titleMatches ? titleMatches.length : 0).toBe(1);
      expect(idMatches ? idMatches.length : 0).toBe(1);
      expect(categoryMatches ? categoryMatches.length : 0).toBe(1);
    }
    
    // Step 3: Update the node multiple times
    for (let i = 1; i <= 3; i++) {
      console.log(`Step 3.${i}: Performing markdown update ${i}...`);
      
      const updateData = {
        title: `Markdown YAML Test - Update ${i}`,
        content: `Updated content for markdown YAML test - iteration ${i}`,
        tags: ['test', 'markdown', `update-${i}`],
        aliases: ['MD Test', `Alias ${i}`],
        refs: ['https://example.com', `https://test${i}.com`],
        category: `test-update-${i}`
      };
      
      const updateResponse = await ApiHelpers.updateNode(initialNode.id, updateData);
      console.log(`Markdown update ${i} response status:`, updateResponse.status);
      expect(updateResponse.status).toBe(200);
      
      // Check content after each update
      const updatedContent = await ApiHelpers.getNodeContent(initialNode.id);
      if (updatedContent.status === 200) {
        const content = updatedContent.body.content || '';
        console.log(`After markdown update ${i} - content preview (first 500 chars):`, content.substring(0, 500));
        
        // Check for duplicated YAML metadata
        const yamlStartMatches = content.match(/^---$/gm);
        const titleMatches = content.match(/^title:/gm);
        const idMatches = content.match(/^id:/gm);
        const categoryMatches = content.match(/^category:/gm);
        const aliasMatches = content.match(/^roam_aliases:/gm);
        const refMatches = content.match(/^roam_refs:/gm);
        
        console.log(`After markdown update ${i} - YAML start (---) count:`, yamlStartMatches ? yamlStartMatches.length : 0);
        console.log(`After markdown update ${i} - title: count:`, titleMatches ? titleMatches.length : 0);
        console.log(`After markdown update ${i} - id: count:`, idMatches ? idMatches.length : 0);
        console.log(`After markdown update ${i} - category: count:`, categoryMatches ? categoryMatches.length : 0);
        console.log(`After markdown update ${i} - roam_aliases: count:`, aliasMatches ? aliasMatches.length : 0);
        console.log(`After markdown update ${i} - roam_refs: count:`, refMatches ? refMatches.length : 0);
        
        if (yamlStartMatches && yamlStartMatches.length > 2) {
          console.error(`BUG DETECTED: YAML front matter duplication after markdown update ${i}!`);
          console.error(`YAML delimiters found: ${yamlStartMatches.length} (expected 2)`);
        }
        
        if (titleMatches && titleMatches.length > 1) {
          console.error(`BUG DETECTED: title: duplication after markdown update ${i}!`);
          console.error(`title: lines found: ${titleMatches.length}`);
        }
        
        if (idMatches && idMatches.length > 1) {
          console.error(`BUG DETECTED: id: duplication after markdown update ${i}!`);
          console.error(`id: lines found: ${idMatches.length}`);
        }
        
        if (categoryMatches && categoryMatches.length > 1) {
          console.error(`BUG DETECTED: category: duplication after markdown update ${i}!`);
          console.error(`category: lines found: ${categoryMatches.length}`);
        }
        
        // Test assertions
        expect(yamlStartMatches ? yamlStartMatches.length : 0).toBe(2);
        expect(titleMatches ? titleMatches.length : 0).toBe(1);
        expect(idMatches ? idMatches.length : 0).toBe(1);
        expect(categoryMatches ? categoryMatches.length : 0).toBe(1);
        expect(aliasMatches ? aliasMatches.length : 0).toBeLessThanOrEqual(1);
        expect(refMatches ? refMatches.length : 0).toBeLessThanOrEqual(1);
      }
    }
    
    // Cleanup
    console.log('Cleaning up markdown test node...');
    try {
      await ApiHelpers.deleteNode(initialNode.id);
      console.log('Cleanup successful');
    } catch (err) {
      console.error('Cleanup failed:', err);
    }
    
    console.log('=== MARKDOWN YAML DUPLICATION TEST COMPLETE ===');
  });

  it('should test for metadata duplication with concurrent updates', async () => {
    console.log('=== TESTING CONCURRENT METADATA UPDATES ===');
    
    // Test both org and markdown files concurrently
    const orgNode = await TestCleanup.createTestNode({
      title: 'Concurrent Org Test',
      content: 'Concurrent test content',
      tags: ['concurrent', 'org'],
      file_type: 'org'
    });
    
    const mdNode = await TestCleanup.createTestNode({
      title: 'Concurrent MD Test',
      content: 'Concurrent test content',
      tags: ['concurrent', 'md'],
      file_type: 'md'
    });
    
    console.log('Created org node:', orgNode.id);
    console.log('Created md node:', mdNode.id);
    
    // Perform concurrent updates
    console.log('Performing concurrent updates...');
    const concurrentUpdates = [
      ApiHelpers.updateNode(orgNode.id, { 
        title: 'Concurrent Org Update 1', 
        content: 'Updated org content 1',
        category: 'concurrent-1' 
      }),
      ApiHelpers.updateNode(mdNode.id, { 
        title: 'Concurrent MD Update 1', 
        content: 'Updated md content 1',
        category: 'concurrent-1' 
      }),
      ApiHelpers.updateNode(orgNode.id, { 
        title: 'Concurrent Org Update 2', 
        content: 'Updated org content 2',
        category: 'concurrent-2' 
      }),
      ApiHelpers.updateNode(mdNode.id, { 
        title: 'Concurrent MD Update 2', 
        content: 'Updated md content 2',
        category: 'concurrent-2' 
      })
    ];
    
    const responses = await Promise.allSettled(concurrentUpdates);
    console.log('Concurrent update results:');
    responses.forEach((response, index) => {
      if (response.status === 'fulfilled') {
        console.log(`Update ${index + 1} succeeded:`, response.value.status);
      } else {
        console.log(`Update ${index + 1} failed:`, response.reason);
      }
    });
    
    // Check both files for metadata duplication
    const orgContent = await ApiHelpers.getNodeContent(orgNode.id);
    const mdContent = await ApiHelpers.getNodeContent(mdNode.id);
    
    if (orgContent.status === 200) {
      const content = orgContent.body.content || '';
      const propertiesMatches = content.match(/:PROPERTIES:/g);
      const titleMatches = content.match(/^#\+title:/gm);
      
      console.log('After concurrent updates - Org PROPERTIES count:', propertiesMatches ? propertiesMatches.length : 0);
      console.log('After concurrent updates - Org #+title count:', titleMatches ? titleMatches.length : 0);
      
      expect(propertiesMatches ? propertiesMatches.length : 0).toBe(1);
      expect(titleMatches ? titleMatches.length : 0).toBe(1);
    }
    
    if (mdContent.status === 200) {
      const content = mdContent.body.content || '';
      const yamlStartMatches = content.match(/^---$/gm);
      const titleMatches = content.match(/^title:/gm);
      
      console.log('After concurrent updates - MD YAML delimiters count:', yamlStartMatches ? yamlStartMatches.length : 0);
      console.log('After concurrent updates - MD title: count:', titleMatches ? titleMatches.length : 0);
      
      expect(yamlStartMatches ? yamlStartMatches.length : 0).toBe(2);
      expect(titleMatches ? titleMatches.length : 0).toBe(1);
    }
    
    // Cleanup
    await ApiHelpers.deleteNode(orgNode.id);
    await ApiHelpers.deleteNode(mdNode.id);
    
    console.log('=== CONCURRENT METADATA UPDATES TEST COMPLETE ===');
  });

  it('should test for partial update metadata integrity', async () => {
    console.log('=== TESTING PARTIAL UPDATE METADATA INTEGRITY ===');
    
    // Create node with full metadata
    const testNode = await TestCleanup.createTestNode({
      title: 'Full Metadata Test',
      content: 'Full metadata content',
      tags: ['tag1', 'tag2', 'tag3'],
      aliases: ['alias1', 'alias2'],
      refs: ['https://ref1.com', 'https://ref2.com'],
      category: 'full-test',
      file_type: 'md'
    });
    
    console.log('Created node with full metadata:', testNode.id);
    
    // Perform partial updates (only updating title and content)
    const partialUpdate = {
      title: 'Partially Updated Title',
      content: 'Partially updated content'
      // Intentionally omitting tags, aliases, refs, category
    };
    
    const updateResponse = await ApiHelpers.updateNode(testNode.id, partialUpdate);
    expect(updateResponse.status).toBe(200);
    
    // Check that metadata wasn't duplicated and existing metadata is preserved
    const updatedContent = await ApiHelpers.getNodeContent(testNode.id);
    if (updatedContent.status === 200) {
      const content = updatedContent.body.content || '';
      console.log('After partial update - content preview:', content.substring(0, 600));
      
      // Check for duplication
      const yamlStartMatches = content.match(/^---$/gm);
      const titleMatches = content.match(/^title:/gm);
      const idMatches = content.match(/^id:/gm);
      
      console.log('After partial update - YAML delimiters:', yamlStartMatches ? yamlStartMatches.length : 0);
      console.log('After partial update - title lines:', titleMatches ? titleMatches.length : 0);
      console.log('After partial update - id lines:', idMatches ? idMatches.length : 0);
      
      expect(yamlStartMatches ? yamlStartMatches.length : 0).toBe(2);
      expect(titleMatches ? titleMatches.length : 0).toBe(1);
      expect(idMatches ? idMatches.length : 0).toBe(1);
      
      // Verify title was updated
      expect(content).toContain('title: Partially Updated Title');
      expect(content).toContain('Partially updated content');
    }
    
    // Cleanup
    await ApiHelpers.deleteNode(testNode.id);
    
    console.log('=== PARTIAL UPDATE METADATA INTEGRITY TEST COMPLETE ===');
  });
});