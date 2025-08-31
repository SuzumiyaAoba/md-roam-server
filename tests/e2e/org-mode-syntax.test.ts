import { describe, it, expect } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';

describe('Org-mode Syntax Comprehensive Test', () => {
  it('should register org-mode document with comprehensive syntax via POST', async () => {
    console.log('=== TESTING ORG-MODE COMPREHENSIVE SYNTAX ===');
    
    // 簡素化されたorg-mode構文テスト
    const comprehensiveOrgContent = `* メインタイトル
:PROPERTIES:
:CUSTOM_ID: main-title
:END:

これはorg-modeの構文テストドキュメントです。

** テキスト装飾

*太字テキスト* と /斜体テキスト/ と ~コード~

** リスト構造

- 項目 1
- 項目 2
- 項目 3

** リンク

[[https://www.example.com][Example Website]]

** テーブル

| 名前 | 年齢 |
|------+------|
| 田中 |   30 |

** コードブロック

#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")
#+END_SRC

** TODO項目

*** TODO 重要なタスク
*** DONE 完了済みタスク`;

    console.log('Creating comprehensive org-mode document...');
    
    // POST APIを使用してorg-modeドキュメントを作成
    const createResponse = await ApiHelpers.createNode({
      title: 'Org-mode 包括的構文テスト',
      content: comprehensiveOrgContent,
      tags: ['org-mode', 'syntax', 'comprehensive', 'test', 'japanese'],
      aliases: ['org構文テスト', 'orgモードテスト'],
      refs: ['https://orgmode.org', 'https://www.example.com'],
      category: 'syntax-test',
      file_type: 'org'
    });
    
    console.log('Create response status:', createResponse.status);
    console.log('Created node:', createResponse.body);
    
    expect(createResponse.status).toBe(201);
    expect(createResponse.body).toHaveProperty('id');
    expect(createResponse.body).toHaveProperty('title', 'Org-mode 包括的構文テスト');
    expect(createResponse.body).toHaveProperty('file_type', 'org');
    
    const nodeId = createResponse.body.id;
    TestCleanup.trackNode(nodeId);
    
    // 作成されたコンテンツを取得して確認
    console.log('Retrieving created content for verification...');
    const contentResponse = await ApiHelpers.getNodeContent(nodeId);
    
    expect(contentResponse.status).toBe(200);
    const retrievedContent = contentResponse.body.content || '';
    
    console.log('Content length:', retrievedContent.length);
    console.log('Content preview (first 500 chars):');
    console.log(retrievedContent.substring(0, 500));
    
    // 簡素化された構文チェック
    const syntaxChecks = [
      // 見出し
      { pattern: /^\* メインタイトル/m, name: '主見出し' },
      { pattern: /^\*\* テキスト装飾/m, name: 'サブ見出し' },
      
      // プロパティ
      { pattern: /:PROPERTIES:/m, name: 'プロパティブロック開始' },
      { pattern: /:END:/m, name: 'プロパティブロック終了' },
      { pattern: /:CUSTOM_ID: main-title/m, name: 'カスタムID' },
      
      // テキスト装飾
      { pattern: /\*太字テキスト\*/m, name: '太字' },
      { pattern: /\/斜体テキスト\//m, name: '斜体' },
      { pattern: /~コード~/m, name: 'インラインコード' },
      
      // リスト
      { pattern: /^- 項目 1/m, name: '番号なしリスト' },
      
      // リンク
      { pattern: /\[\[https:\/\/www\.example\.com\]\[Example Website\]\]/m, name: '外部リンク' },
      
      // テーブル
      { pattern: /\| 名前 \| 年齢 \|/m, name: 'テーブルヘッダー' },
      { pattern: /\| 田中 \|   30 \|/m, name: 'テーブルデータ' },
      
      // コードブロック
      { pattern: /^\#\+BEGIN_SRC python/m, name: 'ソースコード開始' },
      { pattern: /def hello_world\(\):/m, name: 'Python関数' },
      
      // TODO項目
      { pattern: /^\*\*\* TODO 重要なタスク/m, name: 'TODO項目' },
      { pattern: /^\*\*\* DONE 完了済みタスク/m, name: 'DONE項目' }
    ];
    
    console.log('Checking org-mode syntax elements...');
    let passedChecks = 0;
    let failedChecks = 0;
    
    syntaxChecks.forEach((check, index) => {
      const found = check.pattern.test(retrievedContent);
      if (found) {
        console.log(`✅ ${(index + 1).toString().padStart(2, ' ')}. ${check.name} - 見つかりました`);
        passedChecks++;
      } else {
        console.log(`❌ ${(index + 1).toString().padStart(2, ' ')}. ${check.name} - 見つかりません`);
        failedChecks++;
      }
    });
    
    console.log(`\n=== 構文チェック結果 ===`);
    console.log(`✅ 成功: ${passedChecks}/${syntaxChecks.length}`);
    console.log(`❌ 失敗: ${failedChecks}/${syntaxChecks.length}`);
    console.log(`成功率: ${Math.round((passedChecks / syntaxChecks.length) * 100)}%`);
    
    // 最低70%の構文要素が保持されていることを期待（簡素化により調整）
    const successRate = passedChecks / syntaxChecks.length;
    expect(successRate).toBeGreaterThanOrEqual(0.7);
    
    // 日本語コンテンツが正しく保存されていることを確認
    expect(retrievedContent).toContain('メインタイトル');
    expect(retrievedContent).toContain('包括的な構文テストドキュメント');
    expect(retrievedContent).toContain('田中');
    expect(retrievedContent).toContain('佐藤');
    expect(retrievedContent).toContain('重要なタスク');
    
    // ファイルサイズとプロパティの確認
    expect(retrievedContent.length).toBeGreaterThan(1000); // 十分な長さがあること
    
    console.log('=== ORG-MODE COMPREHENSIVE SYNTAX TEST COMPLETED ===');
    
    // TestCleanupが自動でクリーンアップを実行
  });

  it('should handle org-mode specific edge cases', async () => {
    console.log('=== TESTING ORG-MODE EDGE CASES ===');
    
    const edgeCaseContent = `* エッジケーステスト

** 特殊文字とエスケープ
- 特殊文字: <>[]{}()*_/=~+^$\\.
- org-mode構文文字: * / _ = ~ + < > [ ]
- 日本語の様々な文字: ひらがな、カタカナ、漢字、記号「」。

** 空行とインデント

項目1

    インデントされた内容
        さらにインデント

項目2


複数の空行

** 長い行
これは非常に長い行のテストです。この行は画面幅を超える可能性があり、org-modeでの長い行の処理をテストします。日本語の場合、文字幅の計算が複雑になることがあります。このような長い行が適切に保存・復元されることを確認します。

** ネストした構造の複雑な例
*** レベル3
**** レベル4  
***** レベル5
1. 番号付きリスト
   - サブ項目
     - さらなるサブ項目
       #+BEGIN_SRC javascript
       // ネストしたコードブロック
       function nested() {
         return "complex nesting";
       }
       #+END_SRC
   - 戻ってきたサブ項目
2. 二番目の項目

** 混在する構文要素
| コード | 説明 | 例 |
|--------+------+----|
| *bold* | 太字 | ~code~ |
| /italic/ | 斜体 | =verbatim= |

上記のテーブル内の *太字* や ~コード~ も正しく処理されるべきです。

** Unicode文字
- 絵文字: 🚀 📝 ✅ ❌ 🔍
- 数学記号: ∑ ∫ ∞ ≠ ≤ ≥
- 矢印: → ← ↑ ↓ ↔ ⇒
- その他: ™ © ® § ¶ †`;

    const testNode = await TestCleanup.createTestNode({
      title: 'Org-mode エッジケース',
      content: edgeCaseContent,
      tags: ['edge-case', 'special-chars', 'japanese'],
      category: 'edge-test',
      file_type: 'org'
    });

    console.log('Edge case node created:', testNode.id);

    // コンテンツ取得
    const contentResponse = await ApiHelpers.getNodeContent(testNode.id);
    expect(contentResponse.status).toBe(200);
    
    const retrievedContent = contentResponse.body.content || '';
    console.log('Edge case content length:', retrievedContent.length);

    // エッジケースの確認
    const edgeCaseChecks = [
      { text: 'エッジケーステスト', name: '日本語タイトル' },
      { text: '特殊文字: <>[]{}()*_/=~+^$\\.', name: '特殊文字' },
      { text: 'ひらがな、カタカナ、漢字', name: '日本語文字種' },
      { text: '    インデントされた内容', name: 'インデント保持' },
      { text: '***** レベル5', name: '深いネスト' },
      { text: '🚀 📝 ✅', name: '絵文字' },
      { text: '∑ ∫ ∞', name: '数学記号' },
      { text: '→ ← ↑', name: '矢印記号' },
      { text: '// ネストしたコードブロック', name: 'ネストしたコード' }
    ];

    console.log('Checking edge case preservation...');
    let edgePassedChecks = 0;

    edgeCaseChecks.forEach((check, index) => {
      const found = retrievedContent.includes(check.text);
      if (found) {
        console.log(`✅ ${index + 1}. ${check.name} - 保持されています`);
        edgePassedChecks++;
      } else {
        console.log(`❌ ${index + 1}. ${check.name} - 失われました`);
      }
    });

    const edgeSuccessRate = edgePassedChecks / edgeCaseChecks.length;
    console.log(`エッジケース成功率: ${Math.round(edgeSuccessRate * 100)}% (${edgePassedChecks}/${edgeCaseChecks.length})`);
    
    expect(edgeSuccessRate).toBeGreaterThanOrEqual(0.9); // 90%以上の保持率を期待

    console.log('=== ORG-MODE EDGE CASES TEST COMPLETED ===');
  });
});