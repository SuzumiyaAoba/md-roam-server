import { describe, it, expect } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';

describe('Org-mode Syntax POST Test', () => {
  it('should register org-mode document with various syntax elements via POST', async () => {
    console.log('=== TESTING ORG-MODE SYNTAX VIA POST ===');
    
    // org-modeの主要な構文を含む文書
    const orgContent = `* メインタイトル
:PROPERTIES:
:CUSTOM_ID: main-title
:CREATED: [2025-08-30 Fri]
:END:

これはorg-mode構文テストドキュメントです。

** 見出し構造

*** サブ見出し 1
**** さらなるサブ見出し

** テキスト装飾

*太字テキスト* と /斜体テキスト/ と _下線テキスト_ と =等幅フォント= と ~コード~ と +取り消し線+

** リスト構造

*** 番号なしリスト
- 項目 1
- 項目 2
  - サブ項目 2.1
  - サブ項目 2.2
- 項目 3

*** 番号付きリスト  
1. 最初の項目
2. 二番目の項目
   1. サブ項目 A
   2. サブ項目 B
3. 三番目の項目

*** チェックボックス付きリスト
- [ ] 未完了タスク 1
- [X] 完了済みタスク
- [-] 部分完了タスク

** リンクとURL

- [[https://www.example.com][Example Website]]
- [[https://github.com][GitHub]]
- 直接URL: https://www.google.com

** テーブル

| 名前 | 年齢 | 職業 |
|------+------+------|
| 田中 |   30 | 開発者 |
| 佐藤 |   25 | 設計者 |

** コードブロック

#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")
    return "org-mode test"
#+END_SRC

** 引用ブロック

#+BEGIN_QUOTE
これは引用ブロックの内容です。
複数行にわたって記述することができます。
#+END_QUOTE

** TODO項目

*** TODO 重要なタスク
DEADLINE: <2025-09-01 Sun>
*** DONE 完了済みタスク  
CLOSED: [2025-08-29 Thu 14:30]

** 脚注

これは脚注の例です[fn:1]。

[fn:1] これは脚注の内容です。

** コメント

# これは行コメントです

** 区切り線

上の内容
-----
下の内容`;

    console.log('Creating org-mode document via POST...');
    
    // POST APIを使用してorg-modeドキュメントを作成
    const createResponse = await ApiHelpers.createNode({
      title: 'Org-mode 構文テスト',
      content: orgContent,
      tags: ['org-mode', 'syntax', 'test', 'japanese'],
      aliases: ['org構文テスト'],
      refs: ['https://orgmode.org'],
      category: 'syntax-test',
      file_type: 'org'
    });
    
    console.log('Create response status:', createResponse.status);
    console.log('Created node:', createResponse.body);
    
    expect(createResponse.status).toBe(201);
    expect(createResponse.body).toHaveProperty('id');
    expect(createResponse.body).toHaveProperty('title', 'Org-mode 構文テスト');
    expect(createResponse.body).toHaveProperty('file_type', 'org');
    
    const nodeId = createResponse.body.id;
    TestCleanup.trackNode(nodeId);
    
    // 作成されたコンテンツを取得して確認
    console.log('Retrieving created content for verification...');
    const contentResponse = await ApiHelpers.getNodeContent(nodeId);
    
    expect(contentResponse.status).toBe(200);
    const retrievedContent = contentResponse.body.content || '';
    
    console.log('Content length:', retrievedContent.length);
    console.log('Content preview (first 300 chars):');
    console.log(retrievedContent.substring(0, 300));
    
    // org-modeの主要構文要素が含まれていることを確認
    const syntaxChecks = [
      // 見出し
      { pattern: /^\* メインタイトル/m, name: '主見出し' },
      { pattern: /^\*\* 見出し構造/m, name: 'サブ見出し' },
      { pattern: /^\*\*\* サブ見出し 1/m, name: '三次見出し' },
      
      // プロパティ
      { pattern: /:PROPERTIES:/m, name: 'プロパティブロック' },
      { pattern: /:CUSTOM_ID: main-title/m, name: 'カスタムID' },
      
      // テキスト装飾
      { pattern: /\*太字テキスト\*/m, name: '太字' },
      { pattern: /\/斜体テキスト\//m, name: '斜体' },
      { pattern: /~コード~/m, name: 'インラインコード' },
      
      // リスト
      { pattern: /^- 項目 1/m, name: '番号なしリスト' },
      { pattern: /^1\. 最初の項目/m, name: '番号付きリスト' },
      { pattern: /^- \[ \] 未完了タスク 1/m, name: 'チェックボックス' },
      { pattern: /^- \[X\] 完了済みタスク/m, name: '完了チェックボックス' },
      
      // リンク
      { pattern: /\[\[https:\/\/www\.example\.com\]\[Example Website\]\]/m, name: '外部リンク' },
      
      // テーブル
      { pattern: /\| 名前 \| 年齢 \| 職業 \|/m, name: 'テーブル' },
      { pattern: /\| 田中 \|   30 \| 開発者 \|/m, name: 'テーブルデータ' },
      
      // コードブロック
      { pattern: /^\#\+BEGIN_SRC python/m, name: 'ソースコードブロック' },
      { pattern: /def hello_world\(\):/m, name: 'Python関数' },
      
      // 引用ブロック
      { pattern: /^\#\+BEGIN_QUOTE/m, name: '引用ブロック' },
      
      // TODO項目
      { pattern: /^\*\*\* TODO 重要なタスク/m, name: 'TODO項目' },
      { pattern: /^\*\*\* DONE 完了済みタスク/m, name: 'DONE項目' },
      
      // 脚注
      { pattern: /これは脚注の例です\[fn:1\]/m, name: '脚注参照' },
      { pattern: /\[fn:1\] これは脚注の内容です。/m, name: '脚注定義' },
      
      // コメントと区切り線
      { pattern: /^# これは行コメントです/m, name: '行コメント' },
      { pattern: /^-----$/m, name: '区切り線' }
    ];
    
    console.log('Checking org-mode syntax elements...');
    let passedChecks = 0;
    let failedChecks = 0;
    
    syntaxChecks.forEach((check, index) => {
      const found = check.pattern.test(retrievedContent);
      if (found) {
        console.log(`✅ ${(index + 1).toString().padStart(2, ' ')}. ${check.name}`);
        passedChecks++;
      } else {
        console.log(`❌ ${(index + 1).toString().padStart(2, ' ')}. ${check.name}`);
        failedChecks++;
      }
    });
    
    console.log(`\n=== 構文チェック結果 ===`);
    console.log(`✅ 成功: ${passedChecks}/${syntaxChecks.length}`);
    console.log(`❌ 失敗: ${failedChecks}/${syntaxChecks.length}`);
    console.log(`成功率: ${Math.round((passedChecks / syntaxChecks.length) * 100)}%`);
    
    // 最低80%の構文要素が保持されていることを期待
    const successRate = passedChecks / syntaxChecks.length;
    expect(successRate).toBeGreaterThanOrEqual(0.8);
    
    // 日本語コンテンツが正しく保存されていることを確認
    expect(retrievedContent).toContain('メインタイトル');
    expect(retrievedContent).toContain('org-mode構文テストドキュメント');
    expect(retrievedContent).toContain('田中');
    expect(retrievedContent).toContain('重要なタスク');
    
    console.log('=== ORG-MODE SYNTAX POST TEST COMPLETED ===');
  }, 30000); // 30秒のタイムアウト

  it('should handle Japanese characters and special symbols', async () => {
    console.log('=== TESTING JAPANESE AND SPECIAL CHARACTERS ===');
    
    const japaneseContent = `* 日本語テスト

** ひらがな・カタカナ・漢字
- ひらがな: あいうえお かきくけこ さしすせそ
- カタカナ: アイウエオ カキクケコ サシスセソ
- 漢字: 日本語 文字 処理 機能 確認

** 特殊記号・句読点
- 句読点: 、。「」『』（）
- 記号: ・〜※→←↑↓
- 数学記号: ±×÷≠≤≥∞

** 絵文字
- 基本: 😀 😊 😂 🤔 😎
- 作業: 💻 📝 📊 🔍 ⚡
- 状態: ✅ ❌ ⚠️ 📌 🚀

** 長い日本語文
これは日本語の長い文章のテストです。句読点や助詞、敬語などが含まれた自然な日本語文章が正しく保存・復元されることを確認します。また、改行や段落分けも適切に処理されるかテストします。

改行後の段落です。`;

    const testNode = await TestCleanup.createTestNode({
      title: '日本語・特殊文字テスト',
      content: japaneseContent,
      tags: ['japanese', 'special-chars', 'unicode'],
      category: 'japanese-test',
      file_type: 'org'
    });

    console.log('Japanese content node created:', testNode.id);

    // コンテンツ取得
    const contentResponse = await ApiHelpers.getNodeContent(testNode.id);
    expect(contentResponse.status).toBe(200);
    
    const retrievedContent = contentResponse.body.content || '';
    console.log('Japanese content length:', retrievedContent.length);

    // 日本語と特殊文字の確認
    const japaneseChecks = [
      { text: '日本語テスト', name: '日本語タイトル' },
      { text: 'あいうえお かきくけこ', name: 'ひらがな' },
      { text: 'アイウエオ カキクケコ', name: 'カタカナ' },
      { text: '日本語 文字 処理', name: '漢字' },
      { text: '、。「」『』', name: '句読点' },
      { text: '・〜※→←', name: '特殊記号' },
      { text: '±×÷≠≤≥', name: '数学記号' },
      { text: '😀 😊 😂', name: '絵文字' },
      { text: '💻 📝 📊', name: '作業絵文字' },
      { text: 'これは日本語の長い文章', name: '長い日本語文' }
    ];

    console.log('Checking Japanese character preservation...');
    let japanesePassedChecks = 0;

    japaneseChecks.forEach((check, index) => {
      const found = retrievedContent.includes(check.text);
      if (found) {
        console.log(`✅ ${index + 1}. ${check.name} - 保持されています`);
        japanesePassedChecks++;
      } else {
        console.log(`❌ ${index + 1}. ${check.name} - 失われました`);
      }
    });

    const japaneseSuccessRate = japanesePassedChecks / japaneseChecks.length;
    console.log(`日本語・特殊文字成功率: ${Math.round(japaneseSuccessRate * 100)}% (${japanesePassedChecks}/${japaneseChecks.length})`);
    
    expect(japaneseSuccessRate).toBeGreaterThanOrEqual(0.9); // 90%以上の保持率を期待

    console.log('=== JAPANESE AND SPECIAL CHARACTERS TEST COMPLETED ===');
  }, 15000); // 15秒のタイムアウト
});