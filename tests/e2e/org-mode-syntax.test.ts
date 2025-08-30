import { describe, it, expect } from 'vitest';
import { ApiHelpers, TestCleanup } from '@/utils/apiHelpers';

describe('Org-mode Syntax Comprehensive Test', () => {
  it('should register org-mode document with comprehensive syntax via POST', async () => {
    console.log('=== TESTING ORG-MODE COMPREHENSIVE SYNTAX ===');
    
    // org-modeの一通りの構文を含む包括的な文書
    const comprehensiveOrgContent = `* メインタイトル
:PROPERTIES:
:CUSTOM_ID: main-title
:CREATED: [2025-08-30 Fri]
:END:

これはorg-modeの包括的な構文テストドキュメントです。

** 見出し構造のテスト

*** サブ見出し 1
**** さらなるサブ見出し
***** 最深レベルの見出し

** テキスト装飾

*太字テキスト* と /斜体テキスト/ と _下線テキスト_ と =等幅フォント= と ~コード~ と +取り消し線+

** リスト構造

*** 番号なしリスト
- 項目 1
- 項目 2
  - サブ項目 2.1
  - サブ項目 2.2
    - さらなるサブ項目
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
- [ ] 未完了タスク 2

** リンクとURL

*** 外部リンク
- [[https://www.example.com][Example Website]]
- [[https://github.com][GitHub]]
- 直接URL: https://www.google.com

*** 内部リンク
- [[#main-title][メインタイトルへのリンク]]
- file:~/Documents/test.org

** テーブル

| 名前 | 年齢 | 職業 |
|------+------+------|
| 田中 |   30 | 開発者 |
| 佐藤 |   25 | 設計者 |
| 鈴木 |   35 | 管理者 |

** コードブロック

*** インライン ~code~ と等幅 =monospace=

*** ソースコードブロック
#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")
    return "org-mode test"

# コメント
x = 42
y = "文字列"
#+END_SRC

*** 例示ブロック
#+BEGIN_EXAMPLE
これは例示ブロックです。
  インデントも保持されます。
  特殊文字: */_=~+ もそのまま表示
#+END_EXAMPLE

** 引用ブロック

#+BEGIN_QUOTE
これは引用ブロックの内容です。
複数行にわたって記述することができます。

  -- 著者名
#+END_QUOTE

** 日時とタイムスタンプ

*** アクティブなタイムスタンプ
<2025-08-30 Fri 20:00>

*** 非アクティブなタイムスタンプ  
[2025-08-30 Fri 15:30]

*** 日時範囲
<2025-08-30 Fri 09:00>--<2025-08-30 Fri 17:00>

** TODO項目とステータス

*** TODO アイテム
**** TODO 重要なタスク
DEADLINE: <2025-09-01 Sun>
**** DONE 完了済みタスク  
CLOSED: [2025-08-29 Thu 14:30]
**** WAITING 待機中のタスク
**** CANCELLED キャンセルされたタスク

** 優先度とタグ

*** TODO [#A] 高優先度タスク :urgent:important:
*** TODO [#B] 中優先度タスク :normal:
*** TODO [#C] 低優先度タスク :someday:maybe:

** 脚注

これは脚注の例です[fn:1]。別の脚注もあります[fn:note2]。

[fn:1] これは最初の脚注の内容です。

[fn:note2] これは名前付き脚注です。

** ドロワー

:PROPERTIES:
:PROJECT: org-mode-test
:EFFORT: 2:00
:COST: 100
:END:

:LOGBOOK:
- State "DONE" from "TODO" [2025-08-30 Fri 20:00]
:END:

** マクロとエクスポート指定

#+TITLE: Org-mode包括的構文テスト
#+AUTHOR: テスト作成者
#+DATE: 2025-08-30
#+LANGUAGE: ja
#+OPTIONS: toc:t num:t
#+TAGS: test(t) syntax(s) org-mode(o)
#+CATEGORY: testing

** 数式 (LaTeX)

インライン数式: \\(E = mc^2\\)

ブロック数式:
\\begin{equation}
\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}
\\end{equation}

** コメントと非表示テキスト

# これは行コメントです
#+COMMENT: これはブロックコメントです

** 特殊ブロック

#+BEGIN_CENTER
中央寄せテキスト
#+END_CENTER

#+BEGIN_VERSE
詩のような
  行分けと
    インデントを
保持するブロック
#+END_VERSE

** HTMLエクスポート用要素

#+HTML: <div class="custom-div">
特別なHTML要素
#+HTML: </div>

** 区切り線

上の内容
-----
下の内容

** 固定幅領域

: これは固定幅の行です
: インデントと空白が保持されます
:   複数行で使用可能

** キーワードとメタデータ

#+STARTUP: overview
#+SEQ_TODO: TODO NEXT | DONE
#+PRIORITIES: A C B

** 終了

以上でorg-modeの主要な構文要素のテストを完了します。`;

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
    
    // org-modeの主要構文要素が含まれていることを確認
    const syntaxChecks = [
      // 見出し
      { pattern: /^\* メインタイトル/m, name: '主見出し' },
      { pattern: /^\*\* 見出し構造のテスト/m, name: 'サブ見出し' },
      { pattern: /^\*\*\* サブ見出し 1/m, name: '三次見出し' },
      
      // プロパティ
      { pattern: /:PROPERTIES:/m, name: 'プロパティブロック開始' },
      { pattern: /:END:/m, name: 'プロパティブロック終了' },
      { pattern: /:CUSTOM_ID: main-title/m, name: 'カスタムID' },
      
      // テキスト装飾
      { pattern: /\*太字テキスト\*/m, name: '太字' },
      { pattern: /\/斜体テキスト\//m, name: '斜体' },
      { pattern: /_下線テキスト_/m, name: '下線' },
      { pattern: /=等幅フォント=/m, name: '等幅フォント' },
      { pattern: /~コード~/m, name: 'インラインコード' },
      { pattern: /\+取り消し線\+/m, name: '取り消し線' },
      
      // リスト
      { pattern: /^- 項目 1/m, name: '番号なしリスト' },
      { pattern: /^1\. 最初の項目/m, name: '番号付きリスト' },
      { pattern: /^- \[ \] 未完了タスク 1/m, name: 'チェックボックス未完了' },
      { pattern: /^- \[X\] 完了済みタスク/m, name: 'チェックボックス完了' },
      
      // リンク
      { pattern: /\[\[https:\/\/www\.example\.com\]\[Example Website\]\]/m, name: '外部リンク' },
      { pattern: /https:\/\/www\.google\.com/m, name: '直接URL' },
      
      // テーブル
      { pattern: /\| 名前 \| 年齢 \| 職業 \|/m, name: 'テーブルヘッダー' },
      { pattern: /\|------\+------\+------\|/m, name: 'テーブル区切り' },
      { pattern: /\| 田中 \|   30 \| 開発者 \|/m, name: 'テーブルデータ' },
      
      // コードブロック
      { pattern: /^\#\+BEGIN_SRC python/m, name: 'ソースコード開始' },
      { pattern: /def hello_world\(\):/m, name: 'Python関数' },
      { pattern: /^\#\+END_SRC/m, name: 'ソースコード終了' },
      
      // 例示ブロック
      { pattern: /^\#\+BEGIN_EXAMPLE/m, name: '例示ブロック開始' },
      { pattern: /特殊文字: \*\/_=~\+ もそのまま表示/m, name: '例示ブロック内容' },
      
      // 引用ブロック
      { pattern: /^\#\+BEGIN_QUOTE/m, name: '引用ブロック開始' },
      { pattern: /-- 著者名/m, name: '引用著者' },
      
      // タイムスタンプ
      { pattern: /<2025-08-30 Fri 20:00>/m, name: 'アクティブタイムスタンプ' },
      { pattern: /\[2025-08-30 Fri 15:30\]/m, name: '非アクティブタイムスタンプ' },
      
      // TODO項目
      { pattern: /^\*\*\*\* TODO 重要なタスク/m, name: 'TODO項目' },
      { pattern: /^\*\*\*\* DONE 完了済みタスク/m, name: 'DONE項目' },
      { pattern: /DEADLINE: <2025-09-01 Sun>/m, name: 'デッドライン' },
      
      // 優先度とタグ
      { pattern: /^\*\*\* TODO \[#A\] 高優先度タスク :urgent:important:/m, name: '優先度とタグ' },
      
      // 脚注
      { pattern: /これは脚注の例です\[fn:1\]/m, name: '脚注参照' },
      { pattern: /\[fn:1\] これは最初の脚注の内容です。/m, name: '脚注定義' },
      
      // 数式
      { pattern: /\\\\begin\{equation\}/m, name: 'LaTeX数式ブロック' },
      { pattern: /E = mc\^2/m, name: 'インライン数式' },
      
      // コメント
      { pattern: /^# これは行コメントです/m, name: '行コメント' },
      { pattern: /^\#\+COMMENT:/m, name: 'ブロックコメント' },
      
      // 特殊ブロック
      { pattern: /^\#\+BEGIN_CENTER/m, name: '中央寄せブロック' },
      { pattern: /^\#\+BEGIN_VERSE/m, name: '詩ブロック' },
      
      // HTMLエクスポート
      { pattern: /^\#\+HTML: <div class="custom-div">/m, name: 'HTMLエクスポート' },
      
      // 固定幅
      { pattern: /^: これは固定幅の行です/m, name: '固定幅テキスト' },
      
      // メタデータ
      { pattern: /^\#\+TITLE: Org-mode包括的構文テスト/m, name: 'タイトルメタデータ' },
      { pattern: /^\#\+AUTHOR: テスト作成者/m, name: '著者メタデータ' },
      { pattern: /^\#\+LANGUAGE: ja/m, name: '言語設定' },
      
      // 区切り線
      { pattern: /^-----$/m, name: '区切り線' }
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
    
    // 最低80%の構文要素が保持されていることを期待
    const successRate = passedChecks / syntaxChecks.length;
    expect(successRate).toBeGreaterThanOrEqual(0.8);
    
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