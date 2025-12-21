# Virtual Package + Async 関数: リンカーエラーの問題

## 概要

**virtual package** の実装パッケージで **async 関数** を使用すると、moonc リンカーが `Key_not_found` エラーで失敗します。リンカーは async 状態マシン型を実装パッケージの名前空間（`impl_native.xxx.State`）ではなく、virtual package の名前空間（`lib.xxx.State`）で探してしまいます。

## 環境

- moonc version: v0.6.35+dd17327ed
- Target: native

## 問題の再現手順

### 1. プロジェクト構造

```
proc_pool/
├── lib/                    # Virtual package（インターフェース）
│   ├── moon.pkg.json
│   ├── pkg.mbti
│   └── lib.mbt
├── impl_native/            # 実装パッケージ
│   ├── moon.pkg.json
│   └── impl.mbt
├── test_native/            # テストパッケージ
│   ├── moon.pkg.json
│   └── test.mbt
└── types/                  # 共有型定義
    ├── moon.pkg.json
    └── types.mbt
```

### 2. Virtual Package の仕組み

Virtual package は以下の3つの要素で構成されます:

1. **pkg.mbti**: インターフェース定義（関数シグネチャ、型宣言）
2. **デフォルト実装**: `lib.mbt` に書かれたスタブ実装
3. **実装パッケージ**: `"implement": "..."` で virtual package を実装

使用側は `"overrides": [...]` で実装パッケージを指定し、デフォルト実装を置き換えます。

### 3. 動作するケース（非 async 関数）

```moonbit
// pkg.mbti
fn hello() -> String
fn new_pool(max_workers? : Int) -> ProcessPool
type ProcessPool

// impl_native/impl.mbt
pub fn hello() -> String { "hello from native" }
pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool { ... }
```

**結果**: テスト成功 ✅

### 4. 失敗するケース（async 関数）

```moonbit
// pkg.mbti に追加
async fn run_all(ProcessPool, Array[@types.Job]) -> Array[@types.JobResult]

// impl_native/impl.mbt に追加
pub async fn run_all(pool : ProcessPool, jobs : Array[@types.Job]) -> Array[@types.JobResult] {
  @async.with_task_group(fn(group) {
    // async 処理
  })
}
```

**結果**: リンカーエラー ❌

## エラーメッセージ

```
Error: Moonc.Basic_hash_string.Key_not_found("$mizchi/proc_pool/lib.run_all.lambda/60.State")
```

## 原因の分析

MoonBit の async 関数は、コンパイル時に状態マシン（State Machine）に変換されます。この状態マシンは `関数名.State` という型として生成されます。

### リンク時の問題

1. リンカーは `lib.run_all.lambda/60.State` を探す
2. 実際の状態マシンは `impl_native.run_all.lambda/60.State` に存在
3. 名前空間の不一致によりリンクに失敗

### なぜ非 async 関数は動くのか

非 async 関数は単純な関数シンボルとして扱われ、`implement` + `overrides` の仕組みで正しく解決されます。しかし async 関数は追加の状態マシン型を生成するため、この解決メカニズムがうまく機能しません。

## 試したこと

1. **内部関数を別名にする**: 効果なし（ラムダ式も同じ問題）
2. **すべてをインライン化**: 効果なし（ラムダ式の状態マシンで同じエラー）
3. **キャッシュクリア**: 効果なし

## 回避策

現時点で有効な回避策はありません。選択肢:

1. **条件付きコンパイル（#cfg）を使う**: virtual package を諦める
2. **sync 関数のみ使用**: async/並列実行を諦める

## 期待される動作

`overrides` が指定されている場合、リンカーは async 状態マシン型を実装パッケージの名前空間から解決すべきです。

## まとめ

Virtual package は関数インターフェースの抽象化には有効ですが、async 関数との組み合わせでコンパイラ/リンカーのバグ（または未サポート機能）があります。現状では:

- **単純な関数**: ✅ 動作する
- **型定義**: ✅ opaque type として動作する
- **async 関数**: ❌ リンクエラー

この問題が解決されれば、virtual package を使って native/js で異なる async 実装を持ちながら、同じテストを共有できるようになります。
