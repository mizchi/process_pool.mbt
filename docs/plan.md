了解、ここから先は「コーディングエージェントが GitHub を読みながら実装していく」前提で、やりたいことと次に調査／実装することを整理したドキュメントを書きます。

そのまま README 的に使えるように Markdown でまとめます。

---

# Moonbit SSG 並列化計画メモ

## 1. やりたいこと（ゴール）

### 1-1. 技術的ゴール

* Moonbit で書かれた Markdown→HTML SSG のビルドを **マルチコア並列化**したい。
* 目標は「ページ数が増えてもビルド時間が線形に伸びすぎない」こと。
* Moonbit 本体（VM や言語ランタイム）を拡張せず、**ユーザーレベルの実装**で実現する。

### 1-2. モデルのイメージ

* プロセスモデルというより「**ページ単位のビルドジョブを並行実行するタスクシステム**」。
* 状態をべったり共有するのではなく、

  * 「親がジョブキューを持ち」
  * 「ワーカー（プロセス or Worker Thread）がジョブを取りに行き」
  * 「成功／失敗を親に返す」
    という **メッセージパッシングモデル**をベースにする。

### 1-3. JS との関係

* JS への依存は**最小限**にしたい。
* ただし現実路線として：

  * **JS backend + Node の `worker_threads`** で並列化する案
  * **ネイティブ backend + `@process` によるマルチプロセス実行**案
* この両方を検討し、最終的にどちらか（または両対応）を採用する。

---

## 2. 現状整理

### 2-1. SSG の現状

* Moonbit で Markdown → HTML の SSG を実装済み（シングルスレッド）。
* 想定フロー：

  1. `content/**/*.md` の列挙
  2. Markdown をファイル単位で読み込み
  3. パース／HTML 化（CPU heavy）
  4. テンプレート適用
  5. `dist/**/*.html` に書き出し
* ページ数増加に伴い、単純な逐次処理では CPU が詰まりビルド時間が長くなっている。

### 2-2. Moonbit の async / Maria から得た知見（要約）

* `moonbitlang/async` は **イベントループ + 構造化コンカレンシ + 各種プリミティブ**を提供する。

  * `with_task_group`, `spawn_bg`, `Semaphore`, `aqueue.Queue`, `sleep`, `retry`, `@process.*` など。
* Maria はこの `async` の上に構築された **非同期タスクシステム**＆エージェント実装。

  * 「タスク管理＋外部プロセス呼び出し＋IO」を Moonbit だけで頑張っている例として参考になる。
* ただし `async` はシングルスレッドのイベントループなので、

  * **「1 プロセス内で CPU をガチ並列実行」**はできない。
  * 真の並列化には「マルチプロセス or 複数 OS スレッド」が必要。

---

## 3. 設計方針（仮）

### 3-1. コア方針

1. **ページ単位の「レンダリング関数」を Moonbit 側に切り出す**

   * 例：`render_page(job: RenderJob) -> Result[Unit, Error]`
   * 入力: Markdown パス、出力パス、共通設定など。
   * SSG の本質ロジックはできるだけ Moonbit 内に閉じ込める。

2. **ビルドを orchestrate する「タスクシステム」を作る**

   * ジョブキュー（配列 or `aqueue.Queue`）
   * Worker（Moonbit タスク or 子プロセス or worker_thread）
   * 成功／失敗の集約

3. **並列化の戦略を2レイヤで用意しておく**

   * **レイヤA:単一プロセス＋async（シンプル版）**

     * IO が多い／エディタ連携用など、軽めな用途向け。
   * **レイヤB:マルチプロセス／Worker Threads（ガチビルド用）**

     * CPU バウンドな SSG のフルビルド向け。

### 3-2. 2つの具体的ルート

#### ルート1: Native backend ＋ `@process` （Moonbit だけで完結）

* Moonbit で以下を実装：

  * `ssg-worker` : 単一ページをビルドするサブコマンド。
  * `ssg-orchestrator` : ジョブリストを読み込み、`@process.run` / `collect_output_merged` で `ssg-worker` を最大 N 並列で起動。
* `Semaphore` や `TaskGroup` で同時プロセス数を制御。
* メリット：

  * JS にほぼ依存しない。
  * Maria と似た構造（`@process` で外部コマンド）なので、参考にしやすい。
* デメリット：

  * プロセス起動オーバーヘッドはある（ただし SSG なら概ね許容範囲）。

#### ルート2: JS backend ＋ Node `worker_threads`

* Moonbit を JS backend でビルドし、Node から以下を構成：

  * `moonbit/dist/ssg.js` 内に `renderFile(inputPath, outputPath)` のような関数を用意。
  * Node のメインスレッドがファイル一覧を作り、Worker のプールを作成。
  * 各 Worker が `renderFile` を呼んで結果を返す。
* メリット：

  * Node のエコシステム（glob, chokidar, etc.）を活用しやすい。
  * watch, dev server 等も JS 側で作りやすい。
* デメリット：

  * JS backend の async 状態など、Moonbit と JS の橋渡し部分を把握する必要がある。
  * 「JS 依存を減らしたい」というモチベとはやや逆方向。

---

## 4. 次に調査すること（Coding Agent 向けタスクリスト）

### 4-1. Moonbit async / Maria 関連

* [ ] `moonbitlang/async` の GitHub リポジトリを開き、以下を重点的に読む：

  * `src/async.mbt` : `with_task_group`, `spawn_bg`, `Semaphore`, `Queue` などの public API。
  * `@process` 系のモジュール：`run`, `collect_output_merged`, `spawn` 等のインターフェース。
* [ ] Maria の GitHub（`moonbit-maria`）を読み、

  * タスク管理やジョブキュー周り（Agent のメインループ、タスク登録、外部コマンド呼び出し）の実装を抽出。
  * 「タスクの再利用／キャンセル／エラー処理」のパターンをメモ。

### 4-2. SSG コアライブラリの API 設計

* [ ] 現行 SSG のコードベースを整理し、**1 ページレンダリングの最小 API** を決める：

  * 入力：`input_path: String`, `output_path: String`, `config: Config` など。
  * 出力：`Result[Unit, RenderError]`（warning を含めるなら別 struct）。
* [ ] `render_page` を Moonbit 内で完結する純粋関数群に分解：

  * `load_markdown(path) -> String`
  * `parse_markdown(src) -> Ast`
  * `render_ast(ast, ctx) -> String`
  * `write_html(path, html) -> Unit`
* [ ] この API がネイティブ／JS どちらの backend からも呼びやすい形になっているか確認。

### 4-3. ルート1（Native + @process）検証タスク

* [ ] Moonbit で `ssg-worker` サブコマンドを実装：

  * `ssg worker --input input.md --output dist/input.html` 形式、または JSON 引数で job を渡す方式。
* [ ] `@process.collect_output_merged` を用いて、Moonbit プログラムから自身の `ssg-worker` を呼び出すサンプル実装を書く。
* [ ] `Semaphore` + `with_task_group` で同時起動プロセス数（= コア数 or 任意値）を制限するコードを書く。
* [ ] 小さな Markdown セットで動作確認し、成功／失敗時の挙動（exit code, stderr）を確認。

### 4-4. ルート2（JS backend + worker_threads）検証タスク

* [ ] SSG コアを JS backend でビルドし、Node から import できるか確認。
* [ ] `renderFile(input, output)` を Node 側から呼ぶ最小サンプルを作成。
* [ ] `worker_threads` を使って簡単な「Job → Worker → Result」サイクルのサンプルを書く。
* [ ] 失敗したページのエラーを Worker からメインへ伝播させ、exit code を 1 にするフローを確認。

### 4-5. パフォーマンス・挙動確認

* [ ] 小～中規模のコンテンツセットで：

  * 逐次実行
  * ルート1（マルチプロセス）
  * ルート2（worker_threads）
    を比較し、ビルド時間と CPU 使用率を計測。
* [ ] ページ数が増えたときのスケーリング挙動と問題点（プロセス数制限、IO ボトルネックなど）を記録。

---

## 5. 実装時の注意点メモ

* **状態共有を欲張らない**：

  * SSG の性質上、ページ単位で独立していることが多い。
  * 「巨大なグローバル状態を共有する」より、「事前にメタ情報を JSON などに出しておき、各ジョブはそれを読む」方が安全でシンプル。

* **エラーとログの設計を最初から決める**：

  * 成功／失敗をどう表現するか（`Result` / exit code / JSON レスポンスなど）。
  * ログのフォーマット（job id, path, error message）を揃えておく。

* **後から UI / HTTP 化できるように**：

  * Maria のように「daemon + HTTP API」に発展させたい場合、

    * ビルドオーケストレータのコアロジックを純粋関数 or 独立モジュールとして切り出し、
    * CLI / HTTP / Node CLI などから共通利用できるようにしておくと拡張しやすい。

---

こんな感じで、

* 何をしたいか（SSG のマルチコア並列化）
* どんな設計方針か（タスクシステム＋ジョブ単位の並列）
* 何を読み・何を作るか（async/Maria/SSG コア API/@process/worker_threads）

を整理しておけば、コーディングエージェントに渡したときも迷いなく進めやすくなると思います。
