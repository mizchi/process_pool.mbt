// proc_pool: Moonbit プロセスプールライブラリ
//
// 複数のプロセスを並列実行するタスクランナー
//
// 設計方針:
// - Semaphore で同時実行数を制限
// - TaskGroup で構造化コンカレンシを実現
// - @process.run で子プロセスを実行
//
// 使用例:
//   let pool = @proc_pool.ProcessPool::new(max_workers=4)
//   let results = pool.run_all([
//     @proc_pool.job("echo", ["hello"]),
//     @proc_pool.job("echo", ["world"]),
//   ])
//
// 対応プラットフォーム:
// - native: フル機能
// - js: Node.js 環境でフル機能
// - wasm/wasm-gc: 未実装 (abort)

/// ジョブの定義
pub struct Job {
  cmd : String
  args : Array[String]
  cwd : String?
  timeout : Int? // タイムアウト（ミリ秒）
}

/// ジョブの実行結果
pub struct JobResult {
  job : Job
  exit_code : Int
  stdout : String
  stderr : String
  timed_out : Bool // タイムアウトしたかどうか
}

/// ジョブ実行時のエラー
pub suberror JobError {
  ProcessFailed(job~ : Job, exit_code~ : Int, stdout~ : String, stderr~ : String)
  ProcessTimedOut(job~ : Job, timeout~ : Int, stdout~ : String, stderr~ : String)
}

/// ジョブ完了時のコールバック
pub(all) struct OnJobComplete((JobResult) -> Unit)

/// ジョブを作成するヘルパー関数
///
/// Parameters:
/// - `cmd`: 実行するコマンド
/// - `args`: コマンドの引数
/// - `cwd`: 作業ディレクトリ（オプション）
/// - `timeout`: タイムアウト時間（ミリ秒、オプション）
pub fn job(
  cmd : String,
  args : Array[String],
  cwd? : String,
  timeout? : Int,
) -> Job {
  Job::{ cmd, args, cwd, timeout }
}

/// 複数のジョブを並列実行し、エラーがあれば例外を発生させる
///
/// いずれかのジョブが失敗（exit_code != 0）またはタイムアウトした場合、
/// JobError を raise する。
pub async fn ProcessPool::run_all_checked(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  let results = self.run_all(jobs)
  for result in results {
    if result.timed_out {
      raise JobError::ProcessTimedOut(
        job=result.job,
        timeout=result.job.timeout.unwrap_or(0),
        stdout=result.stdout,
        stderr=result.stderr,
      )
    }
    if result.exit_code != 0 {
      raise JobError::ProcessFailed(
        job=result.job,
        exit_code=result.exit_code,
        stdout=result.stdout,
        stderr=result.stderr,
      )
    }
  }
  results
}

/// 入力データを変換してジョブを作成し、並列実行する
///
/// SSG などのユースケースで、ファイルリストなどから
/// ジョブを生成して並列実行する場合に便利。
pub async fn[T] ProcessPool::map(
  self : ProcessPool,
  items : Array[T],
  to_job : (T) -> Job,
) -> Array[JobResult] {
  let jobs = items.map(to_job)
  self.run_all(jobs)
}

