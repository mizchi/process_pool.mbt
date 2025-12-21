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

/// プロセスプール
pub struct ProcessPool {
  max_workers : Int
  semaphore : @async.Semaphore
  on_complete : OnJobComplete? // ジョブ完了時のコールバック
}

/// プロセスプールを作成
///
/// Parameters:
/// - `max_workers`: 同時実行するプロセスの最大数（デフォルト: 4）
/// - `on_complete`: ジョブ完了時に呼ばれるコールバック（オプション）
pub fn ProcessPool::new(
  max_workers~ : Int = 4,
  on_complete? : OnJobComplete,
) -> ProcessPool {
  ProcessPool::{
    max_workers,
    semaphore: @async.Semaphore::new(max_workers),
    on_complete,
  }
}

/// 単一のジョブを実行（内部関数）
async fn run_job(job : Job) -> JobResult {
  let decoder = @encoding.decoder(UTF8)

  match job.timeout {
    Some(timeout) => {
      let result = @async.with_timeout_opt(timeout, () => {
        @process.collect_output(job.cmd, job.args, cwd?=job.cwd)
      })
      match result {
        Some((exit_code, stdout, stderr)) => {
          let stdout_str = decoder.decode_lossy(stdout.binary())
          let stderr_str = decoder.decode_lossy(stderr.binary())
          JobResult::{ job, exit_code, stdout: stdout_str, stderr: stderr_str, timed_out: false }
        }
        None => {
          JobResult::{ job, exit_code: -1, stdout: "", stderr: "Process timed out", timed_out: true }
        }
      }
    }
    None => {
      let (exit_code, stdout, stderr) = @process.collect_output(
        job.cmd,
        job.args,
        cwd?=job.cwd,
      )
      let stdout_str = decoder.decode_lossy(stdout.binary())
      let stderr_str = decoder.decode_lossy(stderr.binary())
      JobResult::{ job, exit_code, stdout: stdout_str, stderr: stderr_str, timed_out: false }
    }
  }
}

/// Semaphore で同時実行数を制限しながらジョブを実行（内部関数）
async fn run_job_with_semaphore(
  semaphore : @async.Semaphore,
  job : Job,
  on_complete : OnJobComplete?,
) -> JobResult {
  semaphore.acquire()
  defer semaphore.release()
  let result = run_job(job)
  if on_complete is Some(callback) {
    callback.0(result)
  }
  result
}

/// 複数のジョブを並列実行し、すべての結果を返す
///
/// すべてのジョブが完了するまで待機し、結果の配列を返す。
/// ジョブの順序は入力と同じ順序で保持される。
pub async fn ProcessPool::run_all(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  @async.with_task_group(group => {
    let tasks = jobs.map(job => group.spawn(() =>
      run_job_with_semaphore(self.semaphore, job, self.on_complete)))
    let results = []
    for task in tasks {
      results.push(task.wait())
    }
    results
  })
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
