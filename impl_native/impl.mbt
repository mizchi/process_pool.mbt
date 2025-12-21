// Native (C backend) implementation

/// 最小限のテスト用関数（native 実装）
pub fn hello() -> String {
  "hello from native"
}

/// ProcessPool（native 実装）
pub struct ProcessPool {
  semaphore : @async.Semaphore
}

/// ProcessPool を作成（native 実装）
pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool {
  ProcessPool::{
    semaphore: @async.Semaphore::new(max_workers),
  }
}

/// 複数のジョブを並列実行
pub async fn run_all(
  pool : ProcessPool,
  jobs : Array[@types.Job],
) -> Array[@types.JobResult] {
  let decoder = @encoding.decoder(UTF8)

  @async.with_task_group(fn(group) {
    let tasks = jobs.map(fn(job) {
      group.spawn(fn() {
        pool.semaphore.acquire()
        defer pool.semaphore.release()

        match job.timeout {
          Some(timeout) => {
            let result = @async.with_timeout_opt(timeout, fn() {
              @process.collect_output(job.cmd, job.args, cwd?=job.cwd)
            })
            match result {
              Some((exit_code, stdout, stderr)) => {
                let stdout_str = decoder.decode_lossy(stdout.binary())
                let stderr_str = decoder.decode_lossy(stderr.binary())
                @types.JobResult::{ job, exit_code, stdout: stdout_str, stderr: stderr_str, timed_out: false }
              }
              None => {
                @types.JobResult::{ job, exit_code: -1, stdout: "", stderr: "Process timed out", timed_out: true }
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
            @types.JobResult::{ job, exit_code, stdout: stdout_str, stderr: stderr_str, timed_out: false }
          }
        }
      })
    })
    let results = []
    for task in tasks {
      results.push(task.wait())
    }
    results
  })
}
