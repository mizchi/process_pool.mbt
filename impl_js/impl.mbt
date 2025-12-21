// JavaScript implementation

/// 最小限のテスト用関数（js 実装）
pub fn hello() -> String {
  "hello from js"
}

/// ProcessPool（js 実装）
pub struct ProcessPool {
  max_workers : Int
}

/// ProcessPool を作成（js 実装）
pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool {
  ProcessPool::{ max_workers }
}

/// 複数のジョブを並列実行（js 実装 - スタブ）
pub async fn run_all(
  pool : ProcessPool,
  jobs : Array[@types.Job],
) -> Array[@types.JobResult] {
  // スタブ実装: 各ジョブに対してダミー結果を返す
  jobs.map(fn(job) {
    @types.JobResult::{
      job,
      exit_code: 0,
      stdout: "js stub",
      stderr: "",
      timed_out: false,
    }
  })
}
