// Default implementation (stub - should be overridden)

/// 最小限のテスト用関数（デフォルト実装）
pub fn hello() -> String {
  "hello from default"
}

/// ProcessPool（デフォルト: ダミー実装）
pub struct ProcessPool {
  max_workers : Int
}

/// ProcessPool を作成（デフォルト実装）
pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool {
  ProcessPool::{ max_workers }
}

/// 複数のジョブを並列実行（デフォルト: 未実装）
pub async fn run_all(
  pool : ProcessPool,
  jobs : Array[@types.Job],
) -> Array[@types.JobResult] {
  ignore(pool)
  ignore(jobs)
  abort("run_all is not implemented. Please use an implementation package.")
}
