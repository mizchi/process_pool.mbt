/// ジョブの定義
pub(all) struct Job {
  cmd : String
  args : Array[String]
  cwd : String?
  timeout : Int?
}

/// ジョブの実行結果
pub(all) struct JobResult {
  job : Job
  exit_code : Int
  stdout : String
  stderr : String
  timed_out : Bool
}

/// ジョブ実行時のエラー
pub(all) suberror JobError {
  ProcessFailed(job~ : Job, exit_code~ : Int, stdout~ : String, stderr~ : String)
  ProcessTimedOut(job~ : Job, timeout~ : Int, stdout~ : String, stderr~ : String)
}

/// ジョブ完了時のコールバック
pub(all) struct OnJobComplete {
  callback : (JobResult) -> Unit
}

/// OnJobComplete を作成
pub fn OnJobComplete::new(callback : (JobResult) -> Unit) -> OnJobComplete {
  OnJobComplete::{ callback }
}

/// コールバックを実行
pub fn OnJobComplete::call(self : OnJobComplete, result : JobResult) -> Unit {
  (self.callback)(result)
}

/// ジョブを作成するヘルパー関数
pub fn job(
  cmd : String,
  args : Array[String],
  cwd? : String,
  timeout? : Int,
) -> Job {
  Job::{ cmd, args, cwd, timeout }
}
