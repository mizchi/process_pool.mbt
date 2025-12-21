///|
async fn main {
  println("=== process_pool demo ===")
  println("")

  // === デモ1: 基本的な並列実行 ===
  println("[Demo 1] Basic parallel execution")
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let jobs = [
    @process_pool.job("echo", ["job 1"]),
    @process_pool.job("echo", ["job 2"]),
    @process_pool.job("echo", ["job 3"]),
    @process_pool.job("echo", ["job 4"]),
  ]
  println(
    "Running \{jobs.length()} jobs with max \{pool.max_workers} workers...",
  )
  let results = pool.run_all(jobs)
  for i, result in results {
    println(
      "  Job \{i}: exit=\{result.exit_code}, stdout=\{result.stdout.trim_space()}",
    )
  }
  println("")

  // === デモ2: 進捗報告コールバック ===
  println("[Demo 2] Progress callback")
  let completed = @ref.new(0)
  let pool_with_callback = @process_pool.ProcessPool::new(max_workers=2, on_complete=fn(
    result,
  ) {
    completed.val += 1
    println(
      "  [callback] Job completed: \{result.job.cmd} -> exit \{result.exit_code}",
    )
  })
  let jobs2 = [
    @process_pool.job("echo", ["hello"]),
    @process_pool.job("echo", ["world"]),
    @process_pool.job("echo", ["foo"]),
    @process_pool.job("echo", ["bar"]),
  ]
  ignore(pool_with_callback.run_all(jobs2))
  println("  Total completed: \{completed.val}")
  println("")

  // === デモ3: タイムアウト ===
  println("[Demo 3] Timeout handling")
  let pool3 = @process_pool.ProcessPool::new(max_workers=2)
  let jobs3 = [
    @process_pool.job("echo", ["fast"], timeout=1000),
    @process_pool.job("sleep", ["5"], timeout=100),
  ]
  let results3 = pool3.run_all(jobs3)
  for i, result in results3 {
    if result.timed_out {
      println("  Job \{i}: TIMED OUT")
    } else {
      println("  Job \{i}: exit=\{result.exit_code}")
    }
  }
  println("")

  // === デモ4: SSG風のmap API ===
  println("[Demo 4] Map API (SSG-style)")
  let files : Array[String] = ["index.md", "about.md", "contact.md"]
  let pool4 = @process_pool.ProcessPool::new(max_workers=2)
  let results4 = pool4.map(files, fn(file) {
    @process_pool.job("echo", ["Rendering \{file}"])
  })
  for result in results4 {
    println("  \{result.stdout.trim_space()}")
  }
  println("")
  println("=== Done ===")
}
