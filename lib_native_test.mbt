// Native専用テスト: タイミングに依存するテスト
// JS環境ではSemaphoreの動作タイミングが異なるため、native専用

///|
async test "max_workers limits concurrency" {
  let pool = ProcessPool::new(max_workers=1)
  let start = now()
  let jobs = [
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
  ]
  let results = pool.run_all(jobs)
  let elapsed = now() - start

  assert_eq(results.length(), 2)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
  // max_workers=1 なので順次実行、2 * 100ms = 200ms 以上かかるはず
  assert_true(elapsed >= 150L)
}

///|
async test "parallel execution is faster" {
  let pool = ProcessPool::new(max_workers=4)
  let start = now()
  let jobs = [
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
  ]
  let results = pool.run_all(jobs)
  let elapsed = now() - start

  assert_eq(results.length(), 4)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
  // max_workers=4 なので並列実行、100ms + α で終わるはず
  assert_true(elapsed < 300L)
}
