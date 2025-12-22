///|
async fn main {
  println("=== Process Pool Benchmark ===")
  println("")

  // Generate file list based on the pattern from generate_docs.mjs
  // Default: 16 documents named doc_000.md, doc_001.md, ..., doc_015.md
  let docs_dir = "scripts/docs"
  let num_docs = 16
  let files : Array[String] = []
  for i = 0; i < num_docs; i = i + 1 {
    let num_str = i.to_string().pad_start(3, '0')
    files.push("\{docs_dir}/doc_\{num_str}.md")
  }

  println("Found \{files.length()} documents")
  println("")

  // Benchmark with different worker counts
  let worker_counts = [1, 2, 4, 8]
  let results : Array[(Int, Int64)] = []

  for workers in worker_counts {
    let actual_workers = if workers > files.length() {
      files.length()
    } else {
      workers
    }

    println("--- \{actual_workers} worker(s) ---")
    let pool = @process_pool.ProcessPool::new(max_workers=actual_workers)

    let start = @process_pool.now()
    let job_results = pool.map(files, fn(file) {
      @process_pool.job("node", ["scripts/wc.mjs", file])
    })
    let elapsed = @process_pool.now() - start

    // Verify all jobs succeeded
    let mut total_words = 0
    let mut failed = 0
    for result in job_results {
      if result.exit_code == 0 {
        // Parse JSON output to get word count
        let output = result.stdout.trim_space().to_string()
        // Simple extraction of "words" field using index search
        let words_key = "\"words\":"
        let idx = find_substring(output, words_key)
        if idx >= 0 {
          let start_pos = idx + words_key.length()
          let after_key = output[start_pos:].to_string()
          let comma_idx = find_char(after_key, ',')
          let num_str = if comma_idx >= 0 {
            after_key[:comma_idx].to_string().trim_space().to_string()
          } else {
            after_key.trim_space().to_string()
          }
          let n = parse_int_simple(num_str)
          if n >= 0 {
            total_words += n
          }
        }
      } else {
        failed += 1
      }
    }

    println("  Time: \{elapsed}ms")
    println("  Total words counted: \{total_words}")
    if failed > 0 {
      println("  Failed jobs: \{failed}")
    }
    results.push((actual_workers, elapsed))
    println("")
  }

  // Summary
  println("=== Summary ===")
  let baseline = results[0].1
  for i = 0; i < results.length(); i = i + 1 {
    let pair = results[i]
    let workers = pair.0
    let elapsed = pair.1
    let speedup = baseline.to_double() / elapsed.to_double()
    if i == 0 {
      println("  \{workers} worker:  \{elapsed}ms (baseline)")
    } else {
      println("  \{workers} workers: \{elapsed}ms (\{format_speedup(speedup)}x speedup)")
    }
  }
  println("")
  println("=== Done ===")
}

fn format_speedup(x : Double) -> String {
  let int_part = x.to_int()
  let frac_part = ((x - int_part.to_double()) * 100.0).to_int()
  "\{int_part}.\{frac_part.to_string().pad_start(2, '0')}"
}

fn find_substring(haystack : String, needle : String) -> Int {
  let h_chars = haystack.to_array()
  let n_chars = needle.to_array()
  let h_len = h_chars.length()
  let n_len = n_chars.length()
  if n_len > h_len {
    return -1
  }
  for i = 0; i <= h_len - n_len; i = i + 1 {
    let mut found = true
    for j = 0; j < n_len; j = j + 1 {
      if h_chars[i + j] != n_chars[j] {
        found = false
        break
      }
    }
    if found {
      return i
    }
  }
  -1
}

fn find_char(s : String, c : Char) -> Int {
  let chars = s.to_array()
  for i = 0; i < chars.length(); i = i + 1 {
    if chars[i] == c {
      return i
    }
  }
  -1
}

fn parse_int_simple(s : String) -> Int {
  let chars = s.to_array()
  let mut result = 0
  for c in chars {
    if c >= '0' && c <= '9' {
      result = result * 10 + (c.to_int() - '0'.to_int())
    } else {
      return -1
    }
  }
  result
}
