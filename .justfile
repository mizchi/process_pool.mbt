# proc_pool justfile

# デフォルトタスク: 全テスト実行
default: test

# 全ターゲットのテストを実行
test: test-native test-js

# native ターゲットのテスト
test-native:
    moon test --target native

# js ターゲットのテスト
test-js:
    moon test --target js

# ビルドチェック
check:
    moon check --target native
    moon check --target js

# クリーン
clean:
    moon clean

# example を実行 (native)
run-example:
    moon run example --target native
