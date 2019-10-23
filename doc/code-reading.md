# 2019/10/16 社内コードリーディング勉強会の記録

## 目的

- makeMistakesToLearnHaskellのコードを理解することを通して、Haskellのアプリケーションの作り方を学ぶ
- makeMistakesToLearnHaskellのコミッターを増やす

## 読んだ・解説した箇所

- package.yaml・cabalファイルについて
    - 参考リンク: <https://haskell.e-bigmoon.com/stack/intro/hpack.html>
- src/imports/external.hs:
    - makeMistakesToLearnHaskellにおいて最も標準から外れた書き方をしている箇所
        - CPPの`include`を使って、各モジュールで使う`import`文を共通化している
        - 試み自体はうまくいっているものの、非標準だし、ツールが適切にサポートできていないことも多いし、どこかでやめたい方針ではある。
            - 同じ問題意識はほかのHaskellerも持っていて、<https://github.com/ghc-proposals/ghc-proposals/pull/283>という提案もある。backpackとの兼ね合いとか面倒くさそう
- app/Main.hs
- src/Education/MakeMistakesToLearnHaskell.hs
    - `avoidCodingError`: <https://haskell.jp/blog/posts/2017/windows-gotchas.html>
    - <http://bicycle1885.hatenablog.com/entry/2012/12/24/234707>
    - `printExerciseList`関数まで

## 補足

- Hasktagsなどのツールを使ってタグジャンプをできるようにしておくと追いやすいです。Haskell IDE Engineの機能を使うのもよし

# 2019/10/23 社内コードリーディング勉強会の記録

## 読んだ・解説した箇所

- `type`キーワード: 型シノニム
- `newtype`キーワード: 単純に一つの型をラップする型を作る
- `<|>`: `Alternative`型クラスのメソッド。`IO`に対して使うと、左辺の`IO`がエラーを投げた場合に右辺の`IO`を実行する、という挙動になる。
- optparse-applicative
- `Env`型:
    - Record of Functionsというテクニックを使って定義。ファイルへの書き込みを伴う関数など、テスト時に実行したくない関数を差し替えられるように定義した型。
    - Javaの文化圏で言うとDIコンテナーが近い
- `withMainEnv`関数の中身を読んでいるところ。`Env`型の値を定義する箇所を読み始めるところまで
