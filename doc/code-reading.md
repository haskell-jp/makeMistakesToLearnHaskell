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

# 2019/10/30 社内コードリーディング勉強会の記録

## 読んだ・解説した箇所

- Education.MakeMistakesToLearnHaskell.Env:
    - `CommandParameters`型の`!`:
        - <https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet>
        - Haskellの記号関数以外で `!` が出てきたら、「正格評価絡み」
            - 記号関数でも`!`がつくものは正格評価をするためのものであることが多い
- 例外の定義の仕方
    - `Exception`型クラスのインスタンスにする
- 掘った関数
    - `Education.MakeMistakesToLearnHaskell.productionMain`
        - `mainFromReportServer`
            - `withMainEnv`
                - `Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell.runFile`
                    - `Education.MakeMistakesToLearnHaskell.Evaluator.Types.CommandParameters`
                    - `Education.MakeMistakesToLearnHaskell.Evaluator.Types.CommandError`
                    - `Education.MakeMistakesToLearnHaskell.Evaluator.Command.resolveHaskellProcessor`
                    - `Education.MakeMistakesToLearnHaskell.Evaluator.Command.runFileWith`
                - `Education.MakeMistakesToLearnHaskell.Evaluator.Ghc.runFile`

# 2019/11/06 社内コードリーディング勉強会の記録

## 読んだ・解説した箇所

- ガード構文・if式: TODO: 別途課題を設けよう
    - 参考: <https://kazu-yamamoto.hatenablog.jp/entry/20110826/1314352340>
    - パターンマッチにおけるパターンと、`->`や`=`の間に `|` を置くと書けるBool式
    - GHC 8.6.5では、ガード構文に余計なケースを書いても警告が出ないらしい。でるべきでは？
- 掘った関数
    - ...
        - `Education.MakeMistakesToLearnHaskell.withMainEnv`
            - ...
                - `Education.MakeMistakesToLearnHaskell.Evaluator.Command.runFileWith`
                    - `Education.MakeMistakesToLearnHaskell.Evaluator.Command.fixingCodePage`
- 質問
    - `confirm`関数はわざわざDIするほどのものでもないような？
        - 原則として`Education.MakeMistakesToLearnHaskell`モジュールにある関数以外は直接入出力を行うようなことをしない、という方針で作っているため、`Education.MakeMistakesToLearnHaskell.Report.printUrlIfAsked`から呼ばれる`confirm`関数も例に漏れず、直接入出力しないバージョンも作れるよう、DIできるようにしました。  
          と、いいつつ今日の修正のとおり`printUrlIfAsked`でもちゃっかり`putStrLn`や`print`を呼んでましたし、YAGNIと言われればYAGNIな感もありますが...

# 2019/11/13 社内コードリーディング勉強会の記録

- `const`関数
- `liftIO`関数
- `stack.yaml`に管理したいパッケージを複数書けるので、それを利用してcommonsパッケージとreporterパッケージを別のパッケージとしてまとめて管理している
- 掘った関数
    - ...
        - `Education.MakeMistakesToLearnHaskell.mainFromReporter`
            - `Education.MakeMistakesToLearnHaskell.withMainEnv`
                - `Education.MakeMistakesToLearnHaskell.Report.Client.postReport`（詳細は触れず）
            - `Education.MakeMistakesToLearnHaskell.showExercise`
                - `Education.MakeMistakesToLearnHaskell.Exercise.loadDescriptionByName`
                    - `Education.MakeMistakesToLearnHaskell.Exercise.loadDescription`
                        - `Education.MakeMistakesToLearnHaskell.Exercise.loadDescription`
                            - `Education.MakeMistakesToLearnHaskell.Exercise.loadWithExtension`
                                - `Education.MakeMistakesToLearnHaskell.Text.IO.readUtf8File`
                - `Education.MakeMistakesToLearnHaskell.Error.dieWhenNothing`
