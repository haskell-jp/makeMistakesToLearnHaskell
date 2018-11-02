# Make Mistakes to Learn Haskell!

作りながら学ぶHaskell入門

## インストール方法

現状はHackageにまだ公開していないので👇の方法で。

```
git clone https://github.com/haskell-jp/makeMistakesToLearnHaskell
# あるいは git clone git://github.com/haskell-jp/makeMistakesToLearnHaskell

cd makeMistakesToLearnHaskell
stack install
```

## 使い方

1. 課題の一覧を表示する（特に引数を与えなければ一覧が表示されます）  
   ```
   mmlh
   ```
1. 課題1の内容を表示する  
   ```
   mmlh show 1
   ```
1. 課題の回答をテストする  
   （最後に`mmlh show`した課題のテストをする）  
   ```
   mmlh verify your_answer.hs
   ```

## 開発に協力していただける方へ

### 現状

問題を表示したり、ユーザーが与えた回答を判定するための基本的なフレームワークはできています。  
現在はひとまず[Haskell Day 2018](https://haskell-jp.connpass.com/event/92617/)のハンズオンのセッションを少しでも充実させるために、下記のことに取り組んでします。

- [計算アプリケーションを作る（概要）](https://github.com/haskell-jp/makeMistakesToLearnHaskell/blob/master/assets/2.5.md)に書いた目標に従い、課題とその判定処理の実装。
    - 「そのために以下の課題を解いて、Haskellの初歩を身につけましょう」以降に書いた各課題とその判定処理を実装します。
    - 時間がなさそうなので、すべての課題を作り切ることを優先させるつもりはありません（でも課題の数を増やさないとハンズオンの当日に時間が余る恐れがあるのでそこは考慮しよう。最悪別のスライドで穴埋めする）。
- その他[Issues](https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues)をご覧ください。

#### 特に私 igrepが行いたいこと

- 課題の内容とその文章を書く

#### 特に私 igrep以外にやっていただきたいこと

- 課題の判定処理の実装。
    - ヒントの出し方を含みます。詳細は次のセクションをご覧ください。
- その他[Issues](https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues)に書いたこと。

##### 課題の判定処理の実装方法

主に編集するファイルは[src/Education/MakeMistakesToLearnHaskell/Exercise.hs](https://github.com/haskell-jp/makeMistakesToLearnHaskell/blob/master/src/Education/MakeMistakesToLearnHaskell/Exercise.hs)です。  
このファイルに各課題の判定方法や、ヒントの判定処理がすべてまとまっています。  
文字列まわりで共通化できる処理を追加したくなったら、`Exercise.hs`に直接追加するか、[`src/Education/MakeMistakesToLearnHaskell/Evaluator/`](https://github.com/haskell-jp/makeMistakesToLearnHaskell/tree/master/src/Education/MakeMistakesToLearnHaskell/Evaluator)以下にファイルを追加するのが良いかと思います（おいおい`Regex.hs`というファイルを追加します）。

課題の判定処理のテストを書く場合は、[test/Education/MakeMistakesToLearnHaskell/](https://github.com/haskell-jp/makeMistakesToLearnHaskell/tree/master/test/Education/MakeMistakesToLearnHaskell)ディレクトリーに、`ExerciseNSpec.hs`（`N`は整数）という名前のファイルを追加してください。  
具体的なテストの書き方は既存のファイルを見ていただくとわかるかと思います。Hspecを使っています。

### 注意事項

- このアプリケーションは、Haskellの初心者でもソースコードを編集できるよう、できるだけ素朴なHaskellで書くことを目指しています。  
  具体的な基準は適宜議論しようと思いますが、現状は例えば
    - 使用方法が比較的分かりやすい（かつ必要性が高い）`default-extensions`に記載した言語拡張のみを使う
    - いわゆる `ReaderT IO` パターンを使用せず、 `Env` を受け取る関数を全体で使用する
        - Monad Transformerを極力使わない（局所的には使うかも）
    - 型レベルプログラミングをしない
- その一方、**`CPP`を使うことで`import`を[`src/imports/external.hs`](https://github.com/haskell-jp/makeMistakesToLearnHaskell/blob/master/src/imports/external.hs)にまとめる**、というかなり変わったことをしています。  
  これは、コードベース内で`import`の書き方を統一する、同じ`import`を何度も書かないで済ますために考えた方策です。  
  現状、`external.hs`で外部のパッケージの`import`のみをまとめていますが、今後もっとコードベースが大きくなったとき、レイヤーを分ける際にも役立つでしょう。  
  そのため、今後も少なくとも外部のパッケージを新たに`import`する際は、**必ず[`src/imports/external.hs`](https://github.com/haskell-jp/makeMistakesToLearnHaskell/blob/master/src/imports/external.hs)に**書いてください。

## ターゲット

- プログラミングは他の言語で経験している。
- 他のHaskell入門書を読んでみたが、Haskellでプログラムを書く方法がわからない。
    - TODO: にしては今の内容はちょっと初歩的すぎるかもしれないので、このターゲット自体かexerciseの内容を改めよう
- Haskellがどんな言語か、どうやってプログラムを作るのか、軽く知りたい。

## 依存ライブラリ

- [main-tester](https://gitlab.com/igrep/main-tester)