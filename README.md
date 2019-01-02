# Make Mistakes to Learn Haskell!

作りながら学ぶHaskell入門

## 💾インストール方法

### まだ[Stack](https://haskellstack.org)や[Haskell Platform](https://www.haskell.org/platform/)をインストールしていない場合は

[Stack](https://haskellstack.org)のインストールを推奨します。  
下記のいずれかの方法でインストールしてください。

#### 🍎🐧Mac OS XやLinuxなどのUnix系OSをお使いの方:

「ターミナル」を起動し、下記のいずれかのコマンドを実行してください。

```
curl -sSL https://get.haskellstack.org/ | sh
```

あるいは、

```
wget -qO- https://get.haskellstack.org/ | sh
```

#### 🏁Windowsをお使いの方

[64bit版のWindowsをお使いの方はこちら](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)、[32bit版のWindowsをお使いの方はこちら](https://get.haskellstack.org/stable/windows-i386-installer.exe)からインストーラーをダウンロードして、インストールしてください（よくわからなければ、とりあえず64bit版を試してみてください）。  

[Chocolatey](https://chocolatey.org/)をお使いの方は、

```
choco install haskell-stack
```

でもインストールできます。

### 💾GHCと「Make Mistakes to Learn Haskell!」のインストール方法

現状はHackageにまだ公開していないので👇のコマンドを実行してください（どのOSでもこのコマンドです）。

```
git clone https://github.com/haskell-jp/makeMistakesToLearnHaskell
# あるいは git clone git://github.com/haskell-jp/makeMistakesToLearnHaskell

cd makeMistakesToLearnHaskell
stack install
```

GHCのインストールと、「Make Mistakes to Learn Haskell!」のビルドが始まります。

インストールが完了したら、「⚙️使い方」の節に書かれたコマンドを試してみてください。

なお、Linuxにおいて、libtinfoパッケージがないとインストールできない」というトラブルが報告されています。  
例えばUbuntuの場合、下記のコマンドを実行してインストールしておく必要があるかも知れません。

```
sudo apt-get install libtinfo-dev
```

#### ⚠️トラブルが発生したら:

インストール中などに何か困ったことが発生した場合、下記のいずれかのウェブサービスで質問してみてください。

- [teratailのHaskellタグ](https://teratail.com/tags/Haskell)
- [スタック・オーバーフローのHaskellタグ](https://ja.stackoverflow.com/questions/tagged/haskell)
- [日本Haskellユーザーグループ (a.k.a. Haskell-jp)の公式Slack Workspaceにおける、questionsチャンネル](https://haskell-jp.slack.com/messages/C5666B6BB/)
    - [登録はこちらから](https://join.slack.com/t/haskell-jp/shared_invite/enQtNDY4Njc1MTA5MDQxLTAzZGNkZDlkMWYxZDRlODI3NmNlNTQ1ZDc3MjQxNzg3OTg4YzUzNmUyNmU5YWVkMjFmMjFjYzk1OTE3Yzg4ZTM)
- [Redditのr/haskell\_jp](https://www.reddit.com/r/haskell_jp/)

### 🆙「Make Mistakes to Learn Haskell!」自体のアップデート方法

現状、当入門はまだまだ完成度が低いため、今後も度々更新することとなります。  
アップデートが必要な場合は、👆の手順で`git clone`したディレクトリーに移動した上で、

```
git pull
stack install
```

を実行してください。

## ⚙️使い方

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

### 🏁Windowsユーザー向けTips

MSYS2を利用している場合、stackがインストールするMSYS2と衝突して問題が起こることがあります。その場合、`%APPDATA%\stack\config.yaml`に以下の行を追加し、インストール済みのMSYSを使うように設定しましょう。

```
skip-msys: true
```

## 開発に協力していただける方へ

### 現状

問題を表示したり、ユーザーが与えた回答を判定するための基本的なフレームワークはできています。  
私 igrepが社内の勉強会で使用するため、下記のことに取り組んでいます。

- [計算アプリケーションを作る（概要）](https://github.com/haskell-jp/makeMistakesToLearnHaskell/blob/master/assets/2.5.md)に書いた目標に従い、課題とその判定処理の実装。
    - 「そのために以下の課題を解いて、Haskellの初歩を身につけましょう」以降に書いた各課題とその判定処理を実装します（実際には課題の中身を優先して作っています）。
- その他[Issues](https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues)をご覧ください。

#### 課題におけるプロンプト表記について

- GHCi に対する入力は `ghci>`
- シェルに対する入力は `shell>`

と書きましょう。

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
