# 入力の取得

標準入力から読んだ文字列を、改行文字で区切って、逆順に並び替えて標準出力に書き込みましょう。

## 実行結果例

`input.txt`という、次のような内容のファイルがあるとき、

```
Lorem ipsum dolor sit amet,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
consectetur adipiscing elit,
```

下👇のように実行すると、

```
shell> stack exec runhaskell 4.hs < input.txt
```

下👇のような文字列を出力します。

```
consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Lorem ipsum dolor sit amet,
```

## 必要な知識

### リスト、`reverse`関数、`lines`関数、`unlines`関数、`putStr`関数

まずは入力や出力をどのように行うのかは忘れて、「文字列に対して、どんな処理をするのか」を考えましょう。今回の課題に関して言えばずばり「文字列を、改行文字で区切って、逆順に並び替え」る処理が該当します。

ちょっと脱線。Haskellでのプログラミングに限らず、プログラムの設計について考えるときは、可能な限り「入力や出力に関わる部分」と「（入力を受け取ってできた）**値に対して**（最終的に出力する前に）**どのような処理をするのか**」を分けて考えると、より再利用性が高く、テストも理解もしやすいプログラムが書けます。今回の課題に関して言えば、そのように考えることで「文字列を改行文字で区切って、逆順に並び替え」る処理を、GHCi上でもお気軽にテストできます。

と、言うわけで今回の課題を解くための関数をGHCi上で試しましょう。

```bash
shell> ghci
```

最初に紹介するのは文字列を「改行文字で区切る」関数、`lines`関数です。課題3で触れた、`\n`を思い出しながら、次のように入力してみてください。

```haskell
ghci> lines "A string\ncontaining\nnewlines\nin the middle."
["A string","containing","newlines","in the middle."]
```

出力された角括弧`[]`で囲われたものは、文字列のリストです。リストは、Haskellで（同じ型の）複数の値を表すのに使われる、最も単純なデータ構造です。もちろん文字列だけでなく、ほかのどんな型の値も入れることができます。例えば下記は数値のリストです。

```haskell
ghci> [1, 2, 3]
[1,2,3]
```

`lines`関数は、受け取った文字列を改行文字(`\n`)で区切ることで、その文字列のリストを作ります。これで「改行文字で区切って」という処理は実装できますね。

続いて、「逆順に並び替え」る処理です。`lines`関数で作った文字列のリストを逆順にするには、文字通り`reverse`関数を使いましょう:

```haskell
ghci> reverse ["A string","containing","newlines","in the middle."]
["in the middle.","newlines","containing","A string"]
```

逆順になって返ってきましたね。

さて、この課題の要件では最終的に「標準出力に書き込」まなければならないのでした。これまでの課題で勉強した`putStrLn`関数は、文字列を渡さないと型エラーになります。

```haskell
ghci> putStrLn ["in the middle.","newlines","containing","A string"]

<interactive>:12:11: error:
    ? Couldn't match expected type ‘Char’ with actual type ‘[Char]’
    ? In the expression: "in the middle."
      In the first argument of ‘putStrLn’, namely
        ‘["in the middle.", "newlines", "containing", "A string"]’
      In the expression:
        putStrLn ["in the middle.", "newlines", "containing", "A string"]

... （以下略）
```

課題2で紹介した`print`関数は文字列のリストにも使えます。

```haskell
ghci> print ["in the middle.","newlines","containing","A string"]
["in the middle.","newlines","containing","A string"]
```

ただし、これは課題の要件にあった出力ではありません。改行文字で区切って逆順にした後、各行を逆順に出力するには、また改行文字で文字列を結合する必要があるのです。そこで役に立つのが`unlines`関数です:

```haskell
ghci> unlines ["A string","containing","newlines","in the middle."]
"A string\ncontaining\nnewlines\nin the middle.\n"
```

期待したとおり、リストの要素が改行文字で結合されて、一つの文字列に変わったのがわかるでしょうか？できた文字列を`putStrLn`してみると、よくわかります:

```haskell
ghci> putStrLn "A string\ncontaining\nnewlines\nin the middle.\n"
A string
containing
newlines
in the middle.

ghci>
```

おっと、最後の行`in the middle`の後に、余計な空行ができてしまいました。これでは課題の要件をクリアできません。`putStrLn`の`Ln`は「Line」の略であるとおり、必ず行の末尾の文字である`\n`を出力します。そして、`unlines`も文字列の末尾に`\n`を加えます。`\n`はあくまでも行の末尾にあるべき文字なので、最後の文字にもちゃんと加えるのでしょう。

いずれにしても、この問題を回避するためには、`putStr`という文字通り`Ln`がつかない関数を使います。

```haskell
ghci> putStr "A string\ncontaining\nnewlines\nin the middle.\n"
A string
containing
newlines
in the middle.
ghci>
```

今度は余計な空行が出てませんね！LGTM！

### 全部組み合わせる

さて、ここまで紹介した関数を生かして「文字列を、改行文字で区切って、逆順に並び替え」る処理と、さらにそれを出力する処理を実装してみましょう。全部組み合わせてしまうと課題の答えになってしまうので、一部の処理を示すことで、注意すべき点をお伝えしたいと思います。

例えば、文字列を改行文字`\n`で区切って`print`関数で中身を表示する処理は、次のように書くことで実装できます:

```haskell
ghci> print (lines "A string\ncontaining\nnewlines\nin the middle.\n")
["A string","containing","newlines","in the middle."]
```

`(lines "A string\ncontaining\nnewlines\nin the middle.\n")`と、**関数名とその引数両方をカッコで**囲っている点に注意してください。ほかのよくあるプログラミング言語のように、

```haskell
ghci> print lines("A string\ncontaining\nnewlines\nin the middle.\n")
```

でもなければ、ましてや

```haskell
ghci> print lines "A string\ncontaining\nnewlines\nin the middle.\n"
```

でもない点に注意してください。この場合は、`print`関数に`lines`関数と`"A string\ncontaining\nnewlines\nin the middle.\n"`という**2つの引数**を渡している、という意味になります。「`print`関数に`lines`関数を渡す」というのはちょっとおかしく聞こえるかもしれません。`print`関数以外の関数ですと、Haskellの構文上そういう式も普通にあり得るので、Haskellは2つの引数を渡した、と解釈します。

いずれにしても、`print`関数は1つの引数しか受け取らないので上記の式はエラーになります:

```haskell
ghci> print lines "A string\ncontaining\nnewlines\nin the middle.\n"

<interactive>:23:1: error:
    ? Couldn't match expected type ‘[Char] -> t’
                  with actual type ‘IO ()’
    ? The function ‘print’ is applied to two arguments,
      but its type ‘(String -> [String]) -> IO ()’ has only one
      In the expression:
        print lines "A string\ncontaining\nnewlines\nin the middle.\n"
      In an equation for ‘it’:
          it = print lines "A string\ncontaining\nnewlines\nin the middle.\n"
    ? Relevant bindings include it :: t (bound at <interactive>:23:1)
```

`? The function ‘print’ is applied to two arguments,`  
`but its type ‘(String -> [String]) -> IO ()’ has only one` という箇所で「`print`関数は引数を1つしか取らないのに2つ渡しているよ」ヒントを出していますので、ぼんやり覚えておいてください。

ここまでの話を一般化しましょう。Haskellで関数を呼び出す際は、次のような形式で書いてください。

```haskell
関数名 引数1 引数2 ... 引数N
```

関数名と引数をスペースで区切っているだけでいいのです。カッコは、どの関数にどの引数を渡しているのか、明確にしたいときだけ使います。

※以下で使用しているように、Haskellで行コメントは`--`で始めます。`--`から行末まではコメントとして扱われ、GHCには解釈されません。

```haskell
関数A Aの引数1 (関数B Bの引数1 Bの引数2 ... Bの引数N) Aの引数3 ... Aの引数N
--             ^------------------------------------^
--                  この部分が関数Aの引数2になる
```

関数呼び出しは非常に頻繁に使う構文なので、簡単に使えるのはいいことですね！


ちなみに、Haskellにはこうした関数呼び出しがもっと複雑に入れ子になった場合に、より簡潔に書く方法があるので、おいおい説明します。

### `do`記法で`getContents`関数と細い左矢印`<-`を使って、入力を受け取る

いよいよ、標準入力から文字列を読んで、変数に代入する方法を説明しましょう。Haskellでこれを一番簡単に実現する方法は、ちょっと変わっています。例えば「`getContents`という命令を使って標準入力から読んだ文字列を、`input`という変数に代入する」という処理は、`do`記法の中で、下記のように書きます:

```haskell
do
  input <- getContents
```

変数への代入というと、イコール `=` を使った構文をイメージする方が多いかと思います。しかし、Haskellではこの場合細い左矢印 `<-` を使わなければなりません（まだ詳しく説明していませんが、イコール `=`を使った代入もやっぱりあります）。Haskellにおいて`getContents`は、「命令（慣習的に『アクション』と呼ばれることも多いです）」であり、その実行結果を取り出して変数に代入する際、細い左矢印 `<-` を使わなければなりません。

これまでに何度も使った、`putStrLn`や`print`、それから`putStr`も「命令」です。ただし、`putStrLn`や`print`などは特に意味のある結果を返さないので、これまで`<-`は出てきませんでした。`do`記法は、これらのような「命令」を列挙したり、「命令」の実行結果を簡単に利用するための仕組みを提供してくれるのです。

細い左矢印 `<-` を使って代入した変数は、普通の変数と同様に参照して使うことができます。

```haskell
do
  input <- getContents
  putStr input
```

このように書くと、おなじみ`cat`コマンドのように「標準入力から文字列を読んで、標準出力にそのまま書き込む」処理ができます。

おなじみ`main`の定義に書けば、簡易版`cat`コマンドのできあがりです！

```haskell
main = do
  input <- getContents
  putStr input
```

ただし、**細い左矢印 `<-` は`do`の中でしか使えない**、という点に注意してください。次のプログラムを実行しようとすると、エラーになってしまいます:

```haskell
main =
  input <- getContents
  putStr input
```

```
shell> stack exec runhaskell no-do.hs
no-do.hs:2:9: error:
    parse error on input ‘<-’
    Perhaps this statement should be within a 'do' block?
  |
2 |   input <- getContents
  |         ^^
```

`Perhaps this statement should be within a 'do' block?`というエラーメッセージは、わざわざ「この命令(`input <- getContents`)は`do`ブロックの中で使うべきなのでは？」と提案してきてくれています。「わかってるなら補ってくれてもいいのに...」という気もしますが、気を取り直して`do`を付け足しましょう。

## 課題の解き方

`getContents`で取得した標準入力の文字列を、細い左矢印 `<-` で取り出し、適当な変数に代入してください。その代入した変数に対して、「改行文字で区切って、逆順に並び替え」る関数を順番に適用しましょう。最後は逆順になったリストを改行文字で結合し直し、`putStr`で出力すればバッチリです。あっ、すべて`do`記法の中に書くのをお忘れなく。

## コラム: 「代入」か「束縛」か

ところで、読者の皆さんの中には、「Haskell（あるいはHaskellを始めとする関数型プログラミング言語）では『代入 (assignment)』という言葉は使わないよ。『束縛 (binding)』だよ」という主張を聞いたことがあるかも知れません（[例えばこれ][qiita]）。`input <- getContents`という行は、「`input`という変数に`getContents`の結果を**束縛する**」というような説明は実際しばしばあります。しかしこの当入門「失敗しながら学ぶHaskell入門」では、「理論的な背景の違いはあるものの、少なくともソースコードを書いて実行するだけの我々からみれば、実用上の違いはほとんどない」という立場から、読者にとってより親しみの深いであろう「代入」という単語を敢えて使っています。この問題についての詳細は[「名前の束縛」という名の束縛][fumieval]という記事を、またその元となった議論が書かれている[こちら][issue28]や[こちら][pr35]、[Slackにおけるこのあたりの議論][slack1]と[その続き][slack2]もご覧ください。

[qiita]: https://qiita.com/lotz/items/aca1d179c14d4dca5099 "Haskellの変数には値を代入しません、束縛します。"
[fumieval]: http://fumieval.hatenablog.com/entry/2018/10/31/150056 "「名前の束縛」という名の束縛"
[issue28]: https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues/28 "再代入ができないことや「束縛」という単語について触れる #28"
[pr35]: https://github.com/haskell-jp/makeMistakesToLearnHaskell/pull/35 "Fix #28 代入と束縛の区別について追記 #35"
[slack1]: https://haskell.jp/slack-log/html/CD87P78HF/2.html#message-1540696620.004200
[slack2]: https://haskell.jp/slack-log/html/CD87P78HF/3.html#message-1540809627.039500
