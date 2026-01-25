# 関数を定義してリファクタリングする

前の課題、課題hoge(9?)のプログラムを書き換え、重複した箇所を関数として切り出しましょう。切り出した後も、元の課題hoge(9?)のプログラムと同じように動作するようにしてください。

## 必要な知識

### どこを切り出すか考える

以下は、課題hoge(9?)の回答例です:

```haskell
main = do
  putStrLn "Height Weight: "
  hwStr <- getLine
  case words hwStr of
    heightStr : weightStr : _ -> do
      let height = read heightStr
          weight = read weightStr
      print (weight / (height * height))
    [heightStr] -> do
      putStrLn "Weight: "
      weightStr <- getLine
      let height = read heightStr
          weight = read weightStr
      print (weight / (height * height))
    other ->
      putStrLn ("Invalid input: " ++ hwStr)
```

課題hoge(4?)より:

> Haskellでのプログラミングに限らず、プログラムの設計について考えるときは、可能な限り「入力や出力に関わる部分」と「（入力を受け取ってできた）**値に対して**（最終的に出力する前に）**どのような処理をするのか**」を分けて考えると、より再利用性が高く、テストも理解もしやすいプログラムが書けます。

というわけで、上記から抜粋した、重複している箇所のうち出力に関係のある処理、つまり`print`関数を削除した、下記の部分を関数として切り出しましょう。

```haskell
      let height = read heightStr
          weight = read weightStr
      (weight / (height * height))
```

### 引数を受け取る関数を定義する

ここでは、引数を受け取る関数を定義する方法を学びましょう。これまでそうしてきたように、`main`関数に書いた処理に行いたい処理をすべて書き並べるだけでも、プログラムは書けます。しかし課題hoge(9?)の回答が自然とそうなったように、ある程度長いプログラムを書いていると、重複した箇所が発生することがしばしばあります。そのような箇所によい名前を与えて「共通した処理」として切り出すことは、プログラムを理解したり修正したりするときの重要な手がかりになります（ちなみに「よい名前」が与えられない場合は、例え重複していても無闇に切り出さない方がよいでしょう）。これはもちろんHaskellに限った話ではありません。

例として、BMIを計算して返す関数を紹介しましょう... と言いたいところですが、そのままそれをここで定義してしまうと課題の答えになってしまうので、代わりに「身長`heightStr`と体重`weightStr`を文字列として受け取って、BMIを求める計算**式**」を返す関数を`bmiExpression`という名前で書くことにします:

```haskell
bmiExpression heightStr weightStr = weightStr ++ " / (" ++ heightStr ++ " * " ++ heightStr ++ ")"
```

`bmiExpression`が関数の名前で、`heightStr`が1つ目の引数の名前、`weightStr`が2つ目の引数の名前です。`=`より後ろに、`bmiExpression`関数の定義、すなわち`bmiExpression`関数が引数を受け取って何をするのか、を書きます。

関数の定義が長い場合、途中で改行したくなりますよね。そういう場合は改行した次の行でインデント（行頭にスペースを1つ以上挿入）してください。

改行する位置は、変数名の途中などでなければどこでも構いません。こちら👇のようにイコール`=`の直後でも構いませんし、

```haskell
bmiExpression heightStr weightStr =
  weightStr ++ " / (" ++ heightStr ++ " * " ++ heightStr ++ ")"
```

イコール`=`の直前でも構いません:

```haskell
bmiExpression heightStr weightStr
  = weightStr ++ " / (" ++ heightStr ++ " * " ++ heightStr ++ ")"
```

（今回はそうでもないですが）引数のリストが長すぎると感じたら関数名の直後で改行してもいいですし、

```haskell
bmiExpression
  heightStr weightStr =
  weightStr ++ " / (" ++ heightStr ++ " * " ++ heightStr ++ ")"
```

関数の定義の本体、式の途中で改行しちゃっても構いません:

```haskell
bmiExpression heightStr weightStr = weightStr
  ++ " / ("
  ++ heightStr ++ " * " ++ heightStr
  ++ ")"
```

当然複数の箇所で改行してもよいので、これまでの例を組み合わせて次のように書くのもよいでしょう:

```haskell
bmiExpression heightStr weightStr =
  weightStr
  ++ " / ("
  ++ heightStr ++ " * " ++ heightStr
  ++ ")"
```

GHCは、関数定義の途中で改行していることに気づいた場合、インデントしている行が続く限り、つまり行頭にスペースを入力した行が続く限り、該当の関数の定義が続いていると解釈します。なので、👇のように最後の行がインデントされていない場合、エラーになってしまいます:

```haskell
bmiExpression heightStr weightStr =
  weightStr
  ++ " / ("
  ++ heightStr ++ " * " ++ heightStr
++ ")"
```

GHCiに`:{`と`:}`を使って入力しても、やっぱり受け入れてくれません:

```haskell
ghci> :{
ghci| bmiExpression heightStr weightStr =
ghci|   weightStr
ghci|   ++ " / ("
ghci|   ++ heightStr ++ " * " ++ heightStr
ghci| ++ ")"
ghci| :}

<interactive>:54:1: error: parse error on input ‘++’
```

### GHCiの`:l`と`:r`で、ファイルに定義した関数や型を読み込む

ここで、本課題で学習する内容を試す上でとても便利な、GHCiの機能を紹介させてください。それは、`:l`（`:load`の略）と`:r`（`:reload`の略）です。`:l`コマンドは指定したHaskellのソースコードが書かれたファイルを読み、中に書かれている関数などを、GHCi上で直接扱えるようにしてくれます。さらに、`:l`コマンドで読んだファイルを修正した後、`:r`コマンドを使えば、それだけで再読込が出来ます！書いている関数の挙動や型の定義をちょっとGHCiで確かめたい時にとても便利な機能なので、先ほど紹介した`bmiExpression`を試すのにも使ってみましょう。

（もっと早く紹介してもよかった...😰）

前節で定義した`bmiExpression`と言う関数を、例えば`bmiExpression.hs`という名前のファイルに保存した場合、`:l bmiExpression.hs`で、その定義を読み出すことが出来ます:

```haskell
ghci> :l bmiExpression.hs
[1 of 1] Compiling Main             ( bmiExpression.hs, interpreted )
Ok, one module loaded.
```

実際に`:l`コマンドで読み込んだ関数などの一覧を見るには、`:browse`コマンドを使用します。

```haskell
ghci> :browse
bmiExpression :: [Char] -> [Char] -> [Char]
```

定義した関数の名前を直接指定して、型宣言を見ること出来ます。

```haskell
ghci> :t bmiExpression
bmiExpression :: [Char] -> [Char] -> [Char]
```

型推論によって`bmiExpression`の引数と戻り値の型が自動で決定されているのがわかるでしょうか。この場合、`bmiExpression`は「文字列を2つ受け取って、文字列を返す関数」と解釈されています:

```haskell
ghci> bmiExpression "Height" "Weight"
"Weight / (Height * Height)"
ghci> bmiExpression "1.9" "45"
"45 / (1.9 * 1.9)"
```

### 純粋な関数の中でも（`do`の中以外でも）`let`を使う

続いて、「**数値として**身長`height`と体重`weight`を受け取って、BMIを求める計算**式**」を返す関数を書いてみましょう。`Double`型の値を受け取る関数にするため、名前を`bmiExpressionFromDoubles`とします。

この`bmiExpressionFromDoubles`の定義の場合、引数として受け取った`height`や`weight`を一旦`show`関数で文字列に変更する必要があります。ここではその`show`した結果をそれぞれ`heightStr`と`weightStr`というローカル変数に代入しましょう。そういえば課題hoge(5?)で`let`という構文でローカル変数に代入できることを知りました。今回のような関数定義の中でも使えるでしょうか:

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr = show height
      weightStr = show weight
  weightStr ++ " / (" ++ heightStr ++ " * " ++ heightStr ++ ")"
```

残念ながら、上記の状態で`:l`を使ってGHCiに読ませようとすると、エラーになってしまいます:

```haskell
ghci> :l bmiExpressionFromDoubles.hs
[1 of 1] Compiling Main             ( bmiExpressionFromDoubles.hs, interpreted )

bmiExpressionFromDoubles.hs:3:3: error: parse error on input ‘let’
  |
3 |   let weightStr = show weight
  |   ^^^
Failed, no modules loaded.
```

詳しい事情は割愛しますが、Haskellの仕様上、このような形式の`let`は`do`記法の中でしか使えません（`do`記法の中で使う`let`については課題hoge(5?)で紹介しました）。`do`記法は本来、課題hoge(3?)で最初に使用したように複数の「命令」を列挙するための構文です。ここでいう「命令」とは、`putStrLn`や`getContents`など、入出力を伴う関数（など）を指すのでした。一方`bmiExpressionFromDoubles`は、入出力を行わない、単に受け取った引数から結果を計算するだけの「純粋な関数」です。こうした関数で`do`記法を使うのは本来の使い方ではないため、あまり一般的ではありません。

`bmiExpressionFromDoubles`のような「純粋な関数」において`let`を使うときは、次のような`let ... in`という構文を使ってください。と、いうわけで`bmiExpressionFromDoubles`を`let ... in`で定義してみましょう:

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr = show height
      weightStr = show weight
   in weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

今度はできたはずです！

`let ... in`にはいくつか注意事項があります。1つは、`let ... in`では必ず`let`1つに対して`in`が1つだけ対応しなければならない、ということです。なので、例えば次のように`weightStr`も`let`で代入を始めると構文エラーになってしまいます:

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr = show height
  let weightStr = show weight
   in weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

表示されるエラーメッセージの例:

```
<interactive>:13:3: error: parse error on input ‘let’
```

👆のように「変数への代入1つにつき`let`を1つ書く方が好きなんだ！」という場合は都度`in`を書いてください。👇のように行末に`in`を書くとよいでしょう:


```haskell
bmiExpressionFromDoubles height weight =
  let heightStr = show height in
  let weightStr = show weight in
      weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

この点は`do`記法における`let`と使い勝手が異なるのでご注意を。

それから、これは`do`記法における`let`にも言えることですが、`=`の左辺、つまり値を定義する式が複数行にまたがる場合、2行目以降は変数の名前の1文字目より後ろにインデントしてください。例えば、

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr =
    show height -- <= `let` よりは後ろだけど `heightStr` ほど後ろじゃない！
      weightStr = show weight
   in weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

と書いた場合は👇のようなエラーになりますし、

```
<interactive>:7:5: error:
    parse error (possibly incorrect indentation or mismatched brackets)
```

次👇のように書いたとしても、やはり同様に`parse error (possibly incorrect indentation or mismatched brackets)`というエラーになります:

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr =
      show height -- <= `let` よりは後ろだけど `heightStr` と同じ位置に！
      weightStr = show weight
   in weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

下記👇のように必ず変数の名前より後ろで2行目以降を始めましょう:

```haskell
bmiExpressionFromDoubles height weight =
  let heightStr =
        show height -- <= `let` よりも `heightStr` よりも後ろ！
      weightStr = show weight
   in weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
```

このルールは、実は変数に代入したり関数を定義したりする`let`以外の構文でも一貫しています。当課題の最初の方で紹介した「引数を受け取る関数を定義する」においても、次の節で紹介する`where`においても例外はありません。定義が2行以上にまたがる場合は、2行目以降を変数の名前より後ろまでインデントする必要があります。`let`の場合`let`と定義する変数の名前が同じ行に置かれることが多いので（かつての私のように）誤解してしまわないよう気をつけてください。

### `let`の代わりに`where`を使って、トップダウンな定義を書く

※今回の課題を解くだけでは不要ですが、よくHaskellerが使うやり方なので、覚えておきましょう。

Haskellにおいては、`let`のような、関数の定義の中でのみ使用する変数（ローカル変数）や関数（ローカル関数）を定義する構文がもう1つあります。それは「`where`」といいます。`let`の代わりに`where`を使うと、`bmiExpressionFromDoubles`を次のように書き換えることが出来ます:

```haskell
bmiExpressionFromDoubles height weight =
  weightStr ++ " / (" ++ heightStr  ++ " * " ++ heightStr ++ ")"
 where
  heightStr = show height
  weightStr = show weight
```

代入した2つのローカル変数`heightStr`・`weightStr`が、それぞれを利用している箇所より**後ろ**で代入されているところに注目してください。`where`を使えば、このように関数のどこかで定義する変数を、関数の本体よりも後ろで定義することができます。他の多くのプログラミング言語でローカル変数を定義するときの構文と異なり、順番が逆なので慣れないうちは読みにくいかもしれません。しかし`where`を使ってトップダウンに書けば、関数の概要をつかみやすくなります。`main`関数から書き始めてトップダウンに必要な関数を定義していくやり方を、`where`はローカル変数においても推奨しているのです。そうした利点がある故か、`let`よりも`where`を好んで使う人が多いように思います。

### 関数に型注釈をつける

ここまで定義した`bmiExpressionFromDoubles`関数の、型を見てみましょう。`bmiExpressionFromDoubles`関数を書いたファイル（下記の例では`bmiExpressionFromDoubles.hs`という名前にしています）を`:l`コマンドで読んだ後、`:t`コマンドを使います:

```haskell
ghci> :l bmiExpressionFromDoubles.hs
[1 of 1] Compiling Main             ( bmiExpressionFromDoubles.hs, interpreted )
Ok, one module loaded.
ghci> :t bmiExpressionFromDoubles
bmiExpressionFromDoubles
  :: (Show a1, Show a2) => a2 -> a1 -> [Char]
```

`Show`型クラスのインスタンスである型`a1`と、同じく`Show`型クラスのインスタンスの型`a2`の値をそれぞれ1つずつ受け取って、`[Char]`型（つまりは`String`）の値を返す関数となっています。前の節でも触れましたが、GHCには型推論という機能があるので、特に型注釈を付けなくても`bmiExpressionFromDoubles`がどんな型の引数を受け取ってどんな型の値を返すのか、`bmiExpressionFromDoubles`の中身を読むだけで分かります。

この場合はなるほど、`bmiExpressionFromDoubles`関数は`height`と`weight`、2つの引数に対して、`show`関数を実行して文字列に変換することしかしていないので、「`Show`型クラスのインスタンスであればなんでもよい」と解釈されたみたいですね！

しかし、今回はあくまでも関数の名前が`bmiExpressionFromDoubles`と名乗っているとおり、`Double`型の数値のみを受け取るようにしたいものです。というわけで、`bmiExpressionFromDoubles`に「型注釈」を加えて、`bmiExpressionFromDoubles`が何の型の値を受け取って何の型の値を返すのか明確にしましょう。`bmiExpressionFromDoubles`の定義より手前の行に次のように書き加えてください:

```haskell
bmiExpressionFromDoubles :: Double -> Double -> String
bmiExpressionFromDoubles height weight =
  ... 省略 ...
```

`bmiExpressionFromDoubles :: Double -> Double -> String`という行が加えられました。`read`関数について勉強した課題hoge(5?)を思い出してください。`read "1" :: Integer`と書いて`read`関数にどの型を返すべきか教えていたのと同様に、`:: 型`という構文を使います。決定的に異なるのは、`関数名 :: 型`という型注釈を、関数本体の定義とは別に書くという点です。少し冗長な書き方ではありますが、関数の型という、関数の責務を捉えるのにとても有効なドキュメントを敢えて分離することで、関数の概要を捉えるのに集中させてくれる、というメリットがあります。

さて、これによって`bmiExpressionFromDoubles`関数は「`Double`型の値を2つ受け取って、`String`型の値を返す」関数であることを明確に示すことが出来ました。GHCもそのことを認識してくれるはずです。型注釈を書き加えたファイルを保存したら、GHCiの`:r`コマンドで再度`bmiExpressionFromDoubles`の型を確認してみましょう:

```haskell
ghci> :r
[1 of 1] Compiling Main             ( bmiExpressionFromDoubles.hs, interpreted )
Ok, one module loaded.
ghci> :t bmiExpressionFromDoubles
bmiExpressionFromDoubles :: Double -> Double -> String
```

ご覧のとおり、`bmiExpressionFromDoubles`の型を`Double -> Double -> String`、つまり「`Double`型の値を2つ受け取って、`String`型の値を返す」関数として認識してくれました！

以上のようにHaskellでは、型推論を活かして自動で関数の型を解釈してくれる一方、型注釈を付けることで、自分が指定したい型に制限することが出来ます。今回は引数が`Show`型クラスのインスタンスであれば何でもいい`a1`型と`a2`型だったのを、`Double`型のみに制限しました。

このような用途の他にも、型注釈を付ければ自分が想定した型とHaskellが推論した型との答え合わせをするのにも使えますし、後でコードを読むときの重要な手がかりにもなります。一般的に、（`let`や`where`で定義するような）ローカル変数でない、「トップレベル」に定義する関数や変数には型注釈を付けることが推奨されていますので、ぜひそのルールを守ってみてください。
