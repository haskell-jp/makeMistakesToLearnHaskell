# 入力した値の内容に応じて処理を分ける (1)

大きな数を受け取るとびっくり😲するプログラムを書きましょう:

1. 標準入力から1行の入力を受け取ります。
1. 入力した文字列を数値として解釈して、
    1. 数値が10以上であれば「Very big!」と出力してください。
    1. 数値が100以上であれば「Extremely big!!」と出力してください。
    1. それ以外の場合は、入力した数値をそのまま`print`してください。

## 注意事項

- 入力された文字列は10進数で書かれた整数として解釈してください。
- それ以外の形式の文字列が入力されることを想定する必要はありません。

## 必要な知識

### 条件演算子・`Bool`型

まずは「数値が10以上であれば～」などといった「条件」を扱えるようにするために、Haskellにおいて数値（など）を比較するための演算子を紹介します。おおむね他のプログラミング言語と同じなので、今回の課題では使わないものも含めて覚えちゃいましょう。

まずは今回の課題でも使う「～以上」から:

```haskell
-- ～以上であればTrue（「より大きい」か「等しい」）
ghci> 1 >= 2
False
ghci> 2 >= 2
True
ghci> 3 >= 2
True

-- ～以下であればTrue（「より小さい」か「等しい」）
ghci> 1 <= 2
True
ghci> 2 <= 2
True
ghci> 3 <= 2
False
```

（💡コツ: `>=`や`<=`といった記号を`=>`や`=<`と間違えて覚えてしまわないようにするためには、「≧」や「≦」の書き順を思い出しましょう。`>`と`<`を先に書きますよね？）

それから`>=`や`<=`から等号を外したものを:

```haskell
-- ～を越えていればTrue
ghci> 1 > 2
False
ghci> 2 > 2
False
ghci> 3 > 2
True

-- ～未満であればTrue
ghci> 1 < 2
True
ghci> 2 < 2
False
ghci> 3 < 2
False
```

そしてもっとよく使うであろう「等しい」と「等しくない」を:

```haskell
-- ～と等しければTrue
ghci> 1 == 2
False
ghci> 2 == 2
True
ghci> 3 == 2
False

-- ～と等しくなければTrue
ghci> 1 /= 2
True
ghci> 2 /= 2
False
ghci> 3 /= 2
True
```

「～と等しくなければ`True`」が`/=`となっている点に注意してください。他の多くのプログラミング言語では`!=`と感嘆符（いわゆるびっくりマーク）で始まるところ、Haskellでは`/=`とスラッシュで始めています。「≠」を模しているのでしょう。個人的に、感嘆符`!`が否定を表すことに長年違和感を覚えていたので、Haskellのこの仕様を知ったときはうれしかったです。

さて、ここまで何の断りもなく`True`や`False`に触れていました。これらはおなじみ「真偽値」（または「真理値」、「ブール値」など）と呼ばれる値です。Haskellでは`Bool`という名前の型の値で、先ほどまで登場したとおり`True`か`False`という値で表現されます。名前のとおり、「正しい」ことを表すのが`True`で「正しくない」ことを表すのが`False`ですね。

`:t`を使うと、確かに`True`と`False`は`Bool`型の値であることがわかります:

```haskell
ghci> :t True
True :: Bool

ghci> :t False
False :: Bool
```

### `if`式

続いて、「数値が10以上であれば～」などといった「条件」を扱う上で欠かせないもう一つの要素、`if`式を紹介しましょう。「数値が10以上であれば～」という「条件」が「正しいか正しくないか」を表すのが真偽値、`Bool`型の値です。それに対して`if`式は「数値が10以上であれば～」という「条件」が実際に正しかったとき・正しくなかったとき、それぞれにおいて「何をするのか」を指定するための式です。

以下のような構文で使用します:

```haskell
if <Bool型の値>
  then <Trueの場合の値>
  else <Falseの場合の値>
```

`<Bool型の値>`、`<Trueの場合の値>`、`<Falseの場合の値>`、それぞれを当てはめてみましょう:

```haskell
if True
  then "Right!"
  else "Wrong!"
```

この場合`<Bool型の値>`は`True`なので、GHCiに貼り付ければ`"Right!"`という文字列が変えるはずです。やってみましょう！

```haskell
ghci> if True

<interactive>:8:8: error:
    parse error (possibly incorrect indentation or mismatched brackets)
ghci>   then "Right!"

<interactive>:9:3: error: parse error on input ‘then’
ghci>   else "Wrong!"

<interactive>:10:3: error: parse error on input ‘else’
```

おっと... エラーになってしまいました。これは、GHCiが改行を入力した時点で「入力が終わった！」と見なして、入力を解釈しようとしてしまうからです。

例えば`if True`を入力した時点で出てきた、

```haskell
<interactive>:8:8: error:
    parse error (possibly incorrect indentation or mismatched brackets)
```

というエラーは、`if True`の時点で`if`式が終わったとGHCiが見なしてしまったために出たエラーです。`if`式には`then`や`else`が必要なのに、早とちりしてしまったわけですね。

##### GHCiの`:{`と`:}`で、複数行の入力をできるようにする

これを回避するためには、GHCiに「これから複数行の入力が始まるよ！」と教えてあげましょう。そのためには`:{`と`:}`というGHCi専用のコマンドを使います:

```haskell
ghci> :{
ghci| if True
ghci|   then "Right!"
ghci|   else "Wrong!"
ghci| :}
"Right!"
```

`:{` で複数行の入力を始めて、`:}`で終了させます。GHCiの特殊なコマンドなので、いずれもコロン`:`で始める点に注意してください！

今度は期待どおり`"Right!"`が返ってきましたね。続けて他にもいろいろな例を試してみましょう:

```haskell
ghci> :{
ghci| if False
ghci|   then "Right!"
ghci|   else "Wrong!"
ghci| :}
"Wrong!"

ghci> :{
ghci| if 1 < 2
ghci|   then "Right!"
ghci|   else "Wrong!"
ghci| :}
"Right!"

ghci> :{
ghci| if 1 > 2
ghci|   then "Right!"
ghci|   else "Wrong!"
ghci| :}
"Wrong!"
```

#### `if`式の注意事項・型エラーを読んでみる

`if`式の使い方は以上のとおりですが、他のプログラミング言語の`if`式や`if`文に慣れているみなさんには注意していただきたいことがいくつかあります。

一つは、Haskellの`if`式では`else`節、つまり`<Falseの場合の式>`を指定する箇所が必須だということです。試しに省略してみましょう:

```haskell
ghci> :{
ghci| if True
ghci|   then "Only True!"
ghci| :}

<interactive>:28:20: error:
    parse error (possibly incorrect indentation or mismatched brackets)
```

確かにエラーになってしまいましたね。これまでにも何度か登場した`parse error`というエラーは、「構文エラー」と呼ばれるエラーです。入力したソースコードがHaskellの構文（文法）に正しく則っていないため「ちゃんと解釈（パース、英語でparse）できない！」とGHCが嘆いているのです。`else`節がない`if`式は構文に正しく従えていない、ということなのです。

なぜこのようなルールになっているのでしょう？Haskellの関数は、原則として、何らかの形で決まった型の値を返さなければならないことになっています。値を返さなければ、返した値を受け取ってまた別のことをするはずの関数が、どうしようもなくなってしまうのです。`if`式も、`<Bool型の値>`を受け取って`<Trueの場合の値>`・`<Falseの場合の値>`のどちらかを返す関数だと解釈できるので、関数に倣って`<Bool型の値>`が`True`であっても`False`であっても値を返すよう設計されているのです。

それから、`<Trueの場合の値>`・`<Falseの場合の値>`両方が同じ型の値でなければならない、というルールも忘れてはなりません。

こちらも実際にエラーを起こしてみましょう。`<Trueの場合の値>`を文字列に、`<Falseの場合の値>`として`Bool`型の値である`False`を指定してみます:

```haskell
ghci> :{
ghci| if True
ghci|   then "This is a string in a 'then' clause"
ghci|   else False
ghci| :}

<interactive>:33:8: error:
    ? Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
    ? In the expression: False
      In the expression:
        if True then "This is a string in a 'then' clause" else False
      In an equation for ‘it’:
          it = if True then "This is a string in a 'then' clause" else False
```

上記のような`Couldn't match expected type ...`で始まるエラーは「型エラー」と呼ばれるエラーです。型エラーには他の種類のメッセージもありますが、概ね「`type`」や「`instance`」という単語が含まれていれば型エラーだと考えて差し支えありません。「型エラー」は「構文エラーはない。つまり入力したソースコードは正しくHaskellの構文に従っている。でも入力した式の型がおかしい！」というエラーです。

今回発生したエラーを例にしましょう。エラーメッセージの冒頭にある、

```haskell
Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
```

というメッセージに注目してください。「`[Char]`型を期待してたけど、実際の型は`Bool`じゃないか！」というGHCの驚きが伝わるでしょうか？ここでいう、`[Char]`型とは文字のリスト型、つまり文字列型のことです。

それから、`In the expression: False`で始まる行以降は、型エラーがどの式で発生したかを示しています。この場合、`if`式の`<Falseの場合の値>`の箇所に書いた`False`のことですね。

つまり、GHCは文字列型のつもりで`if`式の`<Falseの場合の値>`を見に行ったら、実際には`Bool`型の値である`False`があった、だから型エラーと見なしたのです。`if`式の仕様により、`<Trueの場合の値>`・`<Falseの場合の値>`は両方が同じ型でなければならないことから、GHCは`<Trueの場合の値>`の型が`[Char]`型であることを確認した上で、`<Falseの場合の値>`も`[Char]`型だろうと仮定したために、このようなエラーとなりました。`expected type`（期待する型）が`[Char]`で`actual type`（実際の型）が`Bool`となるのはそういう理由なのです。

少し脱線ですが、GHC（や、その他の一般的な型推論のアルゴリズム）では、型推論を行う際、入力された式を順番に読んでいって**先に現れた式の型を「正しい式」**と仮定します。今回のように`if`式の場合`<Trueの場合の値>`である`[Char]`が先に現れたので、`[Char]`を`expected type`と解釈したわけです。

しかしそもそも、なぜこんな制約があるのでしょう？それは「`else`節を指定する箇所が必須」である理由と関係しています。`if`式もHaskellにおける普通の関数と同様、必ず値を返すことになっています。値を返すと言うことは、その返した値を受け取る関数が別に存在するということです。それらの関数はHaskellの仕様上、一つの決まった型の値しか受け取れません。これは静的型付け言語の原則です。従って、仮に`if`式が`True`か`False`かで異なる型の値を返してしまうと、返した値を受け取る関数が想定していない型の値を受け取ることになってしまい得るのです。これでは原則に従うことができません。

#### `if`式を入れ子に書く: 二つのスタイル

今回の課題では、「数値が10以上であれば」という条件と、「数値が100以上であれば」という二つの条件を同時に扱うことになります。そうした場合他のプログラミング言語では、C言語などのように`else`節に入れ子になった`if`や、Pythonなどのように`elif`節といった専用の構文を使用するでしょう。Haskellの場合「`elif`節」のような専用の構文はなく、例えば次のように、`else`節にもう一つの`if`をぶら下げるような形で書いてください:

```haskell
-- xが負の数であれば "Negative!" を、正の数であれば "Positive!" を、
-- どちらでもない、つまり0であれば "Zero!" を返す

ghci> x = 0
ghci> :{
ghci| if x < 0
ghci|   then "Negative!"
ghci|   else if x > 0
ghci|     then "Positive!"
ghci|     else "Zero!"
ghci| :}
"Zero!"
```

しかしこの書き方だと、`if`式が一つ増える度にインデントが深くなってしまうので、次のようなスタイルで書くとよいでしょう:

```haskell
ghci> :{
ghci| if x < 0 then
ghci|   "Negative!"
ghci| else if x > 0 then
ghci|   "Positive!"
ghci| else
ghci|   "Zero!"
ghci| :}
"Zero!"
```

`<Bool型の値>`と同じ行に`then`を動かした上で、`else`節を最初の`if`と同じ桁にそろえることで、インデントが深くなってしまうのを防ぐ、というスタイルです。

このスタイルは実際のところあまり私は見たことがありません。ここでは割愛しますが、実は他のやり方をGHCが提供してくれているからです。しかし、私は個人的にはこちらを推奨します。こちらの方が、単純な`if`式の組み合わせのみで実現できるという意味で、よりシンプルだからです。なお、今回の課題を解く上では、どちらのスタイルでも構いません。
