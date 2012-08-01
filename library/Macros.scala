import scala.reflect.makro.Context
import collection.mutable.ListBuffer
import collection.mutable.Stack

object Macros {
  // マクロ定義のシグネチャは好きなパラメータを受け取る普通の関数と同じだ。
  // しかし、本文は実装への参照のみとなる。
  def printf(format: String, params: Any*): Unit = macro printf_impl

  // マクロ実装のシグネチャはマクロ定義のものと対応する必要がある。
  // 一見複雑に見えるが、心配する必要はない。
  // もしコンパイラが不満なら、エラーメッセージで必要なシグネチャを教えてくれる。
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    // コンパイラ API は scala.reflect.makro.Context を通じて公開されている。
    // その最も重要な部分であるリフレクション API は c.universe よりアクセスすることができる。
    // 頻繁に使われるものの多くが含まれているため、import c.universe._ をインポートするのが慣例だ。
    import c.universe._

    // まず、渡された format 文字列をパースする。
    // マクロはコンパイル時に動作するため、値ではなく構文木に作用する。
    // そのため、このマクロに渡される format パラメータはコンパイル時のリテラルであり、
    // java.lang.String 型のオブジェクトではない。
    // これはまた、以下のコードでは printf("%d" + "%d", ...) では動作しないことを意味する。
    // なぜならこの場合は format は文字リテラルではなく 
    // 2つの文字リテラルの連結を表す AST となるからだ。
    // 任意のもので動作するようにこのマクロを改良するのは読者への練習問題としよう。
    val Literal(Constant(s_format: String)) = format.tree

    // ここからコンパイラに突入する。
    // すぐ下のコードでは一時的な val を作ってフォーマットされる式を事前に計算する。
    // 動的な Scala コードの生成に関してもっと詳しく知りたい場合は以下のスライドを参照:
    // http://scalamacros.org/talks/2012-04-28-MetaprogrammingInScala210.pdf
    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    // ここはありがちな AST に対する操作で、特に難しいことは行なっていない。
    // マクロのパラメータから構文木を抽出し、分解/解析して変換する。
    // Int と String に対応する Scala 型を手に入れている方法に注意してほしい。
    // これはコアとなるごく一部の型ではうまくいくが、
    // ほとんどの場合は自分で型を作る必要がある。
    // 型に関しての詳細も上記のスライド参照。
    val paramsStack = Stack[Tree]((params map (_.tree)): _*)
    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop, typeOf[Int])
      case "%s" => precompute(paramsStack.pop, typeOf[String])
      case "%%" => Literal(Constant("%"))
      case part => Literal(Constant(part))
    }

    // そして最後に生成したコードの全てを Block へと組み合わせる。
    // 注目してほしいのは reify の呼び出しだ。AST を手っ取り早く作成する方法を提供している。
    // reify の詳細はドキュメンテーションを参照してほしい。
    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }
}
