import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import collection.mutable.Stack

object Macros {
  // macro definition is a normal function with anything you fancy in its signature
  // its body, though, is nothing more that a reference to an implementation
  def printf(format: String, params: Any*): Unit = macro printf_impl

  // macro implementation must correspond to macro definitions that use it
  // required signature is quite involved, but don't be scared
  // if the compiler is unhappy, it will print the signature it wants in the error message
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    // instead of usual import c.universe._, import treehugger.forest._.
    import treehugger.forest._
    import definitions._
    import treehuggerDSL._
    import c.{universe => u}

    // this is a simple scalac AST => treehugger AST conversion.
    // currently it handles only the literals.
    def toTree(tree: u.Tree): Tree = tree match {
      case u.Literal(u.Constant(value)) => LIT(value)
    }

    // first of all, we parse the provided format string
    // macros run during the compile-time, so they operate on trees, not on values
    // this means that the format parameter of our macro will be a compile-time literal
    // not an object of type java.lang.String.
    val s_format = toTree(format.tree) match {
      case lit: Literal => lit.value.value.toString
    }

    // here we jump straight into the compiler
    // the paragraph below creates temporary vals that precompute expressions being formatted
    // to learn more about dynamic generation of Scala code, take a look at our slides:
    // http://scalamacros.org/talks/2012-04-28-MetaprogrammingInScala210.pdf
    val evals = ListBuffer[Tree]()
    def precompute(value: Tree, tpe: Type): Tree = {
      val freshName: String = c.fresh("eval$")

      // before:
      // evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      
      // after:
      evals += (VAL(freshName, tpe) := value)
      REF(freshName)
    }

    // nothing fancy here, just bread and butter AST manipulations
    // extract trees from parameters of a macro, decompose/analyze and transform them
    // note how we get a hold of Scala types that correspond to Int and String
    // this works for a small set of core types
    // but in most cases you will have to create types by yourself
    // read up the aforementioned slides to learn more about types
    val paramsStack = Stack[Tree]((params map (_.tree) map {toTree}): _*)
    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop, IntClass)
      case "%s" => precompute(paramsStack.pop, StringClass)
      case "%%" =>
        // before:
        // Literal(Constant("%"))

        // after:
        LIT("%")
      case part => LIT(part)
    }

    // before:
    // val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    // Block(stats.toList, Literal(Constant(())))

    // after:
    // no need for reification to print thing out
    val tree = BLOCK(evals ++
      (refs map { ref =>
        REF(Predef_print) APPLY(ref)
      })
    )
    val toMacroTree = treehugger.MacroBridge(c).toMacroTree
    c.Expr[Unit]((tree transform toMacroTree).asInstanceOf[c.Tree])
  }
}
