// 構文木の要素の定義
sealed class Term()
class Lambda(val v: String, val body: Term) : Term()
class Id(val name: String) : Term()
class Apply(val fn: Term, val arg: Term) : Term()
class Let(val v: String, val defn: Term, val body: Term) : Term()
class Letrec(val v: String, val defn: Term, val body: Term) : Term()

// 型にかかわる要素（型変数、型演算子）の定義
sealed class Type()

class TypeVariable() : Type() {
    companion object {
      var nextVariableName: Char = 'a'
    }
    val name: String by lazy { "${nextVariableName++}" }
    var instance: Type = UnknownT
    override fun toString(): String = name
}

open class TypeOperator(val name: String, val types: List<Type>) : Type() {
    override fun toString() = name + " " + types.map { it.toString() }.joinToString(" ")
}

// 具体的な型をいくつか定義する
class FunctionT(name: String, types: List<Type>) : TypeOperator(name, types) {
    constructor(fromType: Type, toType: Type) :  this("->", listOf(fromType, toType))
    override fun toString() = types[0].toString() + "->" + types[1].toString()
}

val IntegerT = TypeOperator("int",  emptyList())
val BooleanT = TypeOperator("bool", emptyList())
val UnknownT = TypeOperator("unknown", emptyList())

// 与えられた構文木の型を推論する。
// 推論の上で重要な働きをするのは、Apply （関数の適用）の処理。
// 「apply される関数の型」 と 「引数の型 -> 結果の型」 が一致しなければならない ということを利用し、
// これらを unify にかけていくことで不明な型変数の型を一つずつ特定していく
fun analyse(node: Term, env: Map<String, Type>, nonGeneric: Set<Type> = emptySet()): Type {
    return when (node) {
        is Id -> getType(node.name, env, nonGeneric)
        is Apply -> {
            val funType = analyse(node.fn, env, nonGeneric)
            val argType = analyse(node.arg, env, nonGeneric)
            val resultType = TypeVariable()
            unify(FunctionT(argType, resultType), funType)
            resultType
        }
        is Lambda -> {
            val argType = TypeVariable()
            val resultType = analyse(node.body, env + (node.v to argType), nonGeneric + argType)
            FunctionT(argType, resultType)
        }
        is Let -> {
            val defnType = analyse(node.defn, env, nonGeneric)
            analyse(node.body, env + (node.v to defnType), nonGeneric)
        }
        is Letrec -> {
            val newType = TypeVariable()
            val newEnv = env + (node.v to newType)
            val defnType = analyse(node.defn, newEnv, nonGeneric + newType)
            unify(newType, defnType)
            analyse(node.body, newEnv, nonGeneric)
        }
    }
}

fun getType(name: String, env: Map<String, Type>, nonGeneric: Set<Type>): Type {
    return env[name]?.let {
		    fresh(it, nonGeneric)
    } ?: if (isIntegerTLiteral(name))
        IntegerT
    else
        throw Exception("Undefined symbol ${name}")
}

// 一時的に使うだけの型変数を処理するため、generic な型変数は共有しそうでないものはコピーした新しい型を作る
fun fresh(t: Type, nonGeneric: Set<Type>): Type {
    val mappings = mutableMapOf<Type, Type>()
    fun freshrec(tp: Type): Type {
        val p: Type = prune(tp)
        return when(p) {
            is TypeVariable ->
                if (isGeneric(p, nonGeneric))
                    mappings.getOrPut(p, { TypeVariable() })
                else p
            is TypeOperator ->
            	TypeOperator(p.name, p.types.map { freshrec(it) })
        }
    }
    return freshrec(t)
}

// 右辺 t1 と左辺 t2 は同じ型 という条件のもと、t1, t2 の中に不明な型変数があらわれたら反対側の辺の型を代入していく
fun unify(t1: Type, t2: Type) {
    val a = prune(t1)
    val b = prune(t2)
    if (a is TypeVariable) {
        if (a != b) {
            if (occursInType(a, b))
            	throw Exception("recursive unification")
            else
            	a.instance = b
        }
    } else if (a is TypeOperator && b is TypeVariable) {
        unify(b, a)
    } else if (a is TypeOperator && b is TypeOperator) {
        if (a.name != b.name || a.types.count() != b.types.count()) {
            throw Exception("Type mismatch ${a} != ${b}")
        }
        a.types.zip(b.types).forEach { (p,q) -> unify(p, q) }
    }
}

// tの型が推論できていれば、推論結果の型に展開する
fun prune(t: Type): Type =
    if (t is TypeVariable && t.instance != UnknownT) {
        t.instance = prune(t.instance)
        t.instance
    } else t

fun isGeneric(v: Type, nonGeneric: Set<Type>) = !occursIn(v, nonGeneric)

fun occursIn(t: Type, types: Iterable<Type>) = types.any { occursInType(t, it) }

fun occursInType(v: Type, type2: Type): Boolean {
    val prunedType2 = prune(type2)
    return if (prunedType2 == v) true
    else if (prunedType2 is TypeOperator) occursIn(v, prunedType2.types)
    else false
}

fun isIntegerTLiteral(name: String) = name.toLongOrNull() != null

// いくつかの構文で型を推論するテスト
fun main(args: Array<String>) {
    val var1 = TypeVariable()
    val env = mapOf(
        "true" to BooleanT,
        "false" to BooleanT,
        "if" to FunctionT(BooleanT, FunctionT(var1, FunctionT(var1, var1))),
        "prev" to FunctionT(IntegerT, IntegerT),
        "zero" to FunctionT(IntegerT, BooleanT),
        "times" to FunctionT(IntegerT, FunctionT(IntegerT, IntegerT))
    )

    listOf(
        Id("5"),              // 5 : int
        Lambda("n", Id("5")), // n -> 5 : a->int
        Lambda("n", Lambda("m", Id("5"))), // n -> m -> 5 : a->b->int
        Let("dec", Lambda("n", Apply(Id("prev"), Id("n"))), // fun dec(n) = n - 1; dec(1)
           Apply(Id("dec"), Id("1"))),
		Letrec("factorial",   // fun factorial(n) = if zero(n) 1 else n * factorial(n-1): int
           Lambda("n",
               Apply(Apply(Apply(Id("if"),
                   Apply(Id("zero"),Id("n"))),
                   Id("1")),
                   Apply(Apply(Id("times"),Id("n")),Apply(Id("prev"),Id("n"))))),
           Apply(Id("factorial"), Id("5")))
    ).forEach {
        println(prune(analyse(it, env, emptySet<Type>())))
    }
}
