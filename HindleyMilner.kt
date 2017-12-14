sealed class Term()
data class Lambda(val v: String, val body: Term) : Term()
data class Id(val name: String) : Term()
data class Apply(val fn: Term, val arg: Term) : Term()
data class Let(val v: String, val defn: Term, val body: Term) : Term()
data class Letrec(val v: String, val defn: Term, val body: Term) : Term()

sealed class Type()
class TypeVariable() : Type() {
    companion object {
        var nextVariableId: Int = 0
        var nextVariableName: Char = 'a'
    }
    val id: Int = nextVariableId++
    fun name() {
        if (name == null) {
            name = nextVariableName++
        }
        name
    }
    var name: Char? = null
    var instance: Type? = null
}

open class TypeOperator(val name: String, val types: List<Type>) : Type()
class Function(name: String, types: List<Type>) : TypeOperator(name, types) {
    constructor(fromType: Type, toType: Type) :  this("->", listOf(fromType, toType))
}

val INTEGER = TypeOperator("int",  emptyList())
val BOOL    = TypeOperator("bool", emptyList())

fun analyse(node: Term, env: Map<String, Type>, nonGeneric: Set<Type> = emptySet()): Type =
    when (node) {
        is Id -> getType(node.name, env, nonGeneric)
        is Apply -> {
            val funType = analyse(node.fn, env, nonGeneric)
            val argType = analyse(node.arg, env, nonGeneric)
            val resultType = TypeVariable()
            unify(Function(argType, resultType), funType)
            resultType
        }
        is Lambda -> {
            val argType = TypeVariable()
            val resultType = analyse(node.body, env + (node.v to argType), nonGeneric + argType)
            Function(argType, resultType)
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

fun getType(name: String, env: Map<String, Type>, nonGeneric: Set<Type>): Type {
    val a = env[name]
    return if (a != null)
		fresh(a, nonGeneric)
    else if (isIntegerLiteral(name))
        INTEGER
    else
        throw Exception("Undefined symbol")
}

fun fresh(t: Type, nonGeneric: Set<Type>): Type {
    val mappings = mapOf<Type, Type>()
    fun freshrec(tp: Type): Type {
        val p: Type = prune(tp)
        return when(p) {
            is TypeVariable ->
                if (isGeneric(p, nonGeneric)) {
                    val q = mappings[p]
                    if (q == null) {
                        val r = TypeVariable()
                        mappings[p] = r
                        return r
                    } else q
                } else p
            is TypeOperator ->
            	TypeOperator(p.name, p.types.map { freshrec(it) })
        }
    }
    return freshrec(t)
}

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
        a.types.zip(b.types).forEach { unify(it[0], it[1]) }
    }
}

fun prune(t: Type): Type {
    if (t is TypeVariable) {
        if (t.instance != null) {
            t.instance = prune(t.instance)
        }
        t.instance
    } else t
}

fun isGeneric(v: Type, nonGeneric: Set<Type>): Boolean = !occursIn(v, nonGeneric)

fun occursIn(t: Type, types: Iterable<Type>): Boolean = types.any { occursInType(t, it) }

fun occursInType(v: Type, type2: Type): Boolean {
    val prunedType2 = prune(type2)
    if (prunedType2 == v) true
    else if (prunedType2 is TypeOperator) occursIn(v, prunedType2.types)
    else false
}

fun isIntegerLiteral(name: String): Boolean = toLongOrNull(name) != null
