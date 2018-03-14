type ImplicitUnknown = implicitUnknown

type Done = { } 
type Block = interface { apply -> Done }
type Boolean = {
     ifTrue(_: Block)
     ifFalse(_:Block)
     ifTrue(_:Block)false(_:Block)
     }

type t0 = interface { } 
type t01 = interface { } 

assert(t0) isSubtypeOf (t01)
assert(t01) isSubtypeOf (t0)
assert(t0) isSubtypeOf (t0)
assert(t01) isSubtypeOf (t01)

type ta = interface { a } 
type ta1 = interface { a }

assert(ta) isSubtypeOf(ta1) 
assert(ta1) isSubtypeOf(ta) 
assert(ta) notEqualsType(ta1)

assert(ta) isSubtypeOf(t0)
assert(t0) notSubtypeOf(ta)

type tb = interface { b } 
type tab = interface { a
                       b 
                       }

assert(tb) isSubtypeOf(t0)
assert(t0) notSubtypeOf(tb)
assert(tab) isSubtypeOf(ta)
assert(tab) isSubtypeOf(tb)
assert(ta) notSubtypeOf(tb)
assert(tb) notSubtypeOf(ta)
assert(ta) notSubtypeOf(tab)
assert(tb) notSubtypeOf(tab)
assert(tab) isSubtypeOf(tab)


type mUU = interface { m(_) } 
type mUA = interface { m(_) -> ta }
type mUB = interface { m(_) -> tb }
type mUAB = interface { m(_) -> tab }

type mAU = interface { m(_ : ta) }
type mBU = interface { m(_ : tb) }
type mABU = interface { m(_ : tab ) }

type mAA = interface { m(_ : ta) -> ta }
type mAB = interface { m(_ : ta) -> tb }
type mAAB = interface { m(_ : ta) -> tab }

assert(mUU) isSubtypeOf(mUU)
assert(mUU) isSubtypeOf(mUA)
assert(mUA) isSubtypeOf(mUU)
assert(mUA) notSubtypeOf(mUB)
assert(mUB) notSubtypeOf(mUA)
assert(mUAB) isSubtypeOf(mUA)
assert(mUAB) isSubtypeOf(mUAB)
assert(mUA) notSubtypeOf(mUAB)

assert(mAA) isSubtypeOf(mUU)
assert(mUU) isSubtypeOf(mAA)

assert(mAU) notSubtypeOf(mBU)
assert(mBU) notSubtypeOf(mAU)
assert(mABU) notSubtypeOf(mAU)
assert(mABU) notSubtypeOf(mBU)
assert(mAU) isSubtypeOf(mABU)
assert(mABU) isSubtypeOf(mABU)
assert(mAU) isSubtypeOf(mABU)


type Generic[[T,U]] = interface { m(_:T) -> U } 

assert(Generic[[ta,tb]]) isSubtypeOf(mAB)
assert(mAB) isSubtypeOf(Generic[[ta,tb]]) 
assert(mAB) notEqualsType(Generic[[ta,tb]])  //equals e.g. defined on nodeID

type TransformerOfAAB = interface { 
   add(_: ta) -> Done
   remove(_ : ta) -> Boolean
   transform(_: ta) -> tab   
}

type Transformer[[T,U]] = interface {
   add(_: T) -> Done
   remove(_ : T) -> Boolean
   transform(_: T) -> U
}

assert(TransformerOfAAB) isSubtypeOf(Transformer[[ta,tab]])
assert(Transformer[[ta,tab]])  isSubtypeOf(TransformerOfAAB)



type Object = interface { } 

def o = Object

type List = interface { 
  car -> Object
  cdr -> List
}

type List1 = interface { 
  car -> Object
  cdr -> List1
}

assert(o) isEqualsType(o)
assert(Object) isEqualsType(Object)
assert(List) isEqualsType(List)
assert(Object) notEqualsType(List)
assert(List) notEqualsType(Object)


assert(Object) notSubtypeOf(List)
assert(List) isSubtypeOf(Object)
assert(List) isSubtypeOf(List)
assert(List) isSubtypeOf(t0)
assert(ta) notSubtypeOf(List)
assert(List) isSubtypeOf(List)
assert(List) isSubtypeOf(Object)

assert(List) isSubtypeOf(List1)
assert(List1) isSubtypeOf(List)

print "done"
