type Unknown = { }   //EVIL
type ImplicitUnknown = { }  //EVIL


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


print "about to Loop"

type Object = interface { } 

type List = interface { 
  car -> Object
  cdr -> List
}

assert(List) isSubtypeOf(t0)
assert(ta) notSubtypeOf(List)
assert(List) isSubtypeOf(List)
assert(List) isSubtypeOf(Object)

print "done"
