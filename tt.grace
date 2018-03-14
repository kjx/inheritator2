type Object = interface { } 

type List = interface { 
  car -> Object
  cdr -> List
}

def o = Object 
assert(o) isEqualsType(o)
assert(Object) isEqualsType(Object)
assert(List) isEqualsType(List)
assert(Object) notEqualsType(List)
assert(List) notEqualsType(Object)


assert(Object) notSubtypeOf(List)
assert(List) isSubtypeOf(Object)
assert(List) isSubtypeOf(List)
