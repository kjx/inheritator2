type ImplicitUnknown = implicitUnknown

type Done = { } 
type Block = interface { apply -> Done }
type Boolean = {
     ifTrue(_: Block)
     ifFalse(_:Block)
     ifTrue(_:Block)false(_:Block)
     }


type Foo = interface {
   x( _ : Foo)
}

type Bar = interface {
   x(_ : Bar) 
}


assert(Foo) isSubtypeOf(Foo)
assert(Bar) isSubtypeOf(Bar)

assert(Foo) isSubtypeOf(Bar)
assert(Bar) isSubtypeOf(Foo)


print "done"
