def plusType = interface {
   +(_)
}

def fooType = interface { foo } 

def foo2Type = interface { foo(_,_) }

def fooBothType = interface {
            foo
            foo(_,_)
            }

def foo = object {
            method foo { "foo" }
            }

def foo2 = object {
            method foo(a,b) { "foo2 {a} {b}" }
            }

def fooBoth = object {
            method foo { "foo" }
            method foo(a,b) { "foo2 {a} {b}" }
            }


print(plusType.match( 3 ))
print(plusType.match( "three" ))
print (fooType.match(foo))
print (fooType.match(foo2))
print (foo2Type.match(foo2))
print (foo2Type.match(foo))
print (fooType.match(fooBoth))
print (fooType.match(3))
print (foo2Type.match(fooBoth))
print (foo2Type.match(3))
print (fooBothType.match(fooBoth))
print (fooBothType.match(foo2))

