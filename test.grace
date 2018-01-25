dialect "jkernandialect"

print "one"
print 2
print (2 + 2)
print (type {
  foo
  bar
  baz(x,y)
})
def x = 2
print "here"
print (x)
print"there"
print (2 + x)
print (x + x)
def y = "hello"
print (y)
var z := "oneone"
print (z)
z := 22
print (z)
method foo {"foomethod" }
print (foo)
print { "a block" }
print { "a block" }

method bar { return "barreturn" }
print (bar)

method blockescape {
  def x = { return "blockescapeOK" }
  def y = { "ping" }
  def z = { "pong" }
  print (y.apply)
  print (x.apply)
  print (z.apply)
}
print (blockescape)

print "bubble1"
method bubble(x,y) { (x + x) + y }
print (bubble(5,10))
print (bubble(20,10))
print "bubble2"

print ({ x -> x + x }.apply(22))

print ({ x -> x + x }.apply(44))

var counter := 0
def bump = { print "BUMP!"
             counter := counter + 2
             counter }
print (bump.apply)
print (bump.apply)
print (bump.apply)

var readc
var nextc
var prevc

{ 
  var counter2 := 0
  readc := {counter2}
  nextc := {counter2 := counter2 + 10}
  prevc := {counter2 := counter2 + 5}
}.apply

print (readc.apply)
print (nextc.apply)
print (readc.apply)
print (nextc.apply)
print (readc.apply)
print (prevc.apply)
print (readc.apply)

"objects"
def o1 = object {
   method one {"one"}
   method two {"two"}
}
print "objectsrun"
print (o1.one)
print (o1.two)

def o2 = object {
   method one {"ONE"}
   method two {"TWO"}
   method three { self.one }
}
print (o2.one)
print (o2.two)
print (o2.three)

def o3 = object {
  method right { "right" } 
  method wrong { "wrong" } 
  def o4 is public = object { 
      method inner { "inner" } 
  }
}

print (o3.right)
print (o3.o4.inner)
// o3.o4.wrong

def o8 = object {
   def b = a
   method a {"a"}
}
print (o8.a)


def o9 = object {
   method a {b}
   def b = "a"
}
print (o9.a)

def o5 = object { 
  //o5b
  method o5b {o5a}
  method o5a {o5v}
  def o5v = "o5v"
  print(o5b)
}



class superclass {
    method a { "A" }
    method b { "B" }   
}

trait sideways {
   method a { "TA" }
}

class subclass {
    inherit superclass
      exclude a
      alias d = a
    use sideways
    method b { "SB" } 
    method c { "C" }
}

def s = superclass
def t = subclass
print (s.a)
print (s.b)
print (t.a)
print (t.b)
print (t.c)
print (t.d)


class jast {
  class node {
    print "making node"
    //method JAST_NODE
  }
  class inh {
    inherit node
    //method INH { }

    print "making inh" 
  }
}
class jeval {
  inherit jast
    alias jnode = node
    alias jinh = inh

  //method JEVAL { } 

  class node {
    inherit jnode
    //method JEVAL_NODE { } 
    print "making jnode"
    def xxxxx = 3 
  }
  class inh {
    inherit jinh
    print "making jinh" 
    print(xxxxx)
  }

}
jast.inh
jeval.inh


class A {
  
  class Inner {
    method foo {
      bar
    }
  }
  
  method bar {
    return "via A"
  }
}
        

class B {
  inherit A.Inner
}


class C {
  class B2 { inherit B }

  method bar {
    return "via C"
  }
}

print(C.B2.foo)



print "DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE "

