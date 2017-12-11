dialect "jkernandialect"

"one"
2
2 + 2
type {
  foo
  bar
  baz(x,y)
}
def x = 2
"here"
x
"there"
2 + x
x + x
def y = "hello"
y
var z := "oneone"
z
z := 22
z
method foo {"foomethod" }
foo
{ "a block" }
{ "a block" }

method bar { return "barreturn" }
bar

method blockescape {
  def x = { return "blockescapeOK" }
  def y = { "ping" }
  def z = { "pong" }
  y.apply
  x.apply
  z.apply
}
blockescape

"bubble"
method bubble(x,y) { (x + x) + y }
bubble(5,10)
bubble(20,10)
"bubble"

{ x -> x + x }.apply(22)

{ x -> x + x }.apply(44)

var counter := 0
def bump = { counter := counter + 2
             counter }
bump.apply
bump.apply
bump.apply      

var readc
var nextc
var prevc

{ 
  var counter2 := 0
  readc := {counter2}
  nextc := {counter2 := counter2 + 10}
  prevc := {counter2 := counter2 + 5}
}.apply

readc.apply
nextc.apply
readc.apply
nextc.apply
readc.apply
prevc.apply
readc.apply

"objects"
def o1 = object {
   method one {"one"}
   method two {"two"}
}
"objectsrun"
o1.one
o1.two

def o2 = object {
   method one {"ONE"}
   method two {"TWO"}
   method three { self.one }
}
o2.one
o2.two
o2.three

def o3 = object {
  method wrong { "wrong" } 
  def o4 is public = object { 
      method inner { "inner" } 
  }
}

o3.wrong
o3.o4.inner
// o3.o4.wrong





"DONE DONE DONE DONE DONE DONE DONE"

