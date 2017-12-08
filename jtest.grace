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
