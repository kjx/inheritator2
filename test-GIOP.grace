dialect "jkernandialect"
//Grace in one page. run here: http://www.cs.pdx.edu/~grace/minigrace/exp/
 
print "Hello World!"
 
// Comments & Layout
// comment to end of line 
 
// Definitions and Variables
def one = 1 // constant
def two : Number = 2 // constant with types
 
var i : Number := 13  // typed variable - note := 
var x := 4 // variable, dynamically typed
var z is readable, writable := "Z" // annotated to give public access
 
//Literals
1
16xF00F00
2x10110100
0xdeadbeef // Radix zero treated as 16 
true
false; // Booleans
"Hello World!"
"\t"
"1 + 2 = {1 + 2}" // Strings with interpolation
{two.something}
{ j -> print(j)} // Blocks (Lambdas) with parameters
 
// Requests
self // self request
x // implicit receiver named request, no arguments (reads variables and constants)
print "Hello world" // implicit receiver named request
"Hello".size // explicit name request 
"abcdefghi".substringFrom(3) to(6) // multipart request
1 + 2 * 3 // operators
!false // unary prefix operatiors!   Can't do that in Smalltalk.
"ab" ++ "cd" // string concatenation is ++
(true || false) && true // only precedence for + - * /
x := 22 // assignment request
 
 
// Control Structures
if (x == 22) then {print "YES"}              // if statements 
  elseif {x == 23} then {print "Maybe"}
  else {print "...nope..."}
 
for (2 .. 4) do { j -> print(j) } // ".." makes a range from Numbers
 
x := 10
while {x < 20} // note need a {block} here 
  do {print(x)
      x:= x + 3} 
 
// Switching & Matching
match (x) // Switch or Case statement 
  case { 0 -> print "zero" }   // literals 
  case { n : Number -> print "Number {n}" }  // type matches 
  case { s : String -> print "String {s}" }
  case { _ -> print "who knows?" }     // catch all 
 
 
// Methods
// Grace methods can be at the "top level"
method pi {3.141592634} //simple method 
method + (other) { other + self } // binary operator 
method prefix- {print "bing!"} //prefix unary operator

method from(n : Number) steps(s: Number) -> Number { //multiple names 
  print "from {n} steps {s}"
  return s  } 
method fromsteps(n : Number, s: Number) -> Number { //multiple arguments 
  print "from {n} steps {s}"
  return s  } 
 
// Objects
def fergus = object {  //make a new object 
  def colour is readable = "Tabby" 
  def name is readable = "Fergus"
  var miceEaten := 0
  method eatMouse {miceEaten := miceEaten + 1}
  method miaow {print "{name}({colour}) has eaten {miceEaten} mice"}
}
 
fergus.eatMouse
fergus.miaow
 
// Classes
class Cat(name') colour(colour') {  //class and its factory method
  def colour is readable = colour' // note primes on names
  def name is readable = name'
  var miceEaten := 0
  method eatMouse {miceEaten := miceEaten + 1}
  method miaow {print "{name}({colour}) has eaten {miceEaten} mice"}
}
Cat("Amelia") colour("Tortoiseshell").miaow
 
// Inheritance
class PedigreeCat(aName) colour(aColour) { //call superclass's factory 
  inherit Cat(aName) colour("Pedigree " ++ aColour)
    alias catMiaow = miaow
  var prizes := 0
  method winner {prizes := prizes + 1}
  method miaow is override {catMiaow
                            print "and won {prizes} prizes"}
}
 
def woopert = PedigreeCat("Woopert") colour("Siamese")
woopert.winner
woopert.winner
woopert.winner
woopert.miaow

// Types
type Cat = type { //**change to "interface"
  colour -> String
  name -> String 
  eatMouse
  miaow
} 
 
// Exceptions
def NegativeError = Exception.refine "NegativeError"  //make a new kind of exception
try { // ...     
  NegativeError.raise "-1 < 0" }   //raise (throw) an exception
   catch { e : Exception -> print "Error {e}" }  //catch particular exceptions
   catch { ne: NegativeError -> print "Negative Error {ne}" }
   finally { print "finally!" }    //always executed 
 
// Modules 
import "io" as io  // import module
io.output.write("Goodbye\n") // call code in module


