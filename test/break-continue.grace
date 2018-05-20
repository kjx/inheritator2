method loop (cond) doWithExits (block) {
  def break = { 
      return "loop return"
     }
  while {cond.apply} do {
    doWithExit { continue -> 
      block.apply( break, continue )
    } 
  }
}


method doWithExit (block) {
 block.apply { 
   return "doWithExit Return" }
}



var foo := 0

loop {foo < 10} doWithExits {
  break, continue ->
    foo := foo + 1
    print "A {foo}"
    if (foo == 2) then {continue.apply}
    if (foo == 4) then {break.apply}
    print "B {foo}"
}

