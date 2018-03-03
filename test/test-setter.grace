class sup {
    var x := 3 
}

class sub { 
   inherit sup
      alias y = x
      alias y():=(_) = x():=(_)
      exclude x
      exclude x():=(_)
}

sub
