

method one { 
  print "11"
  object {
    method doit { "method on one" }
    print "12"
  }
}


method two { 
  print "21"
  object {
    method doit { "method on two" }
    print "22"
  }
}

method three { 
  print "31"
  object {
    method doit { "method on three" }
    print "32"
  }
}

method four { 
  print "41"
  object {
    method doit { "method on four" }
    print "42"
  }
}


class test {
  inherit one 
  //use two
  inherit three
  //use four 
}



print(test.doit)
