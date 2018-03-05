method foo[[x]] { print "foo {x}" }
foo[[3]]
foo

class boxx[[T]] {
  print(T)
}

boxx[[3]]
boxx

method bar { }

//bar[[3]]
