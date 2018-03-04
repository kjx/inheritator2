
class familyOne {
  method xxx(a) { } 
}

class resolution {
  inherit familyOne
    exclude xxx(_)
}

class familyTwo {
  method xx { print "xx" } 
  method xxx(a) { print "xxx{a}" }
}


  //   inherit familyTwo
  //     alias superxx = xx
  //     alias superxxx(_) = xxx(_)

  //   method xx { superxx }
  //   method xxx(a) { superxxx(a) }



resolution

