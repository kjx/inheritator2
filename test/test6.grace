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


print "DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE "
