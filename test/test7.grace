class superclass {
    def a is public = "A"
}

def a = "aaaaaaaaaaaaaaaa"

class subclass {
    inherit superclass
    method foo { 
       a
    } 
}


print(subclass.foo)

