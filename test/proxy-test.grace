
def x = object {
               method hello { print "Hello Cruel World"  }
               //method goodbye {hello}
               //method myself {self}
               method skip {} // don't ask
               }


def px = proxy(x)

//px.hello

//px.goodbye

//px.myself.hello

//px.myself.goodbye

method doit(o) {
   o.hello
   send(px)
   o.hello
   }
   

doit(px)

