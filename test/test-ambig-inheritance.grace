

class sup { 
   method foo { "super foo" } 
}

def o = object { 
   method foo { "outer foo" } 
   class sub { 
      inherit sup
      method test { foo } 
   }

}

print(o.sub.test)

//MG crashes (which is correct)
//kernan doesn't (which is wrong)
