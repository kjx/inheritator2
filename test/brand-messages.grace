

def secure = brand

type Secure = secure.Type

type Message = interface {
    asString    
}

def inner = object {
    class message {
        method asString { "I am a message" }   
    }
    
    class secureMessage is secure {
        method asString { "I am a SECURE message" }   
    }
    
    class listener {
        method accept (m : Message) {
             print "listener accepts {m}" 
             match (m) 
               case { sm : Secure -> print "({sm} is actually secure)" }
               case { _ -> print "({m} really isn't secure)" }
          }
        method secure (m : Secure & Message) {print "listener securely accepts {m}" }
    
    }
}

def l = inner.listener
def m = inner.message
def s = inner.secureMessage

l.accept(m)
l.accept(s)
l.secure(s)
l.secure(m)









////////////////////////////////////////////////////////////
//code below here belongs in a dialect
//but that would mean refactoring the existing definitions
//which was too much work last night.
//just ignore them...

class brand {
  method retainedAnnotation { } 
  method Type { brandType(self) }
}

class brandType(underlyingBrand) {
  method asString {"brandType"}
  method match(other) {primitiveBrandMatch(underlyingBrand,other)}
  method & (otherType) {AndPattern(self,otherType)}
}


class AndPattern(l,r) {
  method asString {"ANDY"}
  method match(other) {l.match(other) && r.match(other) }}



method match(x) case(b1) case (b2) {
   b1.match(x).ifTrue { return b1.apply(x) }
   b2.match(x).ifTrue { return b2.apply(x) }
   error "match/case falls through"
}
