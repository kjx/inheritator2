//systems defs

class brand {
  print "WHOOPEE"
  method iAmBrand { }
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




def secure = brand

type Secure = secure.Type

type Message = interface {
    asString    
}

class message {
    method asString { "I am a message" }   
}

class secureMessage is secure {
    method asString { "I am a SECURE message" }   
}

class listener {
    method accept (m : Message) {print "listener accepts {m}" }
    method secure (m : Secure & Message) {print "listener securely accepts {m}" }
    
}

def l = listener
def m = message
def s = secureMessage

print "++++++++++++++++++++++++++++++++++++++++++++++++++++"
l.accept(m)
l.accept(s)
l.secure(s)
l.secure(m)


