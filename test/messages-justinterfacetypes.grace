type Secure = interface { 
    iAmBrandedSecure  
}

type Message = interface {
    asString    
}

type SecureMessage = interface {
    iAmBrandedSecure  
    asString    
}

class message {
    method asString { "I am a message" }   
}

class secureMessage {
    method asString { "I am a SECURE message" }   
    method iAmBrandedSecure { Exception.raise "crash" }
}

class listener {
    method accept (m : Message) {print "listener accepts {m}" }
    method secure (m : SecureMessage) {print "listener securely accepts {m}" }
    
}

def l = listener
def m = message
def s = secureMessage

l.accept(m)
l.accept(s)
l.secure(s)
l.secure(m)


