
import "platform/KernanCompiler" as kc

print "started diagprint" 

method checker(module) {
    
    for (module.body) do { e ->
     match(e)         
      case { _ : HasValue -> print "checking module {e}: {e.value}" }
      case { _ -> }            
    }
}

type HasValue = interface { value } 

print "done diagprint"



