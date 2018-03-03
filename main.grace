import "platform/KernanCompiler" as kc
import "loader" as loader
import "kernan-translator" as translator
import "evaluator" as eval
translator.jast := eval.singleton

for (kc.args) do { fileName ->
   def nameSize = fileName.size
   def baseName = 
       if ((fileName.substringFrom(nameSize - 5) to(nameSize)) == ".grace")
         then { fileName.substringFrom(1)to(nameSize - 6) }
         else { fileName }
   loader.loadModule( baseName )
}

