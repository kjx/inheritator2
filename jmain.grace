import "platform/KernanCompiler" as kc
import "jloader" as loader
import "jtranslator" as translator
import "jeval" as jeval
translator.jast := jeval.singleton

for (kc.args) do { fileName ->
   def nameSize = fileName.size
   def baseName = 
       if ((fileName.substringFrom(nameSize - 5) to(nameSize)) == ".grace")
         then { fileName.substringFrom(1)to(nameSize - 6) }
         else { fileName }
   loader.loadModule( baseName )
}

