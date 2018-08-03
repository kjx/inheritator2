import "platform/KernanCompiler" as kc
import "combinator-collections" as c
inherit c.abbreviations
import "utility" as utility
use utility.exports
import "kernan-translator" as translator

var objectModel is public //evil dependency injection

type ASTNode = interface { } 

def modules = dictionary[[String, ASTNode]]

def moduleIsBeingLoaded = object { method isLoaded { false } } 

method loadModule(name : String) { 
  def mod = modules.at(name) ifAbsent {
      modules.at(name) put(moduleIsBeingLoaded)
      def newModuleKernanTree = kc.translateFile(name ++ ".grace")
      def newModuleCommonTree = translator.translate(newModuleKernanTree)
      def newModule = newModuleCommonTree.eval(objectModel.context)
      modules.at(name) put(newModule)
      return newModule
  }

  if (!mod.isLoaded) then {error "Module {name} is Loading - circular import" }
  
  return mod
}

method loadModulesFromArguments {
  for (kc.args) do { fileName ->
    def nameSize = fileName.size
    def baseName = 
      if ((fileName.substringFrom(nameSize - 5) to(nameSize)) == ".grace")
        then { fileName.substringFrom(1)to(nameSize - 6) }
        else { fileName }
    loader.loadModule( baseName )
  }
}


method installIntrinsicModule(intrinsicModuleObject) {
  modules.at(INTRINSICMODULE) put(intrinsicModuleObject)
}
