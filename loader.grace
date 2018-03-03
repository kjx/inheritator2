import "platform/KernanCompiler" as kc
import "combinator-collections" as c
inherit c.abbreviations
import "object-model" as om
def objectModel = om.singleton
import "utility" as utility
use utility.exports
import "kernan-translator" as translator

type ASTNode = interface { } 

def modules = dictionary[[String, ASTNode]]

modules.at(INTRINSICMODULE) put(objectModel.intrinsicModuleObject)

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



