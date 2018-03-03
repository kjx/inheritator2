import "platform/KernanCompiler" as kc
import "combinator-collections" as c
inherit c.abbreviations
import "jruntime" as runtime
def objectModel = runtime.singleton
import "jcommon" as common
use common.exports
import "jtranslator" as translator

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



