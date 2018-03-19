import "platform/KernanCompiler" as kc
import "combinator-collections" as c
import "object-model" as om
import "utility" as utility
import "kernan-translator" as translator
import "evaluator" as eval
import "typechecker" as typechecker
import "common-ast" as ast


inherit c.abbreviations
use utility.exports

translator.jast := eval.singleton

def objectModel = om.singleton

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



method checker(kernanAst) -> Done {
    def ast = translator.translate(kernanAst)


    print "this is the checker"
    print "and this is the ast:"
    print (ast)
    staticTypesVisitor.visitModule(ast)

}

class staticTypesVisitor {
    use ast.baseVisitor

    method visitExplicitRequest(node:RequestNode) {
        print "visiting request of {node.name}"
        def rcvrType = node.receiver.accept(self)
        if (rcvrType.isUnknown) then { return typeUnknown }
        def sig = rcvrType.signatureOf (node.name) ifAbsent {
            noSuchMethod(node) }
        checkArguments (node.arguments) against (sig.parameters)
        return sig.returnType
    }

    method checkArguments (args) against (types) {
        for (args) and (types) do { a, t -> checkThat (a) hasType (t) }
    }

    method checkThat(a) hasType(t) {
        if (a.accept(self).conformsTo(t).not) then {
            argument(a)notOfType(t)
        }
    }

    method visitNumberLiteral(node : NumberLiteral) -> T {
        // here we get the type object built from the intrinsic Number type.
        typechecker.numberType
    }

    method visitStringLiteral(node : StringLiteral) -> T {
        typechecker.stringType
    }
}

method noSuchMethod(node) {
    typeError "{node.name} does not exist in {node.receiver}" atNode (node)
}

method argument(a) notOfType(t) {
    typeError "{a.asString} does not have type {t}" atNode (a)
}

method typeError(message) atNode (node) {
    print "type error! { message } at { node }"
}
