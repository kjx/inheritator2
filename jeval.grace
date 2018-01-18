import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports
import "jruntime" as runtime
def ng is public = runtime.exports
import "jcommon" as common
use common.exports

//TODO  refactor / redesign jruntime.grace

//TODO top of dialect - do things continue on to the enclosing scope of the **dialect**
//TODO make a ngmodule object
//TODO make a standardGraceDialect object (special module, empty dialect), 
//   and set up the standard dialect e.g. with debugPrint and annotations and stuff
//TODO lexical lookup gets to a module, then does a lookup including inheriatnace to the dialect --- but does not do the dialect's lexical scope.  I guess you could call that lookupDialect!  (if it's not lookupInheritance)

//TODO - shadowing checks  (checkForShadowing in jruntime)


//TODO alias clauses annotations change privacy? 
//  (need to fix kernan parser)

//TODO types! 
//TODO block matching
//TODO dynamic typechecks on argumenets - and results
//TODO add Kernan primitives for argument access and parsing to execution tree
//TODO   and then convert away from dialect checker to work explicitly
//TODO add Kernan primitives to let us link through to incoming source code
//TODO exceptions
//TODO refactor AST, redesign class names, add progn/sequence properly visitable
//TODO correct canonical names of of assignment methods/requests (wash your dog first)



//method jdebug(block) {block.apply}
method jdebug(block) { } 

method DEBUG(block) {block.apply}
method debugPrint(string) { } 




class jeval {
  inherit jm.jast
      alias jNodeAt(_) = nodeAt(_)
      alias jStringLiteralNode(_) at(_) = stringLiteralNode(_) at(_)
      alias jNumberLiteralNode(_) at(_) = numberLiteralNode(_) at(_)
      alias jInterfaceNode(_) at(_) = interfaceNode(_) at(_) 
      alias jExplicitRequestNode(_,_,_,_) at(_) = explicitRequestNode(_,_,_,_) at(_)
      alias jImplicitRequestNode(_,_,_) at(_) = implicitRequestNode(_,_,_) at(_)
      alias jDefDeclarationNode(_,_,_,_) at(_) = defDeclarationNode(_,_,_,_) at(_)
      alias jVarDeclarationNode(_,_,_,_) at(_) = varDeclarationNode(_,_,_,_) at(_)
      alias jMethodNode(_,_,_) at(_) = methodNode(_,_,_) at(_)
      alias jBlockNode(_,_) at(_) = blockNode(_,_) at(_)
      alias jReturnNode(_) at(_) = returnNode(_) at (_)
      alias jObjectConstructorNode(_) at(_) = objectConstructorNode(_) at(_)
      alias jInheritNode(_,_,_,_) at(_) = inheritNode(_,_,_,_) at(_)

  var nodeCounter := 0

  class nodeAt( source ) -> Node { 
    inherit jNodeAt(source)
    def asStringPrefix = "jeval."
    
    def nodeID is public = nodeCounter
    nodeCounter := nodeCounter + 1 

    //the core of the tree-walking interpreter
    //eval, well, evaluates stuff
    //build called by "Two phase" Contexts, e.g. object constuctors, methods
    //first phase is build, second is eval.
    //declarations should add themsevles to the context in build
    //declarations should initialise themselves in eval
    //expressions should ignore build, and eval themselves on eval
    //there used to be "One phase" contexts - there aren't any more

    method build(ctxt) -> ng.NGO { ng.ngBuild } //NOOP
    method eval(ctxt) -> ng.NGO { error "can't eval {self}" } 
  }






  class stringLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jStringLiteralNode( value' ) at( source ) 
      
      method eval(ctxt) { ng.ngString( value ) } 
  }

  class numberLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jNumberLiteralNode( value' ) at( source ) 
      
      method eval(ctxt) { ng.ngNumber( value ) } 
  }

  class interfaceNode(
        signatures' : Sequence[[Signature]])
          at ( source ) -> Parameter {
      inherit jInterfaceNode(signatures') at( source ) 

      method eval(ctxt ) { ng.ngInterface( self, ctxt ) }
  }

  class blockNode(
      parameters' : Sequence[[Parameter]],
      body' : Sequence[[Statement]])
          at ( source ) -> Parameter {
      inherit jBlockNode( parameters', body' ) at( source ) 
      
      method eval(ctxt) { ng.ngBlock(self,ctxt) }
  }

  class defDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit jDefDeclarationNode(name', typeAnnotation', annotations', value') 
          at( source ) 

      method build(ctxt) {
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = common.processAnnotations(annots,false)
          ctxt.declareDef(name) properties(properties) 
          }
      method eval(ctxt) { 
          ctxt.getLocal(name).initialValue:= value.eval(ctxt)
          ng.ngDone          
      }
  }

  class varDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit defDeclarationNode(name', typeAnnotation', annotations', value') 
          at( source ) 

      method build(ctxt) {
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = common.processVarAnnotations(annots)
          ctxt.declareVar(name) properties(properties) 
          }
  }

  class methodNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]])
          at( source )  -> Method { 
      inherit jMethodNode(signature', body', annotations') at(source)

      method build(ctxt) { 
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = common.processAnnotations(annots,true)
          ctxt.declareName(signature.name) 
                 invocable (ng.ngMethod(self) inContext(ctxt) properties(properties))
          ng.ngDone
      }      
      method eval(_) { ng.ngDone }
  }
  
  class explicitRequestNode(
      receiver' : Expression,
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit jExplicitRequestNode(receiver', name', typeArguments', arguments')
          at( source ) 

      method eval(ctxt) {
         def rcvr = receiver.eval(ctxt)
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup(CREATIO)
         def methodBody = rcvr.lookupExternal(name)

         def ImplicitRequestNodeBrand = type { implicitRequestNodeBrand }

         //if (!methodBody.isPublic) then {error "External request for confidential attribute {name}"}

         if (! (methodBody.isPublic || isSpecialRequest)  ) then {error "External request for confidential attribute {name}"}

         def mySelf = ctxt.lookup("self")
         def isSpecialRequest = rcvr.lexicallyEncloses(mySelf)

         //def isSpecialRequest = 
         //  ImplicitRequestNodeBrand.match(receiver).andAlso {
         //    (receiver.name == "self") || (receiver.name == "outer") }

         Def rv = methodBody.invoke(rcvr) args(args) types(types) creatio(creatio)
         rv
      } 
  }

  class implicitRequestNode(
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit jImplicitRequestNode(name', typeArguments', arguments')
          at( source ) 
         

      method eval(ctxt) {
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup(CREATIO)
         debugPrint ""
         debugPrint "EVAL lookupInternal ({name}) in {ctxt}"
         def methodBody = ctxt.lookupInternal(name) 
         def rv = methodBody.invoke(ctxt) args(args) types(types) creatio(creatio)
         rv
      } 
  }

  class returnNode(
      value' : Expression)
          at ( source )  {
      inherit jReturnNode( value' ) at( source )
      
      method eval(ctxt) {
          def returnCreatio = ctxt.lookup(RETURNCREATIO)
          def subtxt = ctxt.subcontext
          subtxt.declareName(CREATIO) raw( returnCreatio ) 
          ctxt.lookup(RETURNBLOCK).apply( value.eval(subtxt) )
      }
 }

  class objectConstructorNode(
      body' : Sequence[[ObjectStatement]])
          at ( source ) -> Parameter {
      inherit jObjectConstructorNode(body') at(source) 

      method eval(ctxt) { ng.ngObject(body,ctxt) }            
  }

  //consider renaming as "parentNode"
  class inheritNode(
      kind' : String,
      request' : Request,
      excludes' : List[[String]],
      aliases' : Dictionary[[String,String ]])
          at ( source ) -> Parameter {
      inherit jInheritNode(kind', request', excludes', aliases') at ( source )

      def parentID is public = "{PARENT}:{nodeID}"

      method build(ctxt) {ctxt.addParent(self)}

      method eval(ctxt) { 
          def parentalPartObject = ctxt.getLocal(parentID)
          parentalPartObject.initialize //HMMM.
          ng.ngDone          
      }
      
  }  


  method newEmptyContext { ng.newEmptyContext }
}
