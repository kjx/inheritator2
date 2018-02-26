import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports
import "jruntime" as runtime
def ng is public = runtime.exports
import "jcommon" as common
use common.exports


//TODO rename jruntime as jobjectmodel.grace
//TODO put all the tests into a subdirectory?

//TODO nother f**king look at alias/excludes
//do aliases first; check for errors
//TODO should make exclude retun an "abstract" candidate
//TODO check candidate consolidation?


//TODO alias clauses annotations change privacy? 
//  (need to fix kernan parser)

//TODO types! 
//TODO block matching
//TODO exceptions

//TODO dynamic typechecks on argumenets - and results
//TODO add Kernan primitives for argument access and parsing to execution tree
//TODO   and then convert away from dialect checker to work explicitly
//TODO add Kernan primitives to let us link through to incoming source code

//TODO generics...
//TODO where clauses...

//TODO refactor AST, redesign class names
//TODO add "provenacne" to methods, e.g. if they came from a class or type decln
//TODO correct canonical names of of assignment methods/requests (wash your dog first)
//TODO refactor progn out of runtime into jast - 
//TODO add sequence and statementsequence into the common AST


class typeType {
      method ==(other) { asString == other.asString }
}

def typeNumber is public = object { 
    inherit typeType
    method ==(other) { asString == other.asString }
    method asString { "typeNumber" }
}

def typeString is public = object { 
    inherit typeType
    method ==(other) { asString == other.asString }
    method asString { "typeString" }
}


assert {"typeNumber" == "typeNumber"}

assert {typeNumber == typeNumber}
assert {typeString == typeString}
assert {typeString != typeNumber}


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
      
      method eval(ctxt) { typeString } 
  }

  class numberLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jNumberLiteralNode( value' ) at( source ) 
      
      method eval(ctxt) { typeNumber } 
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
          assert (typeAnnotation.eval(ctxt)) equals (value.eval(ctxt))
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
                 invocable (ng.invocableMethod(self) properties(properties) inContext(ctxt) )
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

         def creatio = ctxt.creatio
         def argCtxt = ctxt.subcontextWithoutCreatio
         def rcvr = receiver.eval(argCtxt)
         def types = safeFuckingMap { ta -> ta.eval(argCtxt) } over(typeArguments)
         def args = safeFuckingMap { a -> a.eval(argCtxt) } over(arguments)       
         def methodBody = rcvr.lookupExternal(name)

         if (rcvr != rcvr.whole) then {
             print "about to crash"
             print "requesting {name}"
             print "RCVR"
             print (rcvr)
             print "RCVR WHOLE"
             print (rcvr.whole) }

         assert {rcvr.isWhole}
         def mySelf = ctxt.getInternal("self").value
         def isSpecialRequest = mySelf.isInside(rcvr)

         if (! (methodBody.isPublic || isSpecialRequest)  ) 
           then {error "External request for confidential attribute {name}"}

         //if (!methodBody.isPublic) then {error "External request for confidential attribute {name}"}

         //def isSpecialRequest = 
         // ImplicitRequestNodeBrand.match(receiver).andAlso {
         //    (receiver.name == "self") || (receiver.name == "outer") }

         def rv = methodBody.invoke(rcvr) args(args) types(types) creatio(creatio)
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
         def creatio = ctxt.creatio 
         def argCtxt = ctxt.subcontextWithoutCreatio

         def types = safeFuckingMap { ta -> ta.eval(argCtxt) } over(typeArguments)
         def args = safeFuckingMap { a -> a.eval(argCtxt) } over(arguments)   
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
          def returnCreatio = ctxt.getInternal(RETURNCREATIO).value
          def returnBlock = ctxt.getInternal(RETURNBLOCK)
          returnBlock.invoke(ctxt) 
                          args(list( value.eval(ctxt) ))
                          types(empty)                  
                          creatio(returnCreatio)
      }
 }

  class objectConstructorNode(
      body' : Sequence[[ObjectStatement]])
          at ( source ) -> Parameter {
      inherit jObjectConstructorNode(body') at(source) 

      method eval(ctxt) { ng.objectContext(body,ctxt) }            
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
          parentalPartObject.initialize
          ng.ngDone          
      }
      
  }  


  method context { ng.context }
  method lexicalContext(c) { ng.lexicalContext(c) }
  method moduleObject(b,c) { ng.moduleObject(b, c) }
}
