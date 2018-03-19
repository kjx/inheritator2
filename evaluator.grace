import "common-ast" as jm
import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "object-model" as runtime
def ng is public = runtime.singleton
import "utility" as utility
use utility.exports
import "loader" as loader



//TODO dynamic typechecks on arguments - and results
//TODO where clauses...

//TODO types! 

//TODO block matching
//TODO exceptions

//TODO nother f**king look at alias/excludes
//do aliases first; check for errors
//TODO should make exclude retun an "abstract" candidate
//TODO check candidate consolidation?

//TODO alias clauses annotations change privacy? 
//  (need to fix kernan parser)

//TODO add Kernan primitives to let us link through to incoming source code
//TODO add Kernan support for block generic arguments

//TODO imported names should go into an extra surrounding scope
//TODO     currently they go into the current scope
//TOOD     this is bad because they can be inherited
//TODO     although also good because can be local

//TODO refactor AST, redesign class names
//TODO refactor progn out of runtime into jast - 
//TODO AST returnType -> returnTypeAnnotation (all types here are annotations?)
//TODO add sequence and statementsequence into the common AST

//TODO add "provenacne" to methods, e.g. if they came from a class or type decln

//TODO correct canonical names of of assignment methods/requests (wash your dog first)





//method jdebug(block) {block.apply}
method jdebug(block) { } 

method DEBUG(block) {block.apply}
method debugPrint(string) { } 




class jevalFamily {
  inherit jm.jastFamily
      alias jNodeAt(_) = nodeAt(_)
      alias jStringLiteralNode(_) at(_) = stringLiteralNode(_) at(_)
      alias jNumberLiteralNode(_) at(_) = numberLiteralNode(_) at(_)
      alias jInterfaceNode(_) at(_) = interfaceNode(_) at(_)
      alias jExplicitRequestNode(_,_,_,_) at(_) = explicitRequestNode(_,_,_,_) at(_)
      alias jImplicitRequestNode(_,_,_) at(_) = implicitRequestNode(_,_,_) at(_)
      alias jDefDeclarationNode(_,_,_,_) at(_) = defDeclarationNode(_,_,_,_) at(_)
      alias jVarDeclarationNode(_,_,_,_) at(_) = varDeclarationNode(_,_,_,_) at(_)
      alias jMethodDeclarationNode(_,_,_) at(_) = methodDeclarationNode(_,_,_) at(_)
      alias jBlockNode(_,_) at(_) = blockNode(_,_) at(_)
      alias jReturnNode(_) at(_) = returnNode(_) at (_)
      alias jObjectConstructorNode(_) at(_) = objectConstructorNode(_) at(_)
      alias jModuleNode(_,_) at(_) = moduleNode(_,_) at(_)
      alias jInheritNode(_,_,_,_) at(_) = inheritNode(_,_,_,_) at(_)
      alias jImportNode(_,_,_) at(_) = importNode(_,_,_) at(_)

  var nodeCounter := 0

  class nodeAt( source ) -> Node { 
    inherit jNodeAt(source)
    def asStringPrefix = "jeval."
    
    def nodeID is public = nodeCounter
    nodeCounter := nodeCounter + 1 

    //the core of the tree-walking interpreter
    //
    //eval, well, evaluates stuff
    //build called by "Two phase" Contexts, e.g. object constuctors, methods
    //first phase is build, second is eval.
    //
    //only "declarations" that build attributes into contexts
    //   should implement build
    //   declarations should add themsevles to the context in build
    //   declarations should initialise themselves in eval
    //
    //expressions should ignore build, and eval themselves on eval

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
          def properties = utility.processAnnotations(annots,false)
          ctxt.declareDef(name) asType(typeAnnotation) properties(properties)
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
          def properties = utility.processVarAnnotations(annots)
          ctxt.declareVar(name) asType(typeAnnotation) properties(properties)
          }
  }

  class methodDeclarationNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]])
          at( source )  -> Method { 
      inherit jMethodDeclarationNode(signature', body', annotations') at(source)

      method build(ctxt) { 
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = utility.processAnnotations(annots,true)
          ctxt.declareName(signature.name)
                 attribute (ng.attributeMethod(self) properties(properties) inContext(ctxt) )
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
         print "{name} {arguments.size}"

         def creatio = ctxt.creatio
         def argCtxt = ctxt.withoutCreatio
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
         def argCtxt = ctxt.withoutCreatio

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


  class moduleNode(
      moduleDialect' : String,
      body' : Sequence[[ObjectStatement]] )
          at ( source ) -> Parameter {
      inherit jModuleNode(moduleDialect', body') at(source)
    
      method eval(ctxt) {
        def dialectModuleObject = loader.loadModule(moduleDialect)
        ng.moduleObject(body,dialectModuleObject) }            
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



  class importNode(
      path' : String,
      name' : String,
      typeAnnotation' : Expression)
          at ( source ) -> Node {
      inherit jImportNode(path',name',typeAnnotation') at ( source )

      method build(ctxt) {
          ctxt.declareDef(name) asType(typeAnnotation) properties(utility.confidentialAnnotations)
          }

      method eval(ctxt) { 
          def importedModule = loader.loadModule(path)
          ctxt.getLocal(name).initialValue:=importedModule
          ng.ngDone          
      }
  }


  method context { ng.context }
  method lexicalContext(c) { ng.lexicalContext(c) }
  method moduleObject(b,c) { ng.moduleObject(b, c) }
}


def singleton is public = jevalFamily
//loader.commonASTFactory:= singleton
