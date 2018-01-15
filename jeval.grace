import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports
import "jruntime" as runtime
def ng = runtime.exports
import "jcommon" as common
use common.exports

//TODO - inheritance, structure clashes, et.

//TODO - object constructors need to keep separate parts. This seems unavoidable... - or perhaps it is but ONLY if we "resolve implicit requests" (up or out?) before the fun really starts. This means:
//- part objects should store the creatio as representing the "whole" of which they are part
//- "lexical"/ implicit lookup looks up the part object in the lexical content
//-- (not the "whole" object that will have stuff introduced in subclasses)
//-- (for Grace, unless we go up then out?, must also resolve to super-parts)
//- if a method is found in a part object, you don't use the declaration there;
//-- rather you lookup (inheritance only?) the method in whole object of which the part is part, so the defn you found may be overridden. 
//- 

//TODO building methods (switch methods to build/eval like objects; blocks too I guess)

//TODO alias clauses change privacy?
//TODO add extra argument to Invokeable>>invoke()blah()blah()...
//   to code for internal vs external request?
//TODO Invokablesx  have copyReadable/copyWritable/copyConfidential/copyPUblic methods
//    to handle annotations on alias statementts?
//TODO use a wrapper to make things confidential?




//TODO top of dialect - do things continue on to the enclosing scope of the **dialect**
//TODO make a ngdialect object, and set up the standard dialect e.g. with print and annotations and stuff

//TODO types! 
//TODO block matching
//TODO move lookup protocol into objects (from request nodes?)???
//TODO dynamic typechecks on argumenets - and results
//TODO add Kernan primitives for argument access and parsing to execution tree
//TODO   and then convert away from dialect checker to work explicitly
//TODO exceptions
//TODO refactor AST, redesign class names, add progn/sequence properly visitable
//TODO correct canonical names of of assignment methods/requests (wash your dog first)

//BADIDEA - operator :=.    if assignment method isn't found, but accessor method *is*, then run as operator:= on the result of the accessor...
//BADIDEA this comes straight from "boxy grace"
//next step is magic operation ^ or ("proxy" or something) --- if you have an object with it (and you don't imeplement a request???) you delegate by calling ^ and then re-running the method on that??

//method jdebug(block) {block.apply}
method jdebug(block) { } 

method DEBUG(block) {block.apply}





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

  class nodeAt( source ) -> Node { 
    inherit jNodeAt(source)
    //method asString { "jNode" }

    //build called by "Two phase" Contexts, e.g. object constuctors
    //build itself arranges that "eval" will eventually be called
    //for declrations, eval will be called on the initialiser, 
    //not on the declaration.
    //for one-phase contexts, eval can be called on the declaration
    //without build, and eval should just do the lot.
    method build(ctxt) -> ng.NGO { ctxt.addInitialiser( self ) } 
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

      method build(ctxt) { eval(ctxt) }
      method eval(ctxt) { 
          //ctxt.declare(name) asDef(value.eval(ctxt))
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = common.processAnnotations(annots,false)
          ctxt.declare(name) asDefInit(value) properties(properties)
          ng.ngDone          
      }
  }

  class varDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit jVarDeclarationNode(name', typeAnnotation', annotations', value') 
          at( source ) 

      method build(ctxt) { eval(ctxt) }
      method eval(ctxt) { 
          //ctxt.declare(name) asVar(value.eval(ctxt))
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def varProperties = common.processVarAnnotations(annots)
          ctxt.declare(name) asVarInit(value) properties(varProperties)
          ng.ngDone          
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
          ctxt.addLocalSlot(signature.name) 
                 asMethod (ng.ngMethod(self) inContext(ctxt) properties(properties))
          ng.ngDone
      }      
      method eval(ctxt) { 
          //ctxt.declare(signature.name) 
          //       asMethod (acceptVarargs(signature.parameters,ctxt,body,true))
          error "shouldn't happen??"
          ctxt.declare(signature.name) 
                 asMethod (ng.ngMethod(self) inContext(ctxt) properties(properties))
          ng.ngDone
      }
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
         ///print "eval explicitRequest {name}"
         jdebug { print "eval explicitRequest {name}" }
         def rcvr = receiver.eval(ctxt)
         //def types = typeArguments.map { ta -> ta.eval(ctxt) }
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         //def args = arguments.map { a -> a.eval(ctxt) }
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup(CREATIO)
         jdebug { print "call explicitRequest {rcvr} {name} {args}" }
         def methodBody = rcvr.lookupSlot(name)
         jdebug { print "cal2 explicitRequest {rcvr} {name} {args} {methodBody}" }
         if (!methodBody.isPublic) then {error "External request for confidential attribute {name}"}
         ///def rv = applyVarargs(methodBody,args,creatio)
         def rv = methodBody.invoke(rcvr) args(args) types(types) creatio(creatio)
         //??def rv = rcvr.externalRequest(name) args(args) typeArgs(types) creatio(creatio)
         jdebug { print "done explicitRequest {rcvr} {name} {args} {rv}" }
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
         ///print "eval implicitRequest {name}" 
         jdebug { print "eval implicitRequest {name} {ctxt}" }
         //def types = typeArguments.map { ta -> ta.eval(ctxt) }
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         //def args = arguments.map { a -> a.eval(ctxt) }
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup(CREATIO)
         def methodBody = ctxt.lookup(name) //not quite it wrt inheritance!
         //def rv = applyVarargs(methodBody,args,creatio)
         def rv = methodBody.invoke(ctxt) args(args) types(types) creatio(creatio)
         jdebug { print "done implicitRequest {name} {args} {rv}"       }
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
          subtxt.declare(CREATIO) asMethod( returnCreatio ) 
          ctxt.lookup(RETURNBLOCK).apply( value.eval(subtxt) )
      }
 }

  class objectConstructorNode(
      body' : Sequence[[ObjectStatement]])
          at ( source ) -> Parameter {
      inherit jObjectConstructorNode(body') at(source) 

      method eval(ctxt) { ng.ngObject(body,ctxt) }            
  }

  class inheritNode(
      kind' : String,
      request' : Request,
      excludes' : List[[String]],
      aliases' : Dictionary[[String,String ]])
          at ( source ) -> Parameter {
      inherit jInheritNode(kind', request', excludes', aliases') at ( source )

      //don'tcha just love double dispatch! 
      method build(ctxt) { 
          
          match (kind) 
            case { "inherit" -> ctxt.addInheritParent(self) }
            case { "use" -> ctxt.addUseParent(self) }
            case { _ -> error "NOT COBOL!" }
      }

      method eval(_) { error "CAINT EVAL inheritNode" }
      
  }  




  method newEmptyContext { ng.newEmptyContext }
}
