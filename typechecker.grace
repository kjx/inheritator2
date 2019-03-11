import "evaluator" as e
import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "type-model" as runtime
import "utility" as utility
use utility.exports
import "loader" as loader
import "subtyping" as subtyping

//method jdebug(block) {block.apply}
method jdebug(block) { } 

method DEBUG(block) {block.apply}
method debugPrint(string) { } 

var ng is public //evil evil dep injection. should be some kind of import

class jcheckFamily {
  inherit e.jevalFamily
      alias eNodeAt(_) = nodeAt(_)
      alias eStringLiteralNode(_) at(_) = stringLiteralNode(_) at(_)
      alias eNumberLiteralNode(_) at(_) = numberLiteralNode(_) at(_)
      alias eInterfaceNode(_) at(_) = interfaceNode(_) at(_)
      alias eExplicitRequestNode(_,_,_,_) at(_) = explicitRequestNode(_,_,_,_) at(_)
      alias eImplicitRequestNode(_,_,_) at(_) = implicitRequestNode(_,_,_) at(_)
      alias eDefDeclarationNode(_,_,_,_) at(_) = defDeclarationNode(_,_,_,_) at(_)
      alias eVarDeclarationNode(_,_,_,_) at(_) = varDeclarationNode(_,_,_,_) at(_)
      alias eMethodNode(_,_,_,_) at(_) = methodNode(_,_,_,_) at(_)
      alias eBlockNode(_,_) at(_) = blockNode(_,_) at(_)
      alias eReturnNode(_) at(_) = returnNode(_) at (_)
      alias eObjectConstructorNode(_,_) at(_) = objectConstructorNode(_,_) at(_)
      alias eModuleNode(_,_) at(_) = moduleNode(_,_) at(_)
      alias eInheritNode(_,_,_,_) at(_) = inheritNode(_,_,_,_) at(_)
      alias eImportNode(_,_,_) at(_) = importNode(_,_,_) at(_)
      alias eParameterNode(_,_,_) at(_) =  parameterNode(_,_,_) at(_)

  method asStringPrefix { "jcheck." }

  var nodeCounter := 0

  class nodeAt( source ) -> Node { 
    inherit eNodeAt(source)
    def asStringPrefix = "jcheck."
    
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
      inherit eStringLiteralNode( value' ) at( source )
      
      method eval(ctxt) { (ctxt.lookupLexical "stringLiteral").value }
  }

  class numberLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit eNumberLiteralNode( value' ) at( source )
      
      method eval(ctxt) { (ctxt.lookupLexical "numberLiteral").value }
  }

  class interfaceNode(
        signatures' : Sequence[[Signature]])
          at ( source ) -> Parameter {
      inherit eInterfaceNode(signatures') at( source )

      //method eval(ctxt ) { ng.ngTypeType( subtyping.objectType( ng.ngInterface( self, ctxt ) ) ) }
      //method eval(ctxt ) {  ng.ngInterface( self, ctxt ) }

      def underlyingInterfaceNode = self

      method eval(ctxt) {
           //print "FAKE FAKE"

           def fakeBody = list
           
           def fakeObject = object {
             inherit ng.objectContext(empty, ctxt)
             method kind {"fakeObject"}
             method body {fakeBody}
             method fakeID {underlyingInterfaceNode} //evil and wrong
             
             method staticTypeCheck( other ) {
               print "FAKE STC\nSELF:{self}\nOTHER:{other}"
               def rv = subtyping.check(other) isSubtypeOf(self)
               print "FAKE STC RETURNS {rv}"
               rv}
           }
             
           for (signatures) do { sig ->
             fakeObject.declareName(sig.name)
                 attribute (ng.attributeSignature(sig)
                              properties(utility.publicAnnotations)
                              inContext(ctxt) )
             fakeBody.add(
                methodNode(sig,empty,empty,"method") at(source)  )
           }
           
           //print "FAKE MAKE {fakeObject}"
           fakeObject
      }
 }

  class blockNode(
      parameters' : Sequence[[Parameter]],
      body' : Sequence[[Statement]])
          at ( source ) -> Parameter {
      inherit eBlockNode( parameters', body' ) at( source )
      
      method eval(ctxt) {
         //seems to come from block attributes??
         //oops. somehow have to 
         def params = parameters.asList
         def prognBody = ng.progn(body)
         def subtxt = ctxt.subcontext

         //TODO - haven't built any parameters!!!!

         prognBody.build(subtxt)
         def returnType = prognBody.eval(subtxt) //I think this is right

         print "not sure block type is right"
         def ret = ng.ngType(
                 subtyping.blockType(
                        ng.ngBlock(self,ctxt),
                        parameters,
                        subtyping.makeObjectType(returnType), //hmm
                        ctxt))
         ret
       }
  }



  class defDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit eDefDeclarationNode(name', typeAnnotation', annotations', value')
          at( source )

      method build(ctxt) {
          def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def properties = utility.processAnnotations(annots,false)
          ctxt.declareDef(name) asType(typeAnnotation) properties(properties)
          }
      method eval(ctxt) {
          print "eval def {name}"
          def tat = typeAnnotation.eval(ctxt)
          def vt  = value.eval(ctxt)
          print "CHECKDF:{tat}\nAGAINST:{vt}"
          if ((vt == ng.ngUninitialised))
             then {error "value cant be uninit"}
             elseif {!tat.staticTypeCheck(vt)} then {
               //we need to check this here because the value being initialised
               //(if there is one) doesn't get as far as the attribute
               //because we've lifted the attribute to types
               print "type check failed: in def: {vt} does not have type {tat}"
          }
          ctxt.getLocal(name).initialValue:= tat
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


  class methodNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]],
      kind' : String)
          at( source )  -> Method { 
      inherit eMethodNode(signature', body', annotations', kind') at(source)

      method build(ctxt) { 
          //doesn't work with brands
          // def annots = safeFuckingMap { a -> a.eval(ctxt) } over(annotations)
          def annots = list
          def properties = utility.processAnnotations(annots,true)

          match(kind) 
            case { "method" ->
               ctxt.declareName(signature.name)
                 attribute (ng.attributeMethod(self) properties(properties) inContext(ctxt) ) }
            case { "type" ->
                     print "treating {signature.name} as type"

                     def typeSignature = signatureNode(signature.name,
                                          signature.typeParameters,
                                          signature.parameters,
                                          body.first,  //treat body as returnType
                                          empty) at (source)
                                          
                     def typeMethod =
                       methodNode(typeSignature,body,annotations,"type") at(source)
                     ctxt.declareName(signature.name)
                       attribute (ng.attributeMethod(typeMethod) properties(properties) inContext(ctxt) ) }

          ng.ngDone
      }      
      method eval(ctxt) {
             print "EVAL {kind} METHOD {signature.name}"
             ng.ngDone }
  }
  
  class explicitRequestNode(
      receiver' : Expression,
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit eExplicitRequestNode(receiver', name', typeArguments', arguments')
          at( source )

      method eval(ctxt) {
         print "{name} {arguments.size}"
         def creatio = ctxt.creatio
         def argCtxt = ctxt.withoutCreatio
         def rcvr = receiver.eval(argCtxt)
         def types = safeFuckingMap { ta -> ta.eval(argCtxt) } over(typeArguments)
         def args = safeFuckingMap { a -> a.eval(argCtxt) } over(arguments)       

         print "RCVR {rcvr}"

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
             //how the hell does this know statically???

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
      inherit eImplicitRequestNode(name', typeArguments', arguments')
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
      inherit eReturnNode( value' ) at( source )
      
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
      body' : Sequence[[ObjectStatement]],
      origin' : Unknown)
          at ( source ) -> Parameter {
      inherit eObjectConstructorNode(body',origin') at(source)

      method eval(ctxt) {
        ng.objectContext(body,ctxt)

        //             def ret = ng.ngType(
        //                 subtyping.objectConstructorType(
        //                        ng.objectContext(body,ctxt), body, ctxt ) )
        //             ret
       }
  }


  class moduleNode(
      moduleDialect' : String,
      body' : Sequence[[ObjectStatement]] )
          at ( source ) -> Parameter {
      inherit eModuleNode(moduleDialect', body') at(source)

      print "making TC module size {body.size} dialect{moduleDialect}"
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
      inherit eInheritNode(kind', request', excludes', aliases') at ( source )

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
      inherit eImportNode(path',name',typeAnnotation') at ( source )

      method build(ctxt) {
          ctxt.declareDef(name) asType(typeAnnotation) properties(utility.confidentialAnnotations)
          }

      method eval(ctxt) { 
          def importedModule = loader.loadModule(path)
          ctxt.getLocal(name).initialValue:=importedModule
          ng.ngDone          
      }
  }


  class parameterNode(n,t,i) at(s) {
    inherit eParameterNode(n,t,i) at(s)
  }
 
  method context { ng.context }
  method lexicalContext(c) { ng.lexicalContext(c) }
  method moduleObject(b,c) { ng.moduleObject(b, c) }
}


def singleton is public = jcheckFamily
