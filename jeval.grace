import "jast" as jm
import "combinator-collections" as c
inherit c.abbreviations


//TODO top of dialect - do things continue on to the enclosing scope of the **dialect**
//DONE alias, excludes & abstract & strucure clashes
//TODO annotations (incl abstract?)
//TODO privacy
//TODO privacy and annotations thru inheritance
//TODO building methods (switch methods to build/eval like objects; blocks too I guess)
//TODO types! 
//TODO block matching
//TODO move lookup protocol into objects (from request nodes?)???
//TODO dynamic typechecks on argumenets - and results
//TODO add Kernan primitives for argument access and parsing to execution tree
//TODO   and then convert away from dialect checker to work explicitly
//TODO exceptions
//TODO refactor AST, redesign class names, add progn/sequence properly visitable
//TODO correct canonical names of of assignment methods/requests (wash your dog first)

method jdebug(block) {block.apply}
//method jdebug(block) { } 

def interpreterError is public  = Exception.refine "interpreterError"

method error (string) { 
    interpreterError.raise(string)
}

method assert (block) {
    if (!block.apply) then {error "Assertion failed: {block}"}
}

method safeFuckingMap(f)over(col) {
   def rv = list
   for (col) do { each -> rv.add(f.apply(each)) }
   rv
}

//should probably switch to keysAndValuesDo, last is key == size
method for(col) doWithLast(block2) {
   def size = col.size
   var counter := 1
   for (col) do { e -> 
     block2.apply(e, counter == size)
     counter := counter + 1 
   }
}

def CREATIO = "_creatio"
def RETURNBLOCK = "_returnBlock"
def RETURNCREATIO = "_returnCreatio"



//this trait defines shorthand accessors for the annotations slot.
trait annotationsTrait { 
  method annotationNames is confidential { abstract } 
  method isConfidential { annotationNames.contains "confidential" }
  method isPublic       { ! isConfidential }
  method isAbstract     { annotationNames.contains "abstract" }
  method isConcrete     { ! isAbstract }
  method isReadable     { isPublic || annotationNames.contains "readable" }
  method isWriteable    { isPublic || annotationNames.contains "writeable" }
  method isFinal        { annotationNames.contains "final" }
  method isOverrides    { annotationNames.contains "overrides" }
  //method isComplete     { annotationNames.contains "complete" }
}



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
    method build(ctxt) -> NGO { ctxt.addInitialiser( self ) } 
    method eval(ctxt) -> NGO { error "can't eval {self}" } 
  }






  class stringLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jStringLiteralNode( value' ) at( source ) 
      
      method eval(ctxt) { ngString( value ) } 
  }

  class numberLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jNumberLiteralNode( value' ) at( source ) 
      
      method eval(ctxt) { ngNumber( value ) } 
  }


  class interfaceNode(
        signatures' : Sequence[[Signature]])
          at ( source ) -> Parameter {
      inherit jInterfaceNode(signatures') at( source ) 

      method eval(ctxt ) { ngInterface( self, ctxt ) }
  }

  class blockNode(
      parameters' : Sequence[[Parameter]],
      body' : Sequence[[Statement]])
          at ( source ) -> Parameter {
      inherit jBlockNode( parameters', body' ) at( source ) 
      
      method eval(ctxt) { ngBlock(self,ctxt) }
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
          ctxt.declare(name) asDefInit(value)
          ngDone          
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
          ctxt.declare(name) asVarInit(value)
          ngDone          
      }
  }

  class methodNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]])
          at( source )  -> Method { 
      inherit jMethodNode(signature', body', annotations') at(source)

      method build(ctxt) { 
          ctxt.addLocalSlot(signature.name) 
                 asMethod (ngMethod(self) inContext(ctxt))
          ngDone
      }      
      method eval(ctxt) { 
          //ctxt.declare(signature.name) 
          //       asMethod (acceptVarargs(signature.parameters,ctxt,body,true))
          error "shouldn't happen??"
          ctxt.declare(signature.name) 
                 asMethod (ngMethod(self) inContext(ctxt))
          ngDone
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

      method eval(ctxt) { ngObject(body,ctxt) }            
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




  method newEmptyContext { outer.newEmptyContext }
}

//apply the block to the LIST of arguments.
method applyVarargs(block,args,creatio) {//args are NGOS, aka already evaluated...
  def a = args.asList
  match (args.size)
    case { 0 -> block.apply(creatio) }
    case { 1 -> block.apply(a.at(1),creatio) }
    case { 2 -> block.apply(a.at(1),a.at(2),creatio) }
    case { 3 -> block.apply(a.at(1),a.at(2),a.at(3),creatio) }
    case { 4 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),creatio) }
    case { 5 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),a.at(5),creatio) }
    case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }
}

//PSEUDO-NODES:

//understands eval and uses it to run the statements in a body
//body is a sequnce of statements
//should actually replace use of sequence - should be visitable etc.


// HERE - IF creatio is false, then just pass it in to each
// If Creation is true, make a subcontext with creatio = false, 
//    use the new one for each stmt until the last
//    then use the original one...

class progn (body) {
   method eval(ctxt) { 
     jdebug { print "eval progn {self}" }
     var bodyContext
     if (false == ctxt.lookup(CREATIO)) then {
        bodyContext := ctxt
     } else { 
        bodyContext := ctxt.subcontext
        bodyContext.declare(CREATIO) asMethod(false) 
     }
     var rv := ngDone
     for (body) doWithLast { 
       stmt, last -> rv := stmt.eval( if (!last) then {bodyContext} else {ctxt} ) }
     rv
   }
   method build(ctxt) {
     jdebug { print "build progn {self}" }
     var bodyContext
     if (false == ctxt.lookup(CREATIO)) then {
        bodyContext := ctxt
     } else { 
        bodyContext := ctxt.subcontext
        bodyContext.declare(CREATIO) asMethod(false) 
     }
     var rv := ngDone
     for (body) doWithLast {
        stmt, last -> rv := stmt.build( if (!last) then {bodyContext} else {ctxt} ) }
     rv
   }

}

//these two only live in the initialiser list
//actally, at this point, everything in the initialiser list is one of these..
//run some expression in a fixed context
class run(expr) inContext(ctxt) {
     method eval(_) { jdebug { print "runn {expr} inContext {ctxt}" }
                      def rv = expr.eval(ctxt)
                      jdebug { print "done {expr} inContext {ctxt}" }
                      }      
     method build(_) { error "run(_)inContext(_) should only be called in eval not build" }
}

//initialise a box in some context
class initialise(box) to(expr) inContext(ctxt) {
   method eval(_) { jdebug { print "initialise {box} inContext {ctxt}" }
                    box.initialValue:= expr.eval(ctxt) }
   method build(_) { error "initialise(_)to(_)inContext(_) should only be called in eval not build" }
}

//inherit from some expr 
//lives only in inheritParents and useParents lists
class inheritFrom(expr) inContext(ctxt) { //dunno if this needs context or not...
     method parent {expr}
     jdebug { print "inheritFrom {expr} inContext {ctxt}" }
     method eval(_) { jdebug { print "inhr {expr} inContext {ctxt}" }
                      //def rv = expr.eval(ctxt)
                      jdebug { print "done {expr} inContext {ctxt}" }
                      }      
     method build(_) { error "run(_)inContext(_) should only be called in eval not build" }
}


//return a block that evals the body in a subcontext 
//of ctxt where block args are bound to the params
method acceptVarargs(params,ctxt,body,addEscape) {
  def p = params.asList
  def prognBody = progn(body)
  match (p.size)
    case { 0 -> { creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 1 -> { p1, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 2 -> { p1, p2, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 3 -> { p1, p2, p3, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 4 -> { p1, p2, p3, p4, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 5 -> { p1, p2, p3, p4, p5, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         subtxt.declare(p.at(5).name) asDef( p5 ) 
         subtxt.declare(CREATIO) asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { _ -> error "CANT BE BOTHERED TO ACCEPT MORE VARARGS" }
}

//auxilliary method of applyVarargs 
//to set up binding for "return" statements
method setupReturnThenRun(prognBody,subtxt,addEscape) {
          jdebug { print "setupReturn {prognBody} {subtxt} {addEscape}"         }
          if (addEscape) then {
              subtxt.declare(RETURNBLOCK) asMethod {rv -> return rv} 
              subtxt.declare(RETURNCREATIO) asMethod (subtxt.lookup(CREATIO)) }
          prognBody.eval(subtxt) 
}



type NGO = Unknown


//for debugging
var ngoCounter := 0

//all these NGOs should also have an "at(source)" annotation
//so we can report errors properly!
//tie every object back to an O/C in the source
class ngo { 
  def dbgCounter is readable = ngoCounter
  ngoCounter:= ngoCounter + 1
  def structure is public = dictionary //evil evil making this public
     
  method declare(name) asDefInit(expr) {
    if (structure.containsKey(name)) 
      then { error "trying to declare {name} more than once" }
      else { def box = ngDefBox
             box.initialValue:= expr.eval(self)
             structure.at(name) put (box) } 
  }
  method declare(name) asVarInit (expr) { 
    if (structure.containsKey(name) || structure.containsKey(name ++ "():=(_)")) 
      then { error "trying to declare {name} more than once" }
      else { def box = ngVarBox
             box.initialValue:= expr.eval(self)
             structure.at(name) put (box) 
             structure.at(name ++ "():=(_)") put (box) }
  }


  //I quite like these methods, but am splitting 'em for build vs eval 
  //call "declare" from build; "initialise" from eval
   
  //should check for lexical shadowning -- but we don't
  method declare(name) asMethod ( lambda ) { 
    if (structure.containsKey(name)) 
      then { error "trying to declare {name} more than once" }
      else { structure.at(name) put(lambda) }
  }

  method declare(name) asDef ( ngo ) { 
    jdebug { print "declare {name} asDef {ngo}" }
    if (structure.containsKey(name)) 
      then { error "trying to declare {name} more than once" }
      ///else { structure.at(name) put { creatio -> ngo } }
      else { structure.at(name) put (ngMethodLambda { creatio -> ngo } ) }
  }
  method declare(name) asVar ( ngo ) { 
    jdebug { print "declare {name} asVar {ngo}" }
    if (structure.containsKey(name) || structure.containsKey(name ++ "():=(_)")) 
      then { error "trying to declare {name} more than once" }
      else { def box = ngVarBox
             ngVarBox.initialValue:= ngo
             structure.at(name) put(box)             
             structure.at(name ++ "():=(_)") put(box)
           }
  }
  
  method lookup(name){
         jdebug { print "lookup ngo {name}" }
         def rv =  structure.at(name) 
         jdebug { print "lookup ngo {name} returning {rv}" }
         rv
         }

  //lookup only for slots in "self" (external / explicit)
  method lookupSlot(name){ 
         jdebug { print "lookupSlot ngo {name}"        }
         def rv = structure.at(name) 
         jdebug { print "lookupSlot ngo {name} returning {rv}" }
         rv
         }

  method asString {"ngo#{dbgCounter}: {structure}"}
}

class ngNumber( value' ) {
   inherit ngo
   method value {value'}
   method asString { "ngNumber: {value}"}

   declare "+(_)" asMethod( ngMethodLambda { other, creatio ->  
                  def rv = ngNumber(value' + other.value)
                  rv } )

   declare "asString" asMethod( ngMethodLambda { creatio ->  
                  def rv = ngString(value'.asString)
                  rv } )
}

class ngString( value' ) {
   inherit ngo
   method value {value'}
   method asString { "ngString: {value}"}

   declare "++(_)" asMethod( ngMethodLambda { other, creatio ->  
                  def rv = ngString(value' ++ other.value)
                  rv } )

}

class ngInterface( value', ctxt ) {   
          //cheating, just points to ast node - and context
   inherit ngo
   method value {value'}
   method asString { "ngInterface: {value}"}
}

class ngBlock(blockNode,ctxt) {
   inherit lexicalContext(ctxt)
   method asString { "\{a ngBlock\}" }

   //just have to manufacture the apply method
   //could just make all 5.. 10... 20..
  
   def p = blockNode.parameters.asList
   def name = match (p.size)
     case { 0 -> "apply" }
     case { 1 -> "apply(_)" }
     case { 2 -> "apply(_,_)" }
     case { 3 -> "apply(_,_,_)" }
     case { 4 -> "apply(_,_,_,_)" }
     case { 5 -> "apply(_,_,_,_,_)" }
     case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }

   declare(name) asMethod (ngBlockMethod(blockNode) inContext(ctxt))
}


//COMPLETE FUCKING EVIL GLOBAL VARIABLE
//var bottomMost := true  //should eventually be determined by creatio argument
//COMPLETE FUCKING EVIL GLOBAL VARIABLE

class ngObject(body,ctxt) {
  inherit lexicalContext(ctxt)

  method asString { "ngObject#{dbgCounter}:({status}) {structure}" }

  var status is readable := "embryo"
  
  //COMPLETE FUCKING EVIL GLOBAL VARIABLE
  //var bottomMost := true  //should eventually be determined by creatio argument

  //not completely evil variabe. if creatio is FALSE I'm not inherited - i.e. I'm bottommost
  //otherwise, expect creatio to be the ID of the (bottommost) object being created. I think.
  def bottomMost = (false == ctxt.lookup(CREATIO))

  jdebug { print "{asString} bottomMost {bottomMost} creatio {ctxt.lookup(CREATIO)}" }
  
  def initialisers : Sequence[[Node]] is public = list 
  method addInitialiser(i) {initialisers.add(run(i)inContext(self))}
  def inheritParents :  Sequence[[Node]] = list 
  method addInheritParent(p) {inheritParents.add(inheritFrom(p)inContext(self))}
  def useParents :  Sequence[[Node]] = list 
  method addUseParent(p) {useParents.add(inheritFrom(p)inContext(self))}
  def localSlots :  Dictionary[[String,Method]] = dictionary
  method addLocalSlot(name) asMethod(m) {  //is this "addMethod" name right?
    if (localSlots.containsKey(name))
      then { error "trying to declare {name} more than once" }
      else { localSlots.at(name) put(m)} }

  declare "outer" asDef( lookup("self" ) ) // we haven't declared self yet so the enclosing self...
  declare "self" asDef(self) //does this make sense? - seems to


  for (body) do { e -> e.build(self) }

  //jdebug { print "collecting inheritance shite" }
  //for (body) do { e ->
  //  match (e) 
  //     case { _ : InheritNode -> 
  //        match (e.kind) 
  //          case { "inherit" -> addInheritParent(e) }
  //          case { "use" -> addUseParent(e) }
  //          case { _ -> error "NOT COBOL!" }
  //    }
  //    case { _ ->  }
  //}

  //   method declare(name) asDefInit(expr) {
  //     if (structure.containsKey(name)) 
  //       then { error "trying to declare {name} more than once" }
  //       else { def box = ngDefBox
  //              structure.at(name) put(box) 
  //              initialisers.add(initialise(box) to(expr) inContext(self)) }
  //   }
  //   method declare(name) asVarInit(expr) {
  //     def setterName = name ++ "():=(_)"
  //     if (structure.containsKey(name) || structure.containsKey(setterName)) 
  //       then { error "trying to declare {name} or {name}:= more than once" }
  //       else { def box = ngVarBox
  //              print "about to delare {name} asVar in {self}"
  //              structure.at(name) put(box)             
  //              structure.at(setterName) put(box)
  //              initialisers.add(initialise(box) to(expr) inContext(self)) }
  //   }

  method declare(name) asDefInit(expr) {
    def box = ngDefBox
    addLocalSlot(name) asMethod(box) 
    initialisers.add(initialise(box) to(expr) inContext(self))
  }
  method declare(name) asVarInit(expr) {
    def setterName = name ++ "():=(_)"
    def box = ngVarBox
    addLocalSlot(name) asMethod(box) 
    addLocalSlot(setterName) asMethod(box) 
    initialisers.add(initialise(box) to(expr) inContext(self)) 
  }





  //progn(body).eval(self) //whoo! freaky!!
  
  //progn(body).build(self) //whoo! freakIER!!
  //progn(initialisers).eval(self) //whoo! even Freakier!!

  jdebug { print "building parents" }

  var inheritanceRequestContext := ctxt //inherits clauses evalled in surrounding context
  if (bottomMost) then { 
     inheritanceRequestContext := ctxt.subcontext
     inheritanceRequestContext.declare(CREATIO) asMethod(self)  //inherited constructors wont be bottom
  }

  for (inheritParents) do {  pp ->
    def p = pp.parent
    jdebug { print "inherit {p.kind} {p.request} {p.excludes} {p.aliases}" }
    //need to *evaluate* the parent's request but with creatio argument set
    //in the enclosing context so it knows its not the bottom
    //get back an object w/ struture and initaliers - status should be "built"
    //add ALL its initialiers into our initialisers list
    //process its struture via exclude & inherit; add the stuff in here
    //for multiple parents, resolve clashes...

    def stupidParentalPartObject = p.request.eval(inheritanceRequestContext) 
    if (stupidParentalPartObject.status != "built") 
       then { error "FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    initialisers.addAll(stupidParentalPartObject.initialisers)
    //stupidParentalPartObject.structure.keysAndValuesDo { k, v -> structure.at(k) put(v) }
    stupidParentalPartObject.structure.keysAndValuesDo 
      { k, v -> if (!p.excludes.contains(k)) then {structure.at(k) put(v) } }
    p.aliases.keysAndValuesDo 
      { k, v -> structure.at(k) put(stupidParentalPartObject.structure.at(v)) }
    //all these need to deal correctly with overriding, and multiple defns. they dont.
    //and non-excluded messages...
  }
  
  //COPPY AND PASTE: SHOULD ABSTRACT OUT
  for (useParents) do {  pp ->
    def p = pp.parent
    jdebug { print "inherit {p.kind} {p.request} {p.excludes} {p.aliases}" }

    def stupidParentalPartObject = p.request.eval(inheritanceRequestContext) //relying on global variable
    if (stupidParentalPartObject.status != "built") 
       then { error "FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    initialisers.addAll(stupidParentalPartObject.initialisers)
    stupidParentalPartObject.structure.keysAndValuesDo 
      { k, v -> if (!p.excludes.contains(k)) then {structure.at(k) put(v) } }
    p.aliases.keysAndValuesDo 
      { k, v -> structure.at(k) put(stupidParentalPartObject.structure.at(v)) }
    //all these need to deal correctly with overriding, and multiple defns. they dont.
    //and non-excluded messages...
  }


  //should there be some other structure here?
  //things just replace shit via the map
  //
  jdebug { print "building local" }
  localSlots.keysAndValuesDo { k, v -> structure.at(k) put(v) } //WRONG but first approximation

  status := "built"

  //if I'm NOT Bottommost then I quit here.
  //structure is complete, but don't run any initialisers.
  //ALL initialisers get run only by the bottomMost constructor
  if (!bottomMost) then { 
     return self // status still built not cooked!
  }
  
  jdebug { print "initialising" }
  for (initialisers) do { e ->
     jdebug { print "eval {e}" }
     e.eval(self)
  }

  status := "cooked" //i.e. OK to go! 

  //lexical lookup (internal / implicit)
  method lookup(name){  //copy & paste
    jdebug { print "lookup nuObject {name}" }
    // if (status != "cooked") then { error "CAIN'T lookup {name} in {self}" }
    def rv = (if (structure.containsKey(name)) 
       then { structure.at(name) }
       else { ctxt.lookup(name)})
    jdebug { print "lookup nuObject {name} {rv}" }
    rv
  }
}

def ngDone is public = object {
   inherit ngo
   method asString { "ngDone"}
}

def ngBuild is public = object {
   inherit ngo
   method asString { "ngBuild"} //result returned from build. always an error.
}

def ngUninitialised is public = object {
   inherit ngo
   method asString { "ngUninitialised" } //also an error if accessed
}

def ngImplicitUnknown is public = object {
   inherit ngo
   method asString { "ngImplicitUnknown" } //also an error if accessed
}

class newEmptyContext { 
  inherit ngo
  method asString {"context:{structure}"}
  method subcontext {lexicalContext(self)}
} 

class lexicalContext(ctxt) {
  inherit newEmptyContext 

  method lookup(name){ 
    jdebug { print "lookup lexicLContext {name}" }
    def rv = (if (structure.containsKey(name)) 
       then { structure.at(name) }
       else { ctxt.lookup(name)})
    jdebug { "lookup lexicalContext {name} {rv}" }
    rv

    //     if (structure.containsKey(name)) 
    //        then { structure.at(name) }
    //        else { ctxt.lookup(name)}
  }





}

type Invokeable = { 
    invoke(this: NGO) args(args: Sequence[[NGO]]) types(typeArgs: Sequence[[NGO]]) creatio(creatio)-> NGO
}


class ngDefBox {
   var boxValue := ngUninitialised
   method initialValue:= (initialValue) {
      if (boxValue != ngUninitialised) then { error "can't initialise initailsed box" }
      boxValue := initialValue
   }
   method Xapply(creatio) { //can be called as if 'twere a block
      if (boxValue == ngUninitialised) then { error "can't access uninitailsed box" }
      boxValue
   }
   method invoke(this) args(args) types(typeArgs) creatio(_) {
      assert {args.size == 0}
      if (boxValue == ngUninitialised) then { error "can't access uninitailsed box" }
      boxValue
   }
   method asString {"ngDefBox: {boxValue}"}

}

class ngVarBox {
   inherit ngDefBox
     alias defInvoke(_)args(_)types(_)creatio(_) = invoke(_)args(_)types(_)creatio(_)
   method asString {"ngVarBox: {boxValue}"}
   method Xapply(x,creatio) {boxValue:= x}
   method invoke(this) args(args) types(typeArgs) creatio(creatio) {
     if (args.size == 1) 
        then {boxValue:= args.at(1)}
        else {defInvoke(this) args(args) types(typeArgs) creatio(creatio)}
   }
}

//an invokeable method..
class ngMethod(methodNode) inContext(ctxt) {
   method invoke(this) args(args) types(typeArgs) creatio(creatio) {
     ///print "invoke method invokable {methodNode.signature.name}"
     def params = methodNode.signature.parameters.asList
     def prognBody = progn(methodNode.body)
     def subtxt = ctxt.subcontext
     if (args.size != params.size) then {error "arg mismatch"}
     for (params.indices) do { i -> subtxt.declare(params.at(i).name) asDef(args.at(i)) }
     subtxt.declare(CREATIO) asMethod(creatio) 
     subtxt.declare(RETURNBLOCK) asMethod {rv -> return rv} 
     subtxt.declare(RETURNCREATIO) asMethod (creatio) 
     prognBody.eval(subtxt)
   }
}

class ngBlockMethod(blockNode) inContext(ctxt) {
   method invoke(this) args(args) types(typeArgs) creatio(creatio) {
     ///print "invoke block invokable {blockNode.parameters}"
     def params = blockNode.parameters.asList
     def prognBody = progn(blockNode.body)
     def subtxt = ctxt.subcontext
     if (args.size != params.size) then {error "arg mismatch"}
     for (params.indices) do { i -> subtxt.declare(params.at(i).name) asDef(args.at(i)) }
     subtxt.declare(CREATIO) asMethod(creatio) 
     prognBody.eval(subtxt)
   }
}



//old style lambda; takes creatio plus rest. Or something.
class ngMethodLambda(lambda) {
   method invoke(this) args(args) types(typeArgs) creatio(creatio) {
     applyVarargs(lambda,args,creatio)
   }
}


type InheritNode = {
  request
  aliases
  excludes
}
