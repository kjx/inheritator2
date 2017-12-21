import "jast" as jm
import "combinator-collections" as c
inherit c.abbreviations

//TODO alias, excludes & for clashes
//TODO completely change invocation protocol take an array of args, self, creatio, caller?, rather than using blocks. blocks only for primitives. everything passed explicitly. 
//TODO move lookup protocol into objects (from request nodes?)
//TODO dynamic typechecks on argumenets - and results
//TODO exceptions
//TODO annotations (incl abstract?)
//TODO privacy
//TODO refactor AST, redesign class names, add progn/sequence properly visitable


//method jdebug(block) {block.apply}
method jdebug(block) { } 

method safeFuckingMap(f)over(col) {
   def rv = list
   for (col) do { each -> rv.add(f.apply(each)) }
   rv
}
method for(col) doWithLast(block2) {
   def size = col.size
   var counter := 1
   for (col) do { e -> 
     block2.apply(e, counter == size)
     counter := counter + 1 
   }
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
    method eval(ctxt) -> NGO { print "ERROR: can't eval {self}" } 
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
      
      method eval(ctxt) { ngBlock(parameters,ctxt,body) }
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

      method build(ctxt) { eval(ctxt) }
      method eval(ctxt) { 
          ctxt.declare(signature.name) 
                 asMethod (acceptVarargs(signature.parameters,ctxt,body,true))
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
         jdebug { print "eval explicitRequest {name}" }
         def rcvr = receiver.eval(ctxt)
         //def types = typeArguments.map { ta -> ta.eval(ctxt) }
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         //def args = arguments.map { a -> a.eval(ctxt) }
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup("_creatio")
         jdebug { print "call explicitRequest {rcvr} {name} {args}" }
         def methodBody = rcvr.lookupSlot(name)
         jdebug { print "cal2 explicitRequest {rcvr} {name} {args} {methodBody}" }
         def rv = applyVarargs(methodBody,args,creatio)
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
         jdebug { print "eval implicitRequest {name} {ctxt}" }
         //def types = typeArguments.map { ta -> ta.eval(ctxt) }
         def types = safeFuckingMap { ta -> ta.eval(ctxt) } over(typeArguments)
         //def args = arguments.map { a -> a.eval(ctxt) }
         def args = safeFuckingMap { a -> a.eval(ctxt) } over(arguments)       
         def creatio = ctxt.lookup("_creatio")
         def methodBody = ctxt.lookup(name) //not quite it wrt inheritance!
         def rv = applyVarargs(methodBody,args,creatio)
         jdebug { print "done implicitRequest {name} {args} {rv}"       }
         rv
      } 
  }

  class returnNode(
      value' : Expression)
          at ( source )  {
      inherit jReturnNode( value' ) at( source )
      
      method eval(ctxt) {
          def returnCreatio = ctxt.lookup("_returnCreatio")
          def subtxt = ctxt.subcontext
          subtxt.declare("_creatio") asMethod( returnCreatio ) 
          ctxt.lookup("_returnBlock").apply( value.eval(subtxt) )
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
      aliases' : List[[ List[[Unknown]] ]])
          at ( source ) -> Parameter {
      inherit jInheritNode(kind', request', excludes', aliases') at ( source )

      //don'tcha just love double dispatch! 
      method build(ctxt) { 

          match (kind) 
            case { "inherit" -> ctxt.addInheritParent(self) }
            case { "use" -> ctxt.addUseParent(self) }
            case { _ -> print "ERROR: NOT COBOL!" }
      }

      method eval(_) { print "ERROR: CAINT EVAL inheritNode" }
      
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
    case { _ -> print "ERROR: CANT BE BOTHERED TO APPLY MORE VARARGS" }
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
     if (false == ctxt.lookup("_creatio")) then {
        bodyContext := ctxt
     } else { 
        bodyContext := ctxt.subcontext
        bodyContext.declare("_creatio") asMethod(false) 
     }
     var rv := ngDone
     for (body) doWithLast { 
       stmt, last -> rv := stmt.eval( if (!last) then {bodyContext} else {ctxt} ) }
     rv
   }
   method build(ctxt) {
     jdebug { print "build progn {self}" }
     var bodyContext
     if (false == ctxt.lookup("_creatio")) then {
        bodyContext := ctxt
     } else { 
        bodyContext := ctxt.subcontext
        bodyContext.declare("_creatio") asMethod(false) 
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
     method build(_) { print "ERROR: run(_)inContext(_) should only be called in eval not build" }
}

//initialise a box in some context
class initialise(box) to(expr) inContext(ctxt) {
   method eval(_) { jdebug { print "initialise {box} inContext {ctxt}" }
                    box.initialValue:= expr.eval(ctxt) }
   method build(_) { print "ERROR: initialise(_)to(_)inContext(_) should only be called in eval not build" }
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
     method build(_) { print "ERROR: run(_)inContext(_) should only be called in eval not build" }
}


//return a block that evals the body in a subcontext 
//of ctxt where block args are bound to the params
method acceptVarargs(params,ctxt,body,addEscape) {
  def p = params.asList
  def prognBody = progn(body)
  match (p.size)
    case { 0 -> { creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 1 -> { p1, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 2 -> { p1, p2, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 3 -> { p1, p2, p3, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 4 -> { p1, p2, p3, p4, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 5 -> { p1, p2, p3, p4, p5, creatio -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         subtxt.declare(p.at(5).name) asDef( p5 ) 
         subtxt.declare("_creatio") asMethod( creatio ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { _ -> print "ERROR: CANT BE BOTHERED TO ACCEPT MORE VARARGS" }
}

//auxilliary method of applyVarargs 
//to set up binding for "return" statements
method setupReturnThenRun(prognBody,subtxt,addEscape) {
          jdebug { print "setupReturn {prognBody} {subtxt} {addEscape}"         }
          if (addEscape) then {
              subtxt.declare("_returnBlock") asMethod {rv -> return rv} 
              subtxt.declare("_returnCreatio") asMethod (subtxt.lookup("_creatio")) }
          prognBody.eval(subtxt) 
}



type NGO = Unknown


//all these NGOs should also have an "at(source)" annotation
//so we can report errors properly!
//tie every object back to an O/C in the source
class ngo { 
  def structure is public = dictionary //evil evil making this public
     
  method declare(name) asDefInit(expr) {
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { def box = ngDefBox
             box.initialValue:= expr.eval(self)
             structure.at(name) put (box) } 
  }
  method declare(name) asVarInit (expr) { 
    if (structure.containsKey(name) || structure.containsKey(name ++ "():=(_)")) 
      then { print "ERROR: trying to declare {name} more than once" }
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
      then { print "ERROR: trying to declare {name} more than once" }
      else { structure.at(name) put(lambda) }
  }

  method declare(name) asDef ( ngo ) { 
    jdebug { print "declare {name} asDef {ngo}" }
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { structure.at(name) put { creatio -> ngo } }
  }
  method declare(name) asVar ( ngo ) { 
    jdebug { print "declare {name} asVar {ngo}" }
    if (structure.containsKey(name) || structure.containsKey(name ++ "():=(_)")) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { 
           var closedVariable := ngo
           structure.at(name) put { creatio -> closedVariable } 
           structure.at(name ++ "():=(_)") 
              put { v, creatio -> closedVariable:= v
                                  ngDone } 
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

  method asString {"ngo:{structure}"}
}

class ngNumber( value' ) {
   inherit ngo
   method value {value'}
   method asString { "ngNumber: {value}"}

   declare "+(_)" asMethod { other, creatio ->  ngNumber(value + other.value) } 
}

class ngString( value' ) {
   inherit ngo
   method value {value'}
   method asString { "ngString: {value}"}
}

class ngInterface( value', ctxt ) {   
          //cheating, just points to ast node - and context
   inherit ngo
   method value {value'}
   method asString { "ngInterface: {value}"}
}

class ngBlock(parameters,ctxt,body) {
   inherit lexicalContext(ctxt)
   method asString { "\{a ngBlock\}" }

   //just have to manufacture the apply method
   //could just make all 5.. 10... 20..
  
   def p = parameters.asList
   def name = match (p.size)
     case { 0 -> "apply" }
     case { 1 -> "apply(_)" }
     case { 2 -> "apply(_,_)" }
     case { 3 -> "apply(_,_,_)" }
     case { 4 -> "apply(_,_,_,_)" }
     case { 5 -> "apply(_,_,_,_,_)" }
     case { _ -> print "ERROR: CANT BE BOTHERED TO APPLY MORE VARARGS" }

   declare(name) asMethod (acceptVarargs(parameters,ctxt,body,false))
}


//COMPLETE FUCKING EVIL GLOBAL VARIABLE
//var bottomMost := true  //should eventually be determined by creatio argument
//COMPLETE FUCKING EVIL GLOBAL VARIABLE

class ngObject(body,ctxt) {
  inherit lexicalContext(ctxt)
  method asString { "ngObject:({status}) {structure}" }

  var status is readable := "embryo"
  
  //COMPLETE FUCKING EVIL GLOBAL VARIABLE
  //var bottomMost := true  //should eventually be determined by creatio argument

  //not completely evil variabe. if creatio is FALSE I'm not inherited - i.e. I'm bottommost
  //otherwise, expect creatio to be the ID of the (bottommost) object being created. I think.
  def bottomMost = false == ctxt.lookup("_creatio")

  jdebug { print "ngObject bottomMost {bottomMost} creatio {ctxt.lookup("_creatio")}" }
  
  def initialisers : Sequence[[Node]] is public = list 
  method addInitialiser(i) {initialisers.add(run(i)inContext(self))}

  def inheritParents :  Sequence[[Node]] = list 
  method addInheritParent(p) {inheritParents.add(inheritFrom(p)inContext(self))}
  def useParents :  Sequence[[Node]] = list 
  method addUseParent(p) {useParents.add(inheritFrom(p)inContext(self))}

  declare "outer" asDef( lookup("self" ) ) // we haven't declared self yet so the enclosing self...
  declare "self" asDef(self) //does this make sense? - seems to

  jdebug { print "collecting inheritance shite" }
  for (body) do { e ->
    match (e) 
       case { _ : InheritNode -> 
          match (e.kind) 
            case { "inherit" -> addInheritParent(e) }
            case { "use" -> addUseParent(e) }
            case { _ -> print "ERROR: NOT COBOL!" }
      }
      case { _ ->  }
  }


  method declare(name) asDefInit(expr) {
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { def box = ngDefBox
             structure.at(name) put(box) 
             initialisers.add(initialise(box) to(expr) inContext(self)) }
  }
  method declare(name) asVarInit(expr) {
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { def box = ngVarBox
             structure.at(name) put(box)             
             structure.at(name ++ "():=(_)") put(box)
             initialisers.add(initialise(box) to(expr) inContext(self)) }
  }


  //progn(body).eval(self) //whoo! freaky!!
  
  //progn(body).build(self) //whoo! freakIER!!
  //progn(initialisers).eval(self) //whoo! even Freakier!!

  jdebug { print "building parents" }

  var inheritanceRequestContext := ctxt //inherits clauses evalled in surrounding context
  if (bottomMost) then { 
     inheritanceRequestContext := ctxt.subcontext
     inheritanceRequestContext.declare("_creatio") asMethod(self)  //inherited constructors wont be bottom
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
       then { print "ERROR: FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    initialisers.addAll(stupidParentalPartObject.initialisers)
    stupidParentalPartObject.structure.keysAndValuesDo { k, v -> structure.at(k) put(v) }
  }
  
  //COPPY AND PASTE: SHOULD ABSTRACT OUT
  for (useParents) do {  pp ->
    def p = pp.parent
    jdebug { print "inherit {p.kind} {p.request} {p.excludes} {p.aliases}" }

    def stupidParentalPartObject = p.request.eval(inheritanceRequestContext) //relying on global variable
    if (stupidParentalPartObject.status != "built") 
       then { print "ERROR: FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    initialisers.addAll(stupidParentalPartObject.initialisers)
    stupidParentalPartObject.structure.keysAndValuesDo { k, v -> structure.at(k) put(v) }
  }


  //should there be some other structure here?
  //things just replace shit via the map
  //
  jdebug { print "building local" }
  for (body) do { e ->
     jdebug { print "build {e}" }
     e.build(self)
  }

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

class ngDefBox {
   var boxValue := ngUninitialised
   method initialValue:= (initialValue) {
      if (boxValue != ngUninitialised) then { print "ERROR: can't initialise initailsed box" }
      boxValue := initialValue
   }
   method apply(creatio) { //can be called as if 'twere a block
      if (boxValue == ngUninitialised) then { print "ERROR: can't access uninitailsed box" }
      boxValue
   }
}

class ngVarBox {
   inherit ngDefBox
   method apply(x,creatio) {boxValue:= x}
}

type InheritNode = {
  request
  aliases
  excludes
}
