import "jast" as jm
import "combinator-collections" as c
inherit c.abbreviations


//method jdebug(s) {print(s)}
method jdebug(s) { } 

method safeFuckingMap(f)over(col) {
   def rv = list
   for (col) do { each -> rv.add(f.apply(each)) }
   rv
}

var satan := -1000000

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

  class nodeAt( source ) -> Node { 
    inherit jNodeAt(source)
    //method asString { "jNode" }

    //build called by "Two phase" Contexts, e.g. object constuctors
    //build itself arranges that "eval" will eventually be called
    //for declrations, eval will be called on the initialiser, 
    //not on the declaration.
    //for one-phase contexts, eval can be called on the declaration
    //without build, and eval should just do the lot.
    method build ( ctxt ) -> NGO { ctxt.addInitialiser( self ) } 
    method eval( ctxt ) -> NGO { print "ERROR: can't eval {self}" } 
  }






  class stringLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jStringLiteralNode( value' ) at( source ) 
      
      method eval( ctxt ) { ngString( value ) } 
  }

  class numberLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit jNumberLiteralNode( value' ) at( source ) 
      
      method eval( ctxt ) { ngNumber( value ) } 
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
         
      method eval( ctxt ) {
         jdebug "eval explicitRequest {name}"
         satan := satan + 1
         if (satan == 2) then { 1 / 0 }
         def rcvr = receiver.eval( ctxt )
         //def types = typeArguments.map { ta -> ta.eval( ctxt ) }
         def types = safeFuckingMap { ta -> ta.eval( ctxt ) } over(typeArguments)
         //def args = arguments.map { a -> a.eval( ctxt ) }
         def args = safeFuckingMap { a -> a.eval( ctxt ) } over(arguments)       
         jdebug "call explicitRequest {rcvr} {name} {args}"
         def methodBody = rcvr.lookupSlot(name)
         jdebug "cal2 explicitRequest {rcvr} {name} {args} {methodBody}"
         def rv = applyVarargs(methodBody,args)
         jdebug "done explicitRequest {rcvr} {name} {args} {rv}"
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
         

      method eval( ctxt ) {
         jdebug "eval implicitRequest {name} {ctxt}"
         //def types = typeArguments.map { ta -> ta.eval( ctxt ) }
         def types = safeFuckingMap { ta -> ta.eval( ctxt ) } over(typeArguments)
         //def args = arguments.map { a -> a.eval( ctxt ) }
         def args = safeFuckingMap { a -> a.eval( ctxt ) } over(arguments)       
         jdebug "call implicitRequest {name} {args}"
         def methodBody = ctxt.lookup(name) //not quite it wrt inheritance!
         jdebug "bing"
         jdebug "bong {name}"
         jdebug "bang {args}"
         jdebug "zap {methodBody}"
         jdebug "cal2 implicitRequest {name} {args} {methodBody}"
         def rv = applyVarargs(methodBody,args)
         jdebug "done implicitRequest {name} {args} {rv}"      
         rv
      } 
  }

  class returnNode(
      value' : Expression)
          at ( source )  {
      inherit jReturnNode( value' ) at( source )
      
      method eval( ctxt ) {
          ctxt.lookup("_returnBlock").apply( value.eval(ctxt) )
      }
  }


  class objectConstructorNode(
      body' : Sequence[[ObjectStatement]])
          at ( source ) -> Parameter {
      inherit jObjectConstructorNode(body') at(source) 
              
      method eval(ctxt) { ngObject(body,ctxt) }            
  }

  


  method newEmptyContext { outer.newEmptyContext }
}

//apply the block to the LIST of arguments.
method applyVarargs(block,args) {//args are NGOS, aka already evaluated...
  def a = args.asList
  match (args.size)
    case { 0 -> block.apply }
    case { 1 -> block.apply(a.at(1)) }
    case { 2 -> block.apply(a.at(1),a.at(2)) }
    case { 3 -> block.apply(a.at(1),a.at(2),a.at(3)) }
    case { 4 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4)) }
    case { 5 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),a.at(5)) }
    case { _ -> print "CANT BE BOTHERED TO APPLY MORE VARARGS" }
}

//PSEUDO-NODES:

//understands eval and uses it to run the statements in a body
//body is a sequnce of statements
//should actually replace use of sequence - should be visitable etc.
class progn (body) {
   method eval(ctxt) { 
     jdebug "eval progn {self}"
     var rv := ngDone
     for (body) do { stmt -> rv := stmt.eval(ctxt) }
     rv
   }
   method build(ctxt) {
     jdebug "build progn {self}" 
     var rv := ngDone
     for (body) do { stmt -> rv := stmt.build(ctxt) }
     rv
   }

}


//these two only live in the initialiser list
//actally, at this point, everything in the initialiser list is one of these..
//run some expression in a fixed context
class run(expr) inContext(ctxt) {
     method eval(_) { jdebug "runn {expr} inContext {ctxt}"
                      def rv = expr.eval(ctxt)
                      jdebug "done {expr} inContext {ctxt}"
                      }      
     method build(_) { print "ERROR: run(_)inContext(_) should only be called in eval not build" }
}

//initialise a box in some context
class initialise(box) to(expr) inContext(ctxt) {
   method eval(_) { jdebug "initialise {box} inContext {ctxt}"
                    box.initialValue:= expr.eval(ctxt) }
   method build(_) { print "ERROR: initialise(_)to(_)inContext(_) should only be called in eval not build" }
}

//return a block that evals the body in a subcontext 
//of ctxt where block args are bound to the params
method acceptVarargs(params,ctxt,body,addEscape) {
  def p = params.asList
  def prognBody = progn(body)
  match (p.size)
    case { 0 -> { 
         def subtxt = ctxt.subcontext
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 1 -> { p1 -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 2 -> { p1, p2 -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 3 -> { p1, p2, p3 -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 4 -> { p1, p2, p3, p4 -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { 5 -> { p1, p2, p3, p4, p5 -> 
         def subtxt = ctxt.subcontext
         subtxt.declare(p.at(1).name) asDef( p1 ) 
         subtxt.declare(p.at(2).name) asDef( p2 ) 
         subtxt.declare(p.at(3).name) asDef( p3 ) 
         subtxt.declare(p.at(4).name) asDef( p4 ) 
         subtxt.declare(p.at(5).name) asDef( p5 ) 
         setupReturnThenRun(prognBody,subtxt,addEscape) } }
    case { _ -> print "CANT BE BOTHERED TO ACCEPT MORE VARARGS" }
}

//auxilliary method of applyVarargs 
//to set up binding for "return" statements
method setupReturnThenRun(prognBody,subtxt,addEscape) {
          jdebug "setupReturn {prognBody} {subtxt} {addEscape}"        
          if (addEscape) then {subtxt.declare("_returnBlock") asMethod {rv -> return rv} }
          prognBody.eval(subtxt) 
}



type NGO = Unknown


//all these NGOs should also have an "at(source)" annotation
//so we can report errors properly!
//tie every object back to an O/C in the source
class ngo { 
  def structure = dictionary
     
  method declare(name) asDefInit(expr) {
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { def box = ngDefBox
             box.initialValue:= expr.eval(self)
             structure.at(name) put (box) } 
  }
  method declare( name ) asVarInit (expr) { 
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
  method declare( name ) asMethod ( lambda ) { 
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { structure.at(name) put(lambda) }
  }

  method declare( name ) asDef ( ngo ) { 
    jdebug "declare {name} asDef {ngo}"
    if (structure.containsKey(name)) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { structure.at(name) put { ngo } }
  }
  method declare( name ) asVar ( ngo ) { 
    jdebug "declare {name} asVar {ngo}"
    if (structure.containsKey(name) || structure.containsKey(name ++ "():=(_)")) 
      then { print "ERROR: trying to declare {name} more than once" }
      else { 
           var closedVariable := ngo
           structure.at(name) put { closedVariable } 
           structure.at(name ++ "():=(_)") 
              put { v -> closedVariable:= v
                         ngDone } 
           }
  }
  
  method lookup( name ) {
         jdebug "lookup ngo {name}"
         def rv =  structure.at(name) 
         jdebug "lookup ngo {name} returning {rv}"
         rv
         }

  //lookup only for slots in "self" (external / explicit)
  method lookupSlot( name ) { 
         jdebug "lookupSlot ngo {name}"       
         def rv = structure.at(name) 
         jdebug "lookupSlot ngo {name} returning {rv}"
         rv
         }

  method asString {"ngo:{structure}"}
}

class ngNumber( value' ) {
   inherit ngo
   method value {value'}
   method asString { "ngNumber: {value}"}

   declare "+(_)" asMethod { other ->  ngNumber(value + other.value) } 
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
     case { _ -> print "CANT BE BOTHERED TO APPLY MORE VARARGS" }

   declare(name) asMethod (acceptVarargs(parameters,ctxt,body,false))
}

class ngObject(body,parent) {
  inherit lexicalContext(parent)
  method asString { "ngObject:{structure}" }

  //needs inheritance SHITE
  def initialisers : Sequence[[Node]] = list 
  method addInitialiser(i) {initialisers.add(run(i)inContext(self))}

  declare "outer" asDef( lookup("self" ) ) // we haven't declared self yet so the enclosing self...
  declare "self" asDef(self) //does this make sense? - seems to

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

  jdebug "building"
  for (body) do { e ->
     jdebug "build {e}"
     e.build(self)
  }

  jdebug "initialising"       
  for (initialisers) do { e ->
     jdebug "eval {e}"
     e.eval(self)
  }


  //lexical lookup (internal / implicit)
  method lookup( name ) {  //copy & paste
    jdebug "lookup nuObject {name}"
    def rv = (if (structure.containsKey(name)) 
       then { structure.at(name) }
       else { parent.lookup( name ) })
    jdebug "lookup nuObject {name} {rv}"
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

class lexicalContext(parent) {
  inherit newEmptyContext 

  method lookup( name ) { 
    jdebug "lookup lexicLContext {name}"
    def rv = (if (structure.containsKey(name)) 
       then { structure.at(name) }
       else { parent.lookup( name ) })
    jdebug "lookup lexicalContext {name} {rv}"
    rv

    //     if (structure.containsKey(name)) 
    //        then { structure.at(name) }
    //        else { parent.lookup( name ) }
  }
}


class ngDefBox {
   var boxValue := ngUninitialised
   method initialValue:= (initialValue) {
      if (boxValue != ngUninitialised) then { print "ERROR: can't initialise initailsed box" }
      boxValue := initialValue
   }
   method apply { //can be called as if 'twere a block
      if (boxValue == ngUninitialised) then { print "ERROR: can't access uninitailsed box" }
      boxValue
   }
}

class ngVarBox {
   inherit ngDefBox
   method apply(x) {boxValue:= x}
}

