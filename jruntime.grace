import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports


class exports {
   //I really shouldnt' make everything a class family, should I?
   //at least I should explore traits

  //this is a proxy for a statementseuqnce that should be in the AST
  //understands eval and uses it to run the statements in a body
  //body is a sequnce of statements
  //should actually replace use of sequence - should be visitable etc.
  // If creatio is false, then just pass it in to each
  // If Creatio is true, make a subcontext with creatio = false, 
  //    use the new one for each stmt until the last
  //    then use the original one...

  // REFAC:  should this have a build?  what should it do?

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

    def locals :  Dictionary[[String,Invokeable]] 
        is public     //evil evil making this public.
        = dictionary[[String,Invokeable]]

    ////////////////////////////////////////////////////////////
    ////NEW METHODS FOR DECLARING STUFF
    ////NEW METHODS FOR DECLARING STUFF
    ////NEW METHODS FOR DECLARING STUFF
    ////////////////////////////////////////////////////////////

    //bind a value to a name
    //use for things like self, arguments, things that 
    //the interpreter already has to hand, that DON'T need to be initialised
    method declareName(name) value(value) {
        addLocal(name) asMethod(invocableValue(value))
    }

    //evil method for e.g. sticking in things that ARENT invocable
    //notably, creatios, returncreatios, etc
    method declareName(name) raw(rawValue) {
        addLocal(name) asMethod(rawValue)
    }
    method declareDef(name) properties(properties) {
      def box = ngDefBox(name) properties(properties)
      addLocal(name) asMethod(box) 
    }
    method declareVar(name) properties(properties) {
      def setterName = name ++ ASSIGNMENT_TAIL
      def box = ngVarBox(name) properties(properties)
      addLocal(name) asMethod(box) 
      addLocal(setterName) asMethod(box.setter) 
    }

    //TODO rename asMethod -> asInvokeable?
    //compare declare(name) asMethod(?)
    method addLocal(name) asMethod(m) { 
      if (locals.containsKey(name))
        then { error "trying to declare {name} more than once" }
        elseif { checkForShadowing(name) }
        then { error "{name} shadows lexical definition" }
        else { locals.at(name) put(m)} }

    method checkForShadowing(name) { false } //TODO shadowing checks

    ////////////////////////////////////////////////////////////
    ////
    ////old methods for declaring stuff
    ////
    ////////////////////////////////////////////////////////////


    //should check for lexical shadowning -- but we don't
    //TODO: asMethod -> asInvokeable
    //need to compare addLocal(_)asMethod(_)
    method declare(name) asMethod ( invokeable ) { 
      if (locals.containsKey(name)) 
        then { error "trying to declare {name} more than once" }
        else { locals.at(name) put( invokeable) }
    }

    method declare(name) asDef ( ngo ) { 
      jdebug { print "declare {name} asDef {ngo}" }
      if (locals.containsKey(name)) 
        then { error "trying to declare {name} more than once" }
        else { locals.at(name) put (ngMethodLambda { creatio -> ngo } ) }
    }
    method declare(name) asVar ( ngo ) { 
      jdebug { print "declare {name} asVar {ngo}" }
      if (locals.containsKey(name) || locals.containsKey(name ++ ASSIGNMENT_TAIL)) 
        then { error "trying to declare {name} more than once" }
        else { def box = ngVarBox(name) properties(common.confidentialAnnotations)
               ngVarBox.initialValue:= ngo
               locals.at(name) put(box)             
               locals.at(name ++ ASSIGNMENT_TAIL) put(box.setter)
             }
    }


    ////////////////////////////////////////////////////////////
    ////
    //// lookup
    ////
    ////////////////////////////////////////////////////////////
    
    method lookup(name){
           jdebug { print "lookup ngo {name}" }
           def rv =  locals.at(name) 
           jdebug { print "lookup ngo {name} returning {rv}" }
           rv
           }

    //newone!
    method lookupLocal(name){ 
           jdebug { print "lookupLocal ngo {name}"        }
           def rv = locals.at(name) 
           jdebug { print "lookupLocal ngo {name} returning {rv}" }
           rv
           }

    //method lookupExternal(name) {lookupInheritance(name)}  //for exernal requets
    method lookupExternal(name) {lookupLocal(name)}  //for exernal requets
    method lookupInternal(name) {lookup(name)} //for internal requets


    method asString {"ngo#{dbgCounter}\n{locals.keys}"}
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
     method asString { 
        def sigs = safeFuckingMap { sig -> sig.name } over (value.signatures)
        "ngInterface: {sigs}"}
  }

  class ngBlock(blockNode,ctxt) {
     inherit lexicalContext(ctxt)
     method asString { "\{a ngBlock\}" }

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


  class ngObject(body,ctxt) {
    inherit lexicalContext(ctxt)

    method asString { "ngObject#{dbgCounter}:({status}) {locals.keys}" }

    var status is readable := "embryo"

    def creatio = ctxt.lookup(CREATIO)
    def bottomMost = (false == creatio)
    def whole = (if (bottomMost) then {self} else {creatio})

    jdebug { print "{asString} bottomMost {bottomMost} creatio {ctxt.lookup(CREATIO)}" }

    def inheritParents :  Sequence[[Node]] = list 
    //method addInheritParent(p) {inheritParents.add(inheritFrom(p)inContext(self))}
    def useParents :  Sequence[[Node]] = list 
    //method addUseParent(p) {useParents.add(inheritFrom(p)inContext(self))}

    method addInheritParent(p) { }
    method addUseParent(p) { } 

    declareName "outer" value( lookup("self" ) ) // we haven't declared self yet so the enclosing self...
    declareName "self" value(self) //does this make sense? - seems to

    //this should not use progn because progn is only for methods,
    //where the last statement should be treated differently.
    //here, ALL declarations etc will be treated the same.
    for (body) do { e -> e.build(self) } 


    //RIP OUT INHERITANCE FOR NOW
    //RIP OUT INHERITANCE FOR NOW
    //RIP OUT INHERITANCE FOR NOW
    //RIP OUT INHERITANCE FOR NOW

    //progn(initialisers).eval(self) //whoo! even Freakier!!

    //     jdebug { print "building parents" }

    //     var inheritanceRequestContext := ctxt //inherits clauses evalled in surrounding context
    //     if (bottomMost) then { 
    //        inheritanceRequestContext := ctxt.subcontext
    //        inheritanceRequestContext.declare(CREATIO) asMethod(self)  //inherited constructors wont be bottom
    //     }

    //     for (inheritParents) do {  pp ->
    //       def p = pp.parent
    //       jdebug { print "inherit {p.kind} {p.request} {p.excludes} {p.aliases}" }
    //       //need to *evaluate* the parent's request but with creatio argument set
    //       //in the enclosing context so it knows its not the bottom
    //       //get back an object w/ struture and initaliers - status should be "built"
    //       //add ALL its initialiers into our initialisers list
    //       //process its struture via exclude & inherit; add the stuff in here
    //       //for multiple parents, resolve clashes...

    //       def stupidParentalPartObject = p.request.eval(inheritanceRequestContext) 
    //       if (stupidParentalPartObject.status != "built") 
    //          then { error "FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    //       initialisers.addAll(stupidParentalPartObject.initialisers)
    //       //stupidParentalPartObject.structure.keysAndValuesDo { k, v -> structure.at(k) put(v) }
    //       stupidParentalPartObject.structure.keysAndValuesDo 
    //         { k, v -> if (!p.excludes.contains(k)) then {structure.at(k) put(v) } }
    //       p.aliases.keysAndValuesDo 
    //         { k, v -> structure.at(k) put(stupidParentalPartObject.structure.at(v)) }
    //       //all these need to deal correctly with overriding, and multiple defns. they dont.
    //       //and non-excluded messages...
    //     }

    //     //COPPY AND PASTE: SHOULD ABSTRACT OUT
    //     for (useParents) do {  pp ->
    //       def p = pp.parent
    //       jdebug { print "inherit {p.kind} {p.request} {p.excludes} {p.aliases}" }

    //       def stupidParentalPartObject = p.request.eval(inheritanceRequestContext) //relying on global variable
    //       if (stupidParentalPartObject.status != "built") 
    //          then { error "FUCK FUCK FUCKETY FUCK FUCK FUCK {stupidParentalPartObject.status}" }
    //       initialisers.addAll(stupidParentalPartObject.initialisers)
    //       stupidParentalPartObject.structure.keysAndValuesDo 
    //         { k, v -> if (!p.excludes.contains(k)) then {structure.at(k) put(v) } }
    //       p.aliases.keysAndValuesDo 
    //         { k, v -> structure.at(k) put(stupidParentalPartObject.structure.at(v)) }
    //       //all these need to deal correctly with overriding, and multiple defns. they dont.
    //       //and non-excluded messages...
    //     }


    //     //should there be some other structure here?
    //     //things just replace shit via the map
    //     //
    //     jdebug { print "building local" }
    //     locals.keysAndValuesDo { k, v -> structure.at(k) put(v) } //WRONG but first approximation

    status := "built"

    //if I'm NOT Bottommost then I quit here.
    //structure is complete, but don't run any initialisers.
    //ALL initialisers get run only by the bottomMost constructor
    if (!bottomMost) then { 
       error "SHOULDNT FUCKEN HAPPEN WITH NO INHERITNACE"
       return self // status still built not cooked!
    }

    jdebug { print "initialising - i.e. evaling" }
    for (body) do { e ->
       jdebug { print "eval {e}" }
       e.eval(self)
    }

    status := "cooked" //i.e. OK to go! 

    //lexical lookup (internal / implicit)
    method lookup(name){  //copy & paste // FUCKED
      jdebug { print "lookup nuObject {name}" }
      // if (status != "cooked") then { error "CAIN'T lookup {name} in {self}" }
      def rv = (if (locals.containsKey(name)) 
         then { locals.at(name) }
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
     method asString { "ngImplicitUnknown" } 
  }

  class ngBuiltinAnnotation(description' : String) {
     inherit ngo
     method asString { "ngBuiltinAnnotation(\"{description}\")" } 
     method description { description' }
  }


  class newEmptyContext { 
    inherit ngo
    method asString {"newEmptyContext#{dbgCounter} {locals.keys}"}
    method subcontext {lexicalContext(self)}
  } 

  class lexicalContext(ctxt) {
    inherit newEmptyContext 
     
    method asString {
           "lexicalContext#{dbgCounter} {locals.keys}\n" ++ "!!{ctxt.asString}" }

    method lookup(name){ 
      def rv = (if (locals.containsKey(name)) 
         then { locals.at(name) }
         else { ctxt.lookup(name)})
      rv
    }
  }




  type Invokeable = { 
      invoke(this: NGO) args(args: Sequence[[NGO]]) types(typeArgs: Sequence[[NGO]]) creatio(creatio) -> NGO
      isPublic -> Boolean
      isAbstract -> Boolean
      isOverride -> Boolean
      isMissing -> Boolean //usually false. TRUE if lookup failed!!
      asPublic(Boolean) -> Invokeable
  }


  class ngDefBox(origin) properties(properties) {
     use common.annotationsTrait(properties)
     use changePrivacyAnnotations
     assert {!properties.isAbstract} because "A field can't be abstract"
     var boxValue := ngUninitialised
     method initialValue:= (initialValue) {
        if (boxValue != ngUninitialised) then { error "can't initialise initailsed box" }
        boxValue := initialValue
     }
     method invoke(this) args(args) types(typeArgs) creatio(_) {
        assert {args.size == 0}
        assert {typeArgs.size == 0}
        if (ngUninitialised == boxValue) then { error "can't access uninitailsed box" }
        boxValue
     }
     method asString {"ngDefBox: {origin} = {boxValue}"}

  }

  class ngVarBox(origin) properties(properties) {
     inherit ngDefBox(origin) properties(properties.getter)
       alias defInvoke(_)args(_)types(_)creatio(_) = invoke(_)args(_)types(_)creatio(_)
     method asString {"ngVarBox (getter): {origin} := {boxValue}"}
     
     def setter is public = object {
       use common.annotationsTrait(properties.setter)
       method invoke(this) args(args) types(typeArgs) creatio(creatio) {
          assert {args.size == 1}
          assert {typeArgs.size == 0}
          boxValue:= args.at(1)
          ngDone
         }
     }
  }

  //an invokeable method..
  class ngMethod(methodNode) inContext(ctxt) properties(properties) {
     use common.annotationsTrait(properties)
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       jdebug "invoke method invokable {methodNode.signature.name}"
       def params = methodNode.signature.parameters.asList
       def prognBody = progn(methodNode.body)
       def subtxt = ctxt.subcontext
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i -> subtxt.declare(params.at(i).name) asDef(args.at(i)) }
       subtxt.declare(CREATIO) asMethod(creatio) 
       subtxt.declare(RETURNBLOCK) asMethod {rv -> return rv} 
       subtxt.declare(RETURNCREATIO) asMethod (creatio) 
       prognBody.build(subtxt)
       prognBody.eval(subtxt)
     }
  }

  class ngBlockMethod(blockNode) inContext(ctxt) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       jdebug "invoke block invokable {blockNode.parameters}"
       def params = blockNode.parameters.asList
       def prognBody = progn(blockNode.body)
       def subtxt = ctxt.subcontext
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i -> subtxt.declare(params.at(i).name) asDef(args.at(i)) }
       subtxt.declare(CREATIO) asMethod(creatio) 
       prognBody.build(subtxt)
       prognBody.eval(subtxt)
      }
  }


  //a ngo value bound to a name in a context. already initialised! 
  class invocableValue(value) {
     use common.confidentialAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       assert {(args.size == 0) && (typeArgs.size == 0) && (false == creatio)}
       value
     }
  } 
  //potentially every obejct could be invocable, so we don't need this.
  //too confusing to put in now.


  //what lookup retuns when it doesn't find anything.
  class invocableMissing(description) origin(source) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method isMissing { true }
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{descripton} is missing"
     }
  }




  //old style lambda; takes creatio plus rest. Or something.
  //used for primitives pretty mucg
  class ngMethodLambda(lambda) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       applyVarargs(lambda,args,creatio)
     }
  }


  //behaviour to change privacy annotations 
  trait changePrivacyAnnotations {
    method asPublic(shouldBePublic : Boolean) { 
      if (isPublic == shouldBePublic) 
         then {self}
         else {invokeableWrapper(self) privacy(shouldBePublic)}
    }
  }


  class invokeablePrivacyWrapper(subject) privacy(shouldBePublic) {
    assert (self.isPublic != shouldBePublic)  //or else shold never have got here
    method isPublic { shouldBePublic }

    method asPublic(shouldBePublic : Boolean) {
      if (isPublic == shouldBePublic) 
         then {self}
         elseif {subject.isPublic == shouldBePublic}
         then {subject}
         else { error "asPublic(_): should never happen - excluded middle" }
    }

    method isOverride { subject.isOverride }
    method isAbstract { subject.isAbstract }
    method isMissing { subject.isMissing }
    method invoke(this) args(args) types(typeArgs) creatio(creatio) {
      subject.invoke(this) args(args) types(typeArgs) creatio(creatio) }
  }


  //apply the block to the LIST of arguments.
  //currently this mthod is only used for "old style" varargs
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




}
