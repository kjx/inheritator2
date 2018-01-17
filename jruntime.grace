import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports


class exports {
   //I really shouldnt' make everything a class family, should I?
   //at least I should explore traits

  /////////////////////////////////////////////////////////////

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
          bodyContext.declareName(CREATIO) raw(false) 
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
          bodyContext.declareName(CREATIO) raw(false) 
       }
       var rv := ngDone
       for (body) doWithLast {
          stmt, last -> rv := stmt.build( if (!last) then {bodyContext} else {ctxt} ) }
       rv
     }

  }


  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////
  ////
  //// NGO - nano-grace objects.
  ////
  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////

  type NGO = Unknown


  //for debugging
  var ngoCounter := 0

  // ngo?? shold be ngContext? - perhaps that's a better name!
  // basically a list of local "slots", can declare things, 
  // can look things up.
  // ngo is an **abstract class**
  class ngo { 
    def dbgCounter is readable = ngoCounter
    ngoCounter:= ngoCounter + 1

    def locals :  Dictionary[[String,Invokeable]] 
        is public     //evil evil making this public.
        = dictionary[[String,Invokeable]]

    ////////////////////////////////////////////////////////////
    //// declaraing stuff

    method declareName(name) invokeable ( invokeable ) { 
      addLocal(name) slot(invokeable)
    }
    method declareDef(name) properties(properties) {
      def box = ngDefBox(name) properties(properties)
      addLocal(name) slot(box) 
    }
    method declareVar(name) properties(properties) {
      def setterName = name ++ ASSIGNMENT_TAIL
      def box = ngVarBox(name) properties(properties)
      addLocal(name) slot(box) 
      addLocal(setterName) slot(box.setter) 
    }
    //bind a value to a name - for things like self, arguments, things that 
    //the interpreter already has to hand, that DON'T need to be initialised
    method declareName(name) value(value) {
        addLocal(name) slot(invocableValue(value))
    }
    //evil method for e.g. sticking in things that ARENT invocable
    //notably, creatios, returncreatios, etc. kind of evil
    method declareName(name) raw(rawValue) {
        locals.at(name) put(rawValue)
    }

    //hook method
    method addLocal(name) slot(m) is confidential { 
      if (locals.containsKey(name))
        then { error "trying to declare {name} more than once" }
        elseif { checkForShadowing(name) }
        then { error "{name} shadows lexical definition" }
        else { locals.at(name) put(m) }
    }

    method checkForShadowing(name) is confidential { false } //TODO shadowing checks go here!


    ////////////////////////////////////////////////////////////
    //// lookup

    //generic lookup. crashes if not foun
    //use for pseudo-variables within the interpreter - self, outer, CREATIO etc
    //should *not* need to be overridden. really. 
    method lookup(name){
      def rv = lookupLexical(name)
      match (rv) 
        case { _ : type { isMissing } -> 
          if (rv.isMissing) then { error: "{name} is missing from {self}" } }
        case { _ -> rv }
      }

    //lookup locally, then up the lexical chain 
    method lookupLexical(name){
           locals.at(name) ifAbsent {invokeableMissing(name) origin ("")}
    }

    //lookup purely in local declarations - no inheritance or context
    //mostly used e.g to find the def or var box to initalise;
    //only do c.lookupLocal(n) right after c.addLocal(n)slot(s)
    //crashes on error
    method lookupLocal(name){ locals.at(name)  }


    //called by treewalker for intl and extl requests, resp.
    //needed for actual objects (incl primitives, singletons)
    //but SHOULD NOT be on "plain" contexts
    method lookupInternal(name) { lookupLexical(name) }
    method lookupExternal(name) { lookupLexical(name) }


    method asString {"ngo#{dbgCounter}\n{locals.keys}"}
  }


  /////////////////////////////////////////////////////////////
  ////
  //// empty and lexical contexts
  ////
  /////////////////////////////////////////////////////////////

  class newEmptyContext { //needs a better name 
    inherit ngo
    method asString {"newEmptyContext#{dbgCounter} {locals.keys}"}
    method subcontext {lexicalContext(self)}
  } 

  class lexicalContext(ctxt) {
    inherit newEmptyContext 
     
    method asString {
           "lexicalContext#{dbgCounter} {locals.keys}\n" ++ "!!{ctxt.asString}" }

    method lookupLexical(name){ 
      if (locals.containsKey(name)) 
         then { locals.at(name) }
         else { ctxt.lookupLexical(name)}
     } 
  }


  /////////////////////////////////////////////////////////////
  ////
  //// OBJECTS!!!x
  ////
  /////////////////////////////////////////////////////////////


  class ngObject(body,ctxt) {
    inherit lexicalContext(ctxt)

    method asString { "ngObject#{dbgCounter}:({status}) {locals.keys}" }

    var status is readable := "embryo"

    def creatio = ctxt.lookup(CREATIO)
    def bottomMost = (false == creatio)
    def whole is public = (if (bottomMost) then {self} else {creatio})

    jdebug { print "{asString} bottomMost {bottomMost} creatio {ctxt.lookup(CREATIO)}" }

    //HERE
    //HERE
    //HERE
    declareName "outer" value( lookup("self" ) ) // we haven't declared self yet so the enclosing self...
    declareName "self" value(whole) //does this make sense? - seems to

    //start on inheritance --- list of parent part objects
    def inheritParents :  Sequence[[Node]] = list 
    def useParents :  Sequence[[Node]] = list 

    //context to request parents - surronding context of the object
    //but with creatio set to us, so that we know this is a parent request
    var parentRequestContext := ctxt 
    if (bottomMost) then { 
         parentRequestContext := ctxt.subcontext
         parentRequestContext.declareName(CREATIO) raw(self)  
    }

    //add a parent - recuse if necessary.
    //called back from build() when building an inheritNode
    method addParent(parentNode) { 
      match (parentNode.kind) 
        case { "inherit" -> inheritParents.add(parentNode) }
        case { "use" -> useParents.add(parentNode) }
        case { _ -> error "NOT COBOL!" }

      //need to *evaluate* the parent's request in the parentRequestContext context
      // (with creatio argument set) so it knows its not the bottom
      // get back a "part object" that has been built() but not yet eval()
      def parentalPartObject = parentNode.request.eval(parentRequestContext) 
      assert {parentalPartObject.status == "part"}
      assert {parentalPartObject.whole == whole}

      //store it at the parentID
      declareName(parentNode.parentID) raw(parentalPartObject)
    }
    
    for (body) do { e -> e.build(self) } 

    //this is will set up "locals" by requests back 
    //to "declareName (var, def, invokaeanle, etc)"   for vars and methods
    //and "addParent" for inheritance and use
    //
    //note - does not use progn because progn is only for methods,
    //where the last statement should be treated differently.
    //here, ALL declarations etc will be treated the same.

    status := "built"

    //if I'm NOT Bottommost then I quit here - my structure is complete.
    if (!bottomMost) then { 
       status := "part" 
       return self 
    }

    initialize

    method initialize {   
      jdebug { print "initialising - i.e. evaling" }
      for (body) do { e ->
        jdebug { print "eval {e}" }
        e.eval(self)
      }
    }
    status := "cooked" //i.e. OK to go! 
  } 



  /////////////////////////////////////////////////////////////
  ////
  //// primitivies
  ////
  /////////////////////////////////////////////////////////////

  class ngNumber( value' ) {
     inherit ngo
     method value {value'}
     method asString { "ngNumber: {value}"}

     declareName "+(_)" invokeable( ngMethodLambda { other, creatio ->  
                    def rv = ngNumber(value' + other.value)
                    rv } )

     declareName "asString" invokeable( ngMethodLambda { creatio ->  
                    def rv = ngString(value'.asString)
                    rv } )
  }

  class ngString( value' ) {
     inherit ngo
     method value {value'}
     method asString { "ngString: {value}"}

     declareName "++(_)" invokeable( ngMethodLambda { other, creatio ->  
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

     declareName(name) invokeable (ngBlockMethod(blockNode) inContext(ctxt))
  }

  /////////////////////////////////////////////////////////////
  ////
  //// singletons
  ////
  /////////////////////////////////////////////////////////////

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


  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////// Invokeables
  //////// Invokeables
  //////// Invokeables
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////

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
       for (params.indices) do { i -> subtxt.declareName(params.at(i).name) value(args.at(i)) }
       subtxt.declareName(CREATIO) raw(creatio) 
       subtxt.declareName(RETURNBLOCK) raw {rv -> return rv} 
       subtxt.declareName(RETURNCREATIO) raw (creatio) 
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
       for (params.indices) do { i -> subtxt.declareName(params.at(i).name) value(args.at(i)) }
       subtxt.declareName(CREATIO) raw(creatio) 
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
  class invocableMissing(name) origin(source) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method isMissing { true }
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is missing from {source}"
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

  //proxy to change an invokeable's privacy
  class invokeableWrapper(subject) privacy(shouldBePublic) {
    assert (self.isPublic != shouldBePublic)  //or else shouldn't be here
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


  //old style invocable that wraps a lambda block; 
  //blocks takes arguments plus creatio. 
  //use for primitives but otherwise avoid
  class ngMethodLambda(lambda) {
    use common.publicAnnotations
    use changePrivacyAnnotations
    method invoke(this) args(args) types(typeArgs) creatio(creatio) {
      applyVarargs(lambda,args,creatio)
    }
    //apply the block to the LIST of arguments from the interpreter
    //args are already evaluated
    method applyVarargs(block,args,creatio) {
      def a = args.asList
      match (args.size)
        case { 0 -> block.apply(creatio)}
        case { 1 -> block.apply(a.at(1),creatio)}
        case { 2 -> block.apply(a.at(1),a.at(2),creatio)}
        case { 3 -> block.apply(a.at(1),a.at(2),a.at(3),creatio)}
        case { 4 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),creatio)}
        case { 5 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),a.at(5),creatio)}
        case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }
    }
  }


}
