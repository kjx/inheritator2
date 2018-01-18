import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports

method debugPrint(string) {}

class exports {
  debugPrint "EEXxPORTSTSZ"

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

  class progn (body) {
     debugPrint "progn!"

     method eval(ctxt) { 
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
  var ngoCounter is public := 0

  // ngo?? shold be ngContext? - perhaps that's a better name!
  // basically a list of local "slots", can declare things, 
  // can look things up.
  // ngo is an **abstract class**
  class ngo { 
    method kind {"ngo"}
    def dbgCounter is readable = ngoCounter
    ngoCounter:= ngoCounter + 1

    debugPrint "making {kind}#{dbgCounter}"

    def locals :  Dictionary[[String,Invocable]] 
        is public     //evil evil making this public.
        = dictionary[[String,Invocable]]



    ////////////////////////////////////////////////////////////
    //// declaraing stuff

    method declareName(name) invocable ( invocable ) { 
      addLocal(name) slot(invocable)
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
      def rv = lookupInternal(name)
      match (rv) 
        case { _ : type { isMissing } -> 
          if (rv.isMissing) then { error: "{name} is missing from {self}" } }
        case { _ -> rv }
      }

    //lookup purely in local declarations - no inheritance or context
    //mostly used e.g to find the def or var box to initalise;
    //only do c.lookupLocal(n) right after c.addLocal(n)slot(s)
    method lookupLocal(name) is notOverrideable { 
      locals.at(name) ifAbsent {invocableMissing(name) origin ("Llexical in #{dbgCounter}")}}
      
    //like lookupLocal but crashes on error
    method getLocal(name){ locals.at(name)  }

    //called to get only lexical responses for e.g. inheritance vs lexical resolution
    method lookupLexical(name) {lookupLocal(name)}

    //called by treewalker for intl and extl requests, resp.
    //needed for actual objects (incl primitives, singletons)
    method lookupInternal(name){  
           debugPrint "lookupInternal(ngo) {name} #{dbgCounter} {locals.keys}" 
           lookupLocal(name)
    }

    method lookupExternal(name) { error "called lookupExternal {name} on non-object {self}" }

    method asString {"ngo#{dbgCounter}\n{locals.keys}"}
  }


  /////////////////////////////////////////////////////////////
  ////
  //// empty and lexical contexts
  ////
  /////////////////////////////////////////////////////////////

  class newEmptyContext { //needs a better name - could jsut get rit of it.
    inherit ngo
    method kind {"newEmptyContext"}
    method asString {"newEmptyContext#{dbgCounter} {locals.keys}"}
    method subcontext {lexicalContext(self)}
  } 

  class lexicalContext(ctxt) { 
    inherit newEmptyContext 
        
    method kind {"lexicalContext"}

    method asString {
           "lexicalContext#{dbgCounter} {locals.keys}\n!!{ctxt.asString}" }

    method lookupLexical(name){ 
      debugPrint "lookupLexical(context) {name} #{dbgCounter} {locals.keys}" 
      locals.at(name) ifAbsent {ctxt.lookupLexical(name)} 
      }

    method lookupInternal(name){ 
      debugPrint "lookupInternal(context) {name} #{dbgCounter} {locals.keys}" 
      locals.at(name) ifAbsent {ctxt.lookupInternal(name)} 
      }

     } 
  


  /////////////////////////////////////////////////////////////
  ////
  //// OBJECTS!!!
  ////
  /////////////////////////////////////////////////////////////


  class ngObject(body,ctxt) {
    inherit lexicalContext(ctxt)
    method kind{"ngObject"}

    var status is readable := "embryo"

    method asString { "ngObject#{dbgCounter}:({status}) {locals.keys}\n!!{ctxt.asString}" }



    def creatio = ctxt.lookup(CREATIO)
    def bottomMost = (false == creatio)
    def whole is public = (if (bottomMost) then {self} else {creatio})

    declareName "outer" value( ctxt.lookup("self" ) ) // we haven't declared self yet so the enclosing self...
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

      debugPrint "addParent {parentNode.request.name} to#{self.dbgCounter} ctxt#{parentRequestContext.dbgCounter}"
      debugPrint "{parentRequestContext}"

      //need to *evaluate* the parent's request in the parentRequestContext context
      // (with creatio argument set) so it knows its not the bottom
      // get back a "part object" that has been built() but not yet eval()

      debugPrint "\nOBJECT parental request {parentNode.request.name} self#{dbgCounter} {parentRequestContext}"
      def parentalPartObject = parentNode.request.eval(parentRequestContext) 
      assert {parentalPartObject.status == "part"}
      assert {parentalPartObject.whole == whole}

      //store it at the parentID
      declareName(parentNode.parentID) raw(parentalPartObject)
      debugPrint "PARENT {parentNode.parentID} #{parentalPartObject.dbgCounter}"
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
      for (body) do { e ->
        e.eval(self)
      }
    }
    status := "cooked" //i.e. OK to go! 

    method lookupExternal(name) { lookupInheritance(name) }


    method lookupInheritance(name) {
      debugPrint "lookupInheritance({name}) in {self}"
      //doesn't deal with overrides OR abstract/required/...
      def localDefn = lookupLocal(name)  
      def useCandidates = findCandidates(name) parents(useParents)
      def inheritCandidates = findCandidates(name) parents(inheritParents)

      debugPrint "   (localDefn) {localDefn}"
      debugPrint "   (useCandidates) {useCandidates}"
      debugPrint "   (inheritCandidates) {inheritCandidates}"

      if (!localDefn.isMissing) then {
         if (localDefn.isOverride && ((useCandidates.size + inheritCandidates.size) == 0))
           then { error "{name} in {self} isOverride but doesn't override anything" }
           else { return localDefn } }

      if (useCandidates.size == 1) then {return useCandidates.at(1) }
      if (useCandidates.size > 1) then {return invocableAmbiguous(name) origin(self) between(useCandidates)}
      assert {useCandidates.size == 0}


      if (inheritCandidates.size == 1) then {return inheritCandidates.at(1) }
      if (inheritCandidates.size > 1) then {return invocableAmbiguous(name) origin(self) between(inheritCandidates)}
      assert {inheritCandidates.size == 0}

      invocableMissing(name) origin(self)

    }

    method findCandidates(name)parents(parents) {
      def candidates = list
      for (parents) do { parentNode -> 
        debugPrint "findCandidates({name}) in {self}"
        debugPrint "   excludes {parentNode.excludes}"
        debugPrint "   aliases {parentNode.aliases}"
        if (!parentNode.excludes.contains(name)) then {
           def parentName = parentNode.aliases.at(name) ifAbsent{name}
           def parentPartObject = getLocal(parentNode.parentID) 
           def parentDefn = parentPartObject.lookupInheritance(parentName)
           if ((!parentDefn.isMissing) && (!parentDefn.isAbstract)) 
              then {candidates.add(parentDefn)}
      } }
      candidates    
    }

    method isMissing(thingy) {
      match (thingy) 
        case { invocable : type { isMissing } -> invocable.isMissing }
        case { _ -> false }
    }

    method lookupLexical(name) { lookupInternal(name) }

    method lookupInternal(name) {
       debugPrint "lookupInternal(object) {name} #{dbgCounter} {locals.keys}" 
       def localDefn = lookupLocal(name)  
       debugPrint "   (local) {localDefn}"
       def lexicalResult = ctxt.lookupLexical(name)
       debugPrint "   (lexical) {lexicalResult}"
       def inheritanceResult = lookupInheritance(name)
       debugPrint "   (inheritance) {inheritanceResult}"

       if (!isMissing(localDefn)) 
         then {localDefn}
         elseif {isMissing(lexicalResult) && isMissing(inheritanceResult)}
         then {invocableMissing(name) origin(self)}
         elseif {isMissing(inheritanceResult)}
         then {lexicalResult}
         elseif {isMissing(lexicalResult) || (inheritanceResult == lexicalResult)}
         then {whole.lookupInheritance(name)} 
         else {invocableAmbiguous(name) origin(self) between(list(lexicalResult,inheritanceResult))}
    }   






  }



  /////////////////////////////////////////////////////////////
  ////
  //// primitivies
  ////
  /////////////////////////////////////////////////////////////

  class ngPrimitive {
    inherit ngo

    method lookupExternal(name) { lookupLocal(name) }  //primitives only have local slots
  }
  
  class ngNumber( value' ) {
     inherit ngPrimitive
     method kind {"ngNumber"}
     method value {value'}
     method asString { "ngNumber: {value}"}

     declareName "+(_)" invocable( ngMethodLambda { other, creatio ->  
                    def rv = ngNumber(value' + other.value)
                    rv } )

     declareName "asString" invocable( ngMethodLambda { creatio ->  
                    def rv = ngString(value'.asString)
                    rv } )
  }

  class ngString( value' ) {
     inherit ngPrimitive
     method kind {"ngString"}
     method value {value'}
     method asString { "ngString: {value}"}

     declareName "++(_)" invocable( ngMethodLambda { other, creatio ->  
                    def rv = ngString(value' ++ other.value)
                    rv } )

  }

  class ngInterface( value', ctxt ) {   
            //cheating, just points to ast node - and context
     inherit ngPrimitive
     method kind {"ngINrerface"}
     method value {value'}
     method asString { 
        def sigs = safeFuckingMap { sig -> sig.name } over (value.signatures)
        "ngInterface: #{dbgCounter} {sigs}"}
  }

  class ngBlock(blockNode,ctxt) {
     inherit lexicalContext(ctxt)
     method lookupExternal(name) { lookupLocal(name) }
     method asString { "\{a ngBlock\} #{dbgCounter}" }
     method kind {"ngBlock"}
     def p = blockNode.parameters.asList
     def name = match (p.size)
       case { 0 -> "apply" }
       case { 1 -> "apply(_)" }
       case { 2 -> "apply(_,_)" }
       case { 3 -> "apply(_,_,_)" }
       case { 4 -> "apply(_,_,_,_)" }
       case { 5 -> "apply(_,_,_,_,_)" }
       case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }

     declareName(name) invocable (ngBlockMethod(blockNode) inContext(ctxt))
  }

  /////////////////////////////////////////////////////////////
  ////
  //// singletons
  ////
  /////////////////////////////////////////////////////////////


  def ngDone is public = object {
     inherit ngPrimitive
     method kind{"ngDone"}
     method asString { "ngDone"}
  }

  def ngBuild is public = object {
     inherit ngPrimitive
     method kind {"ngBUild"}
     method asString { "ngBuild"} //result returned from build. always an error.
  }

  def ngUninitialised is public = object {
     inherit ngPrimitive
     method kind {"ngUninit"}
     method asString { "ngUninitialised" } //also an error if accessed
  }

  def ngImplicitUnknown is public = object {
     inherit ngPrimitive
     method kind {"ngImplicitU"}
     method asString { "ngImplicitUnknown" } 
  }

  class ngBuiltinAnnotation(description' : String) {
     inherit ngPrimitive                         
     method kind {"ngBuiltinAnnotation"}
     method asString { "ngBuiltinAnnotation(\"{description}\")" } 
     method description { description' }
  }


  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////// Invocables
  //////// Invocables
  //////// Invocables
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////

  type Invocable = { 
      invoke(this: NGO) args(args: Sequence[[NGO]]) types(typeArgs: Sequence[[NGO]]) creatio(creatio) -> NGO
      isPublic -> Boolean
      isAbstract -> Boolean
      isOverride -> Boolean
      isMissing -> Boolean //usually false. TRUE if lookup failed!!
      asPublic(Boolean) -> Invocable
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
       method asString {"ngVarBox (setter): {origin} := {boxValue}"}
       method invoke(this) args(args) types(typeArgs) creatio(creatio) {
          assert {args.size == 1}
          assert {typeArgs.size == 0}
          boxValue:= args.at(1)
          ngDone
         }
     }
  }

  //an invocable method..
  class ngMethod(methodNode) inContext(ctxt) properties(properties) {
     use common.annotationsTrait(properties)
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
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
     method asString {"ngMethod: {methodNode.signature.name} #{ctxt.dbgCounter}"}
  }

  class ngBlockMethod(blockNode) inContext(ctxt) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       def params = blockNode.parameters.asList
       def prognBody = progn(blockNode.body)
       def subtxt = ctxt.subcontext
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i -> subtxt.declareName(params.at(i).name) value(args.at(i)) }
       subtxt.declareName(CREATIO) raw(creatio) 
       prognBody.build(subtxt)
       prognBody.eval(subtxt)
      }
     method asString {"ngBlockMethod"}
  }


  //a ngo value bound to a name in a context. already initialised! 
  class invocableValue(value) {
     use common.confidentialAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       assert {(args.size == 0) && (typeArgs.size == 0) && (false == creatio)}
       value
     }
     method asString {"invocableValue: {value}"}
  } 
  //potentially every obejct could be invocable, so we don't need this.
  //too confusing to put in now.

  var imCtr := 0

  //what lookup retuns when it doesn't find anything.
  class invocableMissing(name) origin(source) {
     imCtr := imCtr + 1
     debugPrint "imCRT:{imCtr}"
     //if (imCtr == 700) then { error "CTASH" }
     use common.publicAnnotations
     use changePrivacyAnnotations
     method isMissing { true }
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is missing from {source}"
     }
     method asString {"invocableMIssing: {name}"}
  }

  //what lookup retuns when it doesn't find anything.
  class invocableAmbiguous(name) origin(source) between(possiblities) {
     inherit invocableMissing(name) origin(source)
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is ambiguous at {source} between {possibilities}"
     }
     method asString {"inocableAmbiguous {name}"}
  }


  //behaviour to change privacy annotations 
  trait changePrivacyAnnotations {
    method asPublic(shouldBePublic : Boolean) { 
      if (isPublic == shouldBePublic) 
         then {self}
         else {invocableWrapper(self) privacy(shouldBePublic)}
    }
  }

  //proxy to change an invocable's privacy
  class invocableWrapper(subject) privacy(shouldBePublic) {
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
    method asString {"inocableWrapper {subject}"}
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
    method asString { "ngMethodLambda {lambda}" }
  }
}
