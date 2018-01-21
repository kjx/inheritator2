import "jast" as jm
import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports
import "jinvocables" as invocables
import "jprimitives" as primitives


method debugPrint(string) {}


class earlyDefinitions { 
  type NGO = Unknown

  //for debugging
  var contextCounter is public := 0
}

class exports {
  inherit earlyDefinitions
  inherit primitives.primitivesFamily
  inherit invocables.invocablesFamily
  

  //I really shouldnt' make everything a class family, should I?
  //at least I should explore traits

  /////////////////////////////////////////////////////////////

  //this is a proxy for a statementseuqnce that should be in the AST
  //understands eval and uses it to run the statements in a body
  //body is a sequnce of statements
  //should actually replace use of sequence - should be visitable etc.
  // If creation.isCreatio is false, then just pass it in to each
  // If Creatio.isCreatio is true, make a subcontext with ngNotCreatio
  //    use the new one for each stmt until the last
  //    then use the original one...

  class progn (body) {
     method build(ctxt) {
       var bodyContext
       if (! ctxt.getInternal(CREATIO).value.isCreatio) then {
          bodyContext := ctxt
       } else { 
          bodyContext := ctxt.subcontext
          bodyContext.addLocal(CREATIO) value(ngNotCreatio) 
       }
       var rv := ngDone
       for (body) doWithLast {
          stmt, last -> rv := stmt.build( if (!last) then {bodyContext} else {ctxt} ) }
       rv
     }
     method eval(ctxt) { 
       var bodyContext
       if (! ctxt.getInternal(CREATIO).value.isCreatio) then {
          bodyContext := ctxt
       } else { 
          bodyContext := ctxt.subcontext
          bodyContext.addLocal(CREATIO) value(ngNotCreatio) 
       }
       var rv := ngDone
       for (body) doWithLast { 
         stmt, last -> rv := stmt.eval( if (!last) then {bodyContext} else {ctxt} ) }
       rv
     }

  }


  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////
  ////
  //// CONTEXT - nano-grace objects.
  ////
  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////


  // a context is a scope where you can declare things,
  // and look them up.  Subclasses get more complex, 
  // including lexical scoping (in lexicalContext)
  // and inheritance (in objectContext aka object)
  class context { 
    method kind {"context"}
    def dbg is readable = contextCounter
    contextCounter:= contextCounter + 1

    def locals :  Dictionary[[String,Invocable]] 
        = dictionary[[String,Invocable]]

    ////////////////////////////////////////////////////////////
    //// local declarations 

    method declareName(name) invocable ( invocable ) { 
      if ( hasLocal(name) )
        then { error "trying to declare {name} more than once" }
        elseif { checkForShadowing(name) }
        then { error "{name} shadows lexical definition" }
        else { addLocal(name) slot(invocable) }
    }

    method checkForShadowing(name) is confidential { 
       //!findInternalDeclaringContext(name).isMissing //forbid all shadowing
       false  //don't forbit any shadowing
    } 

    //declare a Def which must later be initialised
    method declareDef(name) properties(properties) {
      def box = invocableDef(name) properties(properties) inContext(self)
      declareName(name) invocable(box) 
    }

    //declare a Var which must later be initialised
    method declareVar(name) properties(properties) {
      def setterName = name ++ ASSIGNMENT_TAIL
      def box = invocableVar(name) properties(properties) inContext(self)
      declareName(name) invocable(box) 
      declareName(setterName) invocable(box.setter) 
    }

    //bind a value to a name - for things like arguments, that 
    //the interpreter already has to hand, that DON'T need to be initialised
    method declareName(name) value(value) {
        declareName(name) invocable(invocableValue(value) inContext(self))
    }

    //bind an host-interpreter lambda to a name - typically for primitives
    method declareName(name) lambda(lambda) {
        declareName(name) invocable(invocableLambda(lambda) inContext(self))
    }


    //accessing local declarations
    method addLocal(name) slot(m) { locals.at(name) put(m) }
    method addLocal(name) value(m) { locals.at(name) put(invocableValue(m) inContext(self)) }
    method hasLocal(name) { locals.containsKey(name) }
    method getLocal(name) { lookupLocal(name) ifAbsent { error "local {name} missing in #{dbg}"} }
    method lookupLocal(name) ifAbsent(block) { locals.at(name) ifAbsent(block) }
    method lookupLocal(name) {
       lookupLocal(name) ifAbsent { invocableMissing(name) inContext(self) } }

    ////////////////////////////////////////////////////////////
    //// lookups
    ////
    ////  getX crashes if not found
    ////  lookupX returns a missing invocable (or whatever)
    ////  findXDeclaringPart returns the context (which may or may not be an object)
    ////      that has the declaration of the named attribute
    ////  External lookups only look at inheritance, Internal looks consider nesting also
    
    method getInternal(name){
      def rv = lookupInternal(name)
      match (rv) 
        case { _ : type { isMissing } -> 
          if (rv.isMissing) then { error: "{name} is missing from {self}" } }
        case { _ -> rv }
      rv
      }
    method lookupInternal(name) {
      def invocable = lookupDeclaration(name)
      def whole = invocable.context.whole
      whole.lookupInheritance(name) 
      }

    method lookupDeclaration(name){ lookupLocal(name) }
 
    method XXfindInternalDeclaringContext(name) {
        //debugPrint "findInternalDeclaringContext(context) {name} #{dbg} {locals.keys}" 
       if (locals.containsKey(name)) then {
          //debugPrint "  found localMH {name} context#{dbg}"
          return self
          } else {
          //debugPrint "   missing"
          invocableMissing(name) inContext(self)
          }
    }

    //external lookups consider only inheritance
    method lookupExternal(name) {lookupInheritance(name)}
    method lookupInheritance(name) {lookupLocal(name)} 

    //where there are parental part objects, this is the whole object
    //to which they belong
    method whole {self} 

    //create a subcontext nested inside this context
    method subcontext {lexicalContext(self)}
    method isInside(other) {self == other}

    //misc
    method asString {"context#{dbg}\n{locals.keys}"}

    method isCreatio { true } 
  }


  /////////////////////////////////////////////////////////////
  ////
  //// lexical contexts
  ////
  /////////////////////////////////////////////////////////////

  //a lexicaContext is a context that is nested inside another context
  class lexicalContext(ctxt) { 
    inherit context
        
    method kind {"lexicalContext"}

    method asString {
           "lexicalContext#{dbg} {locals.keys}\n!!{ctxt.asString}" }

    method lookupDeclaration(name) {
        lookupLocal(name)ifAbsent {lookupEnclosingDeclaration(name)} }

    method lookupEnclosingDeclaration(name) { ctxt.lookupDeclaration(name) }
    
    method XXXfindInternalDeclaringContext(name){ 
      if (locals.containsKey(name)) then {
          self
          } else {
          ctxt.findInternalDeclaringContext(name)} 
          }

    method isInside(other) {(self == other) || ctxt.isInside(other)}
    }
  


  /////////////////////////////////////////////////////////////
  ////
  //// OBJECTS!!!
  ////
  /////////////////////////////////////////////////////////////

  //this class represents objecs in the underlying interpreter
  //it's called "objectContext" mainly because "object" is already taken by Grace
  //an object is essentially a lexical context that also supports inheritance.
  //to resolve inheritance correctly we have to maintain the contexts of
  //each contributing object constructor - these are "parental part objects" 
  //i.e. objectContexts that represent an inherited part of another "whole" object
  //only objectContexts that represent whole objects are accessible to interpreted programs;
  class objectContext(body,ctxt) {
    inherit lexicalContext(ctxt)
    method kind{"objectContext"}

    var status is readable := "embryo"

    def creatio = ctxt.getInternal(CREATIO).value
    def bottomMost = (!creatio.isCreatio)
    def whole is public = (if (bottomMost) then {self} else {creatio})

    addLocal "outer" slot(ctxt.getInternal("self" )) 
    addLocal "self"  value(whole) 

    //context to use to request parents - the surrounding context of this objectContext
    //but with a creatio set to us, so that we will know this is a parent request
    var parentRequestContext := ctxt 
    if (bottomMost) then { 
         parentRequestContext := ctxt.subcontext
         parentRequestContext.addLocal(CREATIO) value(self)  
    }

    //list of AST nodes for inherit and request clauses
    //note these are AST nodes, not parental part objects
    def inheritParentNodes :  Sequence[[Node]] = list 
    def useParentNodes :  Sequence[[Node]] = list 

    //build the individual declarations into this object    
    //by going through the body of the object constructor in the AST
    //this is will set up declarations by double-dispatch requests back 
    //to "declareName (var, def, invokaeanle, etc)"   for vars and methods
    //and "addParent" for inheritance and use
    //build() doesn't do anyting for inline code or initialisation
    for (body) do { e -> e.build(self) } 

    status := "built"

    //if I'm NOT Bottommost then I must be a parental part object to some other whole
    //my structure is now completly built() so I stop here
    //I'll be initialised in the right order as the parentNode in my inheriting 
    //object is eval()'ed
    if (!bottomMost) then { 
       status := "part" 
       return self 
    }

    initialize
    status := "cooked" //i.e. OK to go! 

    //"constructor" code ends here///

    method initialize {   
      for (body) do { e ->
        e.eval(self)
      }
    }


    //add a parent represented by a parentNode from the Common AST
    //an inheritNode double-dispaches back her in the build() phase
    method addParent(parentNode) { 

      //add the AST node to the appropriate list
      match (parentNode.kind) 
        case { "inherit" -> inheritParentNodes.add(parentNode) }
        case { "use" -> useParentNodes.add(parentNode) }
        case { _ -> error "NOT COBOL!" }

      //now we actually make the parental request to create the parental part object
      def parentalPartObject = parentNode.request.eval(parentRequestContext) 
      assert {parentalPartObject.status == "part"}
      assert {parentalPartObject.whole == whole}

      //store the parental part object as a pseudo-field in the inheriting objectContext
      addLocal(parentNode.parentID) slot(parentalPartObject)
    }

    //////////////////////////////////////////////////
    //lookup methods



    method XlookupInheritance(name) { findInheritanceInContext(name).declaration }

    method XfindInheritanceDeclaringContext(name) { findInheritanceInContext(name).inContext }

    method lookupInheritance(name) {
      debugPrint "lookupInheritance({name}) in {self}"
      def localDefn = lookupLocal(name)  
      def useCandidates = findCandidates(name) parents(useParentNodes)
      def inheritCandidates = findCandidates(name) parents(inheritParentNodes)

      debugPrint "   (localDefn) {localDefn}"
      debugPrint "   (useCandidates) {useCandidates}"
      debugPrint "   (inheritCandidates) {inheritCandidates}"

      if (!localDefn.isMissing) then {
         if (localDefn.isOverride && ((useCandidates.size + inheritCandidates.size) == 0))
           then { error "{name} in {self} isOverride but doesn't override anything" }
           else { return localDefn } }

      if (useCandidates.size == 1) then {return useCandidates.at(1) }
      if (useCandidates.size > 1) then {return invocableAmbiguous(name) inContext(self) between(useCandidates)}
      assert {useCandidates.size == 0}


      if (inheritCandidates.size == 1) then {return inheritCandidates.at(1) }
      if (inheritCandidates.size > 1) then {return invocableAmbiguous(name) inContext(self) between(inheritCandidates)}
      assert {inheritCandidates.size == 0}

      invocableMissing(name) inContext(self)
    }
    
    method XfindInheritanceInContext(name) { 
      def localCandidate = declaration(lookupLocal(name))inContext(self)
      def useCandidates = findCandidatesInContext(name) parents(useParentNodes)
      def inheritCandidates = findCandidatesInContext(name) parents(inheritParentNodes)

      def localDefn = localCandidate.declaration

      if (!localDefn.isMissing) then {
         if (localDefn.isOverride && ((useCandidates.size + inheritCandidates.size) == 0))
           then { error "{name} in {self} isOverride but doesn't override anything" }
           else { return localCandidate } }

      if (useCandidates.size == 1) then {return useCandidates.at(1) }
      if (useCandidates.size > 1) then {return declaration(invocableAmbiguous(name) inContext(self) between(useCandidates))inContext(ambiguousContext(name,self,"use",useCanidates))}
      assert {useCandidates.size == 0}


      if (inheritCandidates.size == 1) then {return inheritCandidates.at(1) }
      if (inheritCandidates.size > 1) then {return declaration(invocableAmbiguous(name) inContext(self) between(inheritCandidates))inContext(ambiguousContext(name,self,"inherit",inheritCanidates))}
      assert {inheritCandidates.size == 0}

      declaration(invocableMissing(name) inContext(self))inContext(self)
     }



    method XfindInheritanceDeclaringContext(name) {
      //debugPrint "findFuckingINHERITANCEMH({name}) in {self}"
      def localDefn = lookupLocal(name)  
      def useCandidates = findfuckingCandidateMethodHolders(name) parents(useParentNodes)
      def inheritCandidates = findfuckingCandidateMethodHolders(name) parents(inheritParentNodes)

      //debugPrint "   (ffinh localDefn) {localDefn}"
      //debugPrint "   (ffinh useCandidates) {useCandidates}"
      //debugPrint "   (ffinh inheritCandidates) {inheritCandidates}"

      //doesn't deal with overides
      if (!localDefn.isMissing) then {
         if (localDefn.isOverride && ((useCandidates.size + inheritCandidates.size) == 0))
           then { error "{name} in {self} isOverride but doesn't override anything" }
           else { return self } }  //return the method holder not the method!

      if (useCandidates.size == 1) then {return useCandidates.at(1) }
      if (useCandidates.size > 1) then {return declaration(invocableAmbiguous(name) inContext(self) between(useCandidates))inContext(ambiguousContext(name,self,"use",useCanidates))}
      assert {useCandidates.size == 0}

      if (inheritCandidates.size == 1) then {return inheritCandidates.at(1) }
      if (inheritCandidates.size > 1) then {return declaration(invocableAmbiguous(name) inContext(self) between(inheritCandidates))inContext(ambiguousContext(name,self,"inherit",inheritCanidates))}
      assert {inheritCandidates.size == 0}

      missingContext(name,self)
    }

  

    method XfindCandidates(name)parents(parents) {
      map { dInC -> dInC.declaration } 
        over (findCandidatesInContext(name)parents(parents))
    }

    method XfindfuckingCandidateMethodHolders(name)parents(parents) {
      map { dInC -> dInC.inContext } 
        over (findCandidatesInContext(name)parents(parents))
    }

    method findCandidates(name)parents(parents) {
      def candidates = list
      for (parents) do { parentNode -> 
        if (!parentNode.excludes.contains(name)) then {
           def parentName = parentNode.aliases.at(name) ifAbsent{name}
           def parentPartObject = getLocal(parentNode.parentID) 
           def parentDefn = parentPartObject.lookupInheritance(parentName)
           if ((!parentDefn.isMissing) && (!parentDefn.isAbstract)) 
              then {candidates.add(parentDefn)}
      } }
      candidates    
    }

    method XfindfuckingCandidateMethodHolders(name)parents(parents) {
      def candidates = list
      for (parents) do { parentNode -> 
        if (!parentNode.excludes.contains(name)) then {
           def parentName = parentNode.aliases.at(name) ifAbsent{name}
           def parentPartObject = getLocal(parentNode.parentID) 
           def parentDefn = parentPartObject.findInheritanceDeclaringContext(parentName)
           if (!isMissing(parentDefn)) then {
              candidates.add(parentDefn)  //should deal with abstract!
      } } }
      candidates    
    }

    method XfindCandidatesInContext(name)parents(parents) {
      def candidatesInContext  = list
      for (parents) do { parentNode -> 
        //debugPrint "findCandidates({name}) in {self}"
        //debugPrint "   excludes {parentNode.excludes}"
        //debugPrint "   aliases {parentNode.aliases}"
        if (!parentNode.excludes.contains(name)) then {
           def parentName = parentNode.aliases.at(name) ifAbsent{name}
           def parentPartObject = getLocal(parentNode.parentID) 
           def parentContext = parentPartObject.findInheritanceDeclaringContext(parentName)
           def parentDefn = parentPartObject.lookupInheritance(parentName) //can do better later?
           if ((!parentDefn.isMissing) && (!parentDefn.isAbstract)) 
              then {candidatesInContext.add(
                      declaration(parentDefn) inContext(parentContext) )
      } } }
      candidatesInContext
    }


    //auxilliary class that pairs up declarations and contexts
    //so we only need one set of lookup code
    class Xdeclaration(d)inContext(c) {
      match(d) 
         case { _ : type { contextImIn } -> assert {d.contextImIn == c} }
         case { _ -> }
      method declaration {d}
      method inContext {c}  //can't be called "context" due do shadowing. grrr
    }
      


    method isMissing(thingy) { 
      match (thingy) 
        case { _ : type { isMissing } -> thingy.isMissing } 
        case { _ -> thingy}
      }

    method isAbstract(parentDefn,name) {
      def invocable = parentDefn.locals.at(name)        //CRASH if not found, parentDefn *must be* a method holder!!!
      invocable.isAbstract
    }

    method lookupDeclaration(name) {
       debugPrint "lookupDeclaration(object) {name} #{self}" 
       if (hasLocal(name)) then {return getLocal(name)}
       //debugPrint "   (ffimh local #{dbg}) NOT FOUND"

       def inheritanceResult = lookupInheritance(name)
       //debugPrint "   (ffimh inheritance #{dbg}) {inheritanceResult}"

       def lexicalResult = lookupEnclosingDeclaration(name)
       //debugPrint "   (ffimh lexical #{dbg}) {lexicalResult}"

       //debugPrint "INHERIT #{dbg} {inheritanceResult} {isMissing(inheritanceResult)}"
       //debugPrint "LEXICAL #{dbg} {lexicalResult} {isMissing(lexicalResult)}"

       if (isMissing(lexicalResult) && isMissing(inheritanceResult))
         then {invocableMissing(name) inContext(self)}
         elseif {isMissing(inheritanceResult)}
         then {lexicalResult}
         elseif {isMissing(lexicalResult) || (inheritanceResult == lexicalResult)}
         then {inheritanceResult}
         else {invocableAmbiguous(name) inContext(self)
               //error "ffIMH ambi-fucked #{dbg} {lexicalResult} {inheritanceResult}"
           }
    }

    

    method XXXfindInternalDeclaringContext(name) {
       //debugPrint "findInternalDeclaringContext(object) {name} #{self}" 
       if (locals.containsKey(name)) then {
          //debugPrint "   (ffimh found localMH {name} obj#{dbg}"
          return self
          }
       //debugPrint "   (ffimh local #{dbg}) NOT FOUND"

       def inheritanceResult = findInheritanceDeclaringContext(name)
       //debugPrint "   (ffimh inheritance #{dbg}) {inheritanceResult}"

       def lexicalResult = ctxt.findInternalDeclaringContext(name)
       //debugPrint "   (ffimh lexical #{dbg}) {lexicalResult}"

       //debugPrint "INHERIT #{dbg} {inheritanceResult} {isMissing(inheritanceResult)}"
       //debugPrint "LEXICAL #{dbg} {lexicalResult} {isMissing(lexicalResult)}"

       if (isMissing(lexicalResult) && isMissing(inheritanceResult))
         then {missingContext(name, self)}
         elseif {isMissing(inheritanceResult)}
         then {lexicalResult}
         elseif {isMissing(lexicalResult) || (inheritanceResult == lexicalResult)}
         then {inheritanceResult}
         else {error "ffIMH ambi-fucked #{dbg} {lexicalResult} {inheritanceResult}"}
    }

    method asString { "objectContext#{dbg}:({status}) {locals.keys}\n!!{ctxt.asString}" }
  }

  //a pseudo-context returned when a lookup can't find anything
  //may or may not be an error given we can have definitions in
  //other parent traits...
  class XmissingContext(name, ctxt) { //dunno if this needs more or not!
    // you tried to look up a context and, well, it was missing
    method asString { "{name} is missing in {ctxt}" }
    method isMissing { true }
    method isAmbiguous { false } 
  }

  //a pseduo-context returned when a lookup finds multiple matching definitions
  class XambiguousContext(name, ctxt, mode, possibilies) { //dunno if this needs more or not!
    // you tried to look up a context and, well, it was ambiguous
    method asString { "{mode} {name} is ambiguous in {ctxt} between {possibilities}" }
    method isMissing { true }
    method isAmbiguous { true } 
  }
}
