import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "attributes" as attributes
import "primitives" as primitives
import "utility" as utility
use utility.exports

def singleton is public = exports
def ng = singleton

method debugPrint(string) {}


class earlyDefinitions { 
  type NGO = Unknown

  //for debugging
  var contextCounter is public := 0
}

class exports {
  inherit earlyDefinitions
  inherit primitives.primitivesFamily
    //exclude context
    //exclude lexicalContext(_)
    //exclude attributeBlockMethod(_) inContext(_)
  inherit attributes.attributesFamily
    exclude ngUninitialised
    exclude ngDone
    exclude ngImplicitUnknown

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
       def bodyContext = ctxt.withoutCreatio
       var rv := ngDone
       for (body) doWithLast {
          stmt, last -> rv := stmt.build( if (!last) then {bodyContext} else {ctxt} ) }
       rv
     }
     method eval(ctxt) { 
       def bodyContext = ctxt.withoutCreatio
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

    def locals :  Dictionary[[String,Attribute]] 
        = dictionary[[String,Attribute]]

    ////////////////////////////////////////////////////////////
    //// local declarations 

    method declareName(name) attribute ( attribute ) { 
      if ( hasLocal(name) )
        then { error "trying to declare {name} more than once" }
        elseif { checkForShadowing(name) }
        then { error "{name} shadows lexical definition" }
        else { addLocal(name) slot(attribute) }
    }

    method checkForShadowing(name) is confidential { 
       //!findInternalDeclaringContext(name).isMissing //forbid all shadowing
       false  //don't forbit any shadowing
    } 

    //declare a Def which must later be initialised
    method declareDef(name) asType(typeAnnotation) properties(properties) {
      def box = attributeDef(name) asType(typeAnnotation) properties(properties) inContext(self)
      declareName(name) attribute(box) 
    }

    //declare a Var which must later be initialised
    method declareVar(name) asType(typeAnnotation) properties(properties) {
      def setterName = name ++ ASSIGNMENT_TAIL
      def box = attributeVar(name) asType(typeAnnotation) properties(properties) inContext(self)
      declareName(name) attribute(box) 
      declareName(setterName) attribute(box.setter) 
    }

    //bind a value to a name - for things like arguments, that 
    //the interpreter already has to hand, that DON'T need to be initialised
    method declareName(name) value(value) {
        declareName(name) attribute(attributeValue(value) inContext(self))
    }

    //bind an host-interpreter lambda to a name - typically for primitives
    method declareName(name) lambda(lambda) {
        declareName(name) attribute(attributeLambda(lambda) inContext(self))
    }

    ////////////////////////////////////////////////////////////
    //// setting and accessing local declarations
    //// 
    //// add does not do shadowing or multiple declaration checks
    //// has - test if present
    //// get - error if not present
    //// lookup - return a missing sentinel if not presentx
    ////
    method addLocal(name) slot(m) { locals.at(name) put(m) }
    method addLocal(name) value(m) { locals.at(name) put(attributeValue(m) inContext(self)) }
    method hasLocal(name) { locals.containsKey(name) }
    method getLocal(name) { lookupLocal(name) ifAbsent { error "local {name} missing in #{dbg}"} }
    method lookupLocal(name) ifAbsent(block) { locals.at(name) ifAbsent(block) }
    method lookupLocal(name) {
       lookupLocal(name) ifAbsent { attributeMissing(name) inContext(self) } }

    ////////////////////////////////////////////////////////////
    //// lookups - external interface
    ////
    //// lookupInternal - called by treewalker for internal requetss
    //// lookupExternal - called by treewalker for external requests
    
    method getInternal(name){
      lookupInternal(name) ifAbsent { error "{name} is missing from {self}" } }

    method lookupInternal(name) ifAbsent(block) { 
      def rv = lookupInternal(name)
      match (rv) 
        case { _ : interface { isMissing } -> if (rv.isMissing) then {block.apply} }
        case { _ -> rv }
      rv
      }
    method lookupInternal(name) {
      def attribute = lookupDeclaration(name)
      if (attribute.context.isWhole)   //just an optimisatiton?
          then {return attribute}
      def whole = attribute.context.whole
      whole.lookupInheritance(name) 
      }


    method lookupDeclaration(name){ lookupLocal(name) }
    
    method lookupLexical(name){ lookupLocal(name) }    

    //external lookups consider only inheritance
    method lookupExternal(name) {lookupInheritance(name)}
    method lookupInheritance(name) {lookupLocal(name)} 

    //where there are parental part objects, this is the whole object
    //to which they belong
    method whole {self} 
    method isWhole {true} 
    method isPart {false}

    //create a subcontext nested inside this context
    method isInside(other) {self == other}
    method subcontext {lexicalContext(self)}
    method subcontextNamed(name) {lexicalContext(self)named(name)}


    //manage creatio shit
    method creatio { 
      def crt = lookupLexical(CREATIO)
      if (crt.isMissing) then { ng.ngNotCreatio } else { crt.value }
    }

    method withoutCreatio {
         if (creatio.isCreatio) 
           then {
             def noCreatioCtxt = subcontext
             noCreatioCtxt.addLocal(CREATIO) value(ng.ngNotCreatio)
             noCreatioCtxt } 
           else { self } 
         }



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
  class lexicalContext(ctxt) { inherit lexicalContext(ctxt)named("") }
  class lexicalContext(ctxt) named(ctxtName) { 
    inherit context
        
    method kind {"lexicalContext"}
    method asString {
           "lexicalContext:{ctxtName}#{dbg} {locals.keys}\n!!{ctxt.asString}" }

    //am I inside some other context?
    method isInside(other) {(self == other) || ctxt.isInside(other)}

    //lookupDeclaration     
    method lookupDeclaration(name) {
        lookupLocal(name)ifAbsent {lookupEnclosingDeclaration(name)} }
    method lookupLexical(name) {
        lookupLocal(name)ifAbsent {lookupEnclosingLexical(name)} }

    //hook methods to control recursion through outer scopes
    method lookupEnclosingDeclaration(name) { ctxt.lookupDeclaration(name) }
    method lookupEnclosingLexical(name) { ctxt.lookupLexical(name) }
    }
  


  /////////////////////////////////////////////////////////////
  ////
  //// OBJECTS!!!
  ////
  /////////////////////////////////////////////////////////////
  //
  // this class represents objecs in the underlying interpreter
  // it's called "objectContext"  because "object" is already taken by Grace
  // an object is a lexical context that also supports inheritance.
  // to resolve inheritance correctly we have to keep the contexts of
  // each contributing object constructor - these are "parental part objects" 
  // that represent an inherited part of another "whole" object
  // only objectContexts that represent "whole" objects 
  // should ever be accessible to the interpreted programs.
  //
  class objectContext(body,ctxt) {
    inherit lexicalContext(ctxt)
    method kind{"objectContext"}

    var status is readable := "embryo"

    def isPart is public = creatio.isCreatio
    def isWhole is public = (!isPart)
    def whole is public = (if (isWhole) then {self} else {creatio})

    //bind "self" and "outer"
    def mySelf = lookupLexical("self")
    if (!mySelf.isMissing) then { addLocal "outer" slot(mySelf.value) }
    addLocal "self"  value(whole) 

    //list of AST nodes for inherit and request clauses
    //note these are AST nodes, not parental part objects
    def inheritParentNodes :  Sequence[[Node]] = list 
    def useParentNodes :  Sequence[[Node]] = list 

    //setup the context for the parental requests
    //by binding the creatio dynamic argument
    var parentRequestContext := ctxt 
    if (isWhole) then { 
         parentRequestContext := ctxt.subcontext
         parentRequestContext.addLocal(CREATIO) value(self)  
    }

    //build the individual declarations into this object    
    //by going through the body of the object constructor in the AST
    //this is will set up declarations by double-dispatch requests back 
    //to "declareName (var, def, invokaeanle, etc)"   for vars and methods
    //and "addParent" for inheritance and use
    //build() doesn't do anyting for inline code or initialisation
    for (body) do { e -> e.build(self) } 

    status := "built"

    //if I'm NOT Bottommost then I must be a parental part object
    //my structure is now completly built() so I stop here
    //I cannot be initialised until the entire "whole" object is built
    if (isPart) then { 
       status := "part" 
       return self 
    }

    initialize

    status := "cooked" //i.e. OK to go! 

    ////////////////////////////////////////////////////////////
    // "constructor" code ends here
    ////////////////////////////////////////////////////////////

    // initilise by callling eval.
    // this needs to be a method (unlike build()) which is inline
    // beceause parent parts can only be initialised once the whole
    // object of which they are a part has been built
    method initialize {   
      for (body) do { e ->
        e.eval(self)
      }
    }

    //add a parent represented by a parentNode from the Common AST
    //an inheritNode double-dispaches back her in the build() phase
    method addParent(parentNode) { 
      //and the AST node to the appropriate list
      match (parentNode.kind) 
        case { "inherit" -> inheritParentNodes.add(parentNode) }
        case { "use" -> useParentNodes.add(parentNode) }
        case { _ -> error "NOT COBOL!" }

      // make the parental request to create the parental part object
      def parentalPartObject = parentNode.request.eval(parentRequestContext) 
      assert {parentalPartObject.status == "part"}

      if (parentalPartObject.whole != whole) then {
         print "PARENTALPARTOBJECT"
         print (parentalPartObject)
         print "WHOLE"
         print (whole) }

      assert {parentalPartObject.whole == whole}

      // store the parental part object as a
      // pseudo-field in the inheriting objectContext
      // based on a unique ID string name of the inherit node
      // this is mainly used so that when an object is initialised
      // it can find the alreadt-built part-object 
      addLocal(parentNode.parentID) slot(parentalPartObject)
    }

    //////////////////////////////////////////////////
    //lookup methods

    // lookup a declaration considering inheritance 
    // but NOT considering nesting
    // used for external requets (including "self" and "outer" requests)
    // because this is the definition for lookupExternal
    method lookupInheritance(name) {
      def localDefn = lookupLocal(name)  
      def useCandidates = findCandidates(name) parents(useParentNodes)
      def inheritCandidates = findCandidates(name) parents(inheritParentNodes)

      if (!localDefn.isMissing) then {
         if (localDefn.isOverride &&
               ((useCandidates.size + inheritCandidates.size) == 0))
           then { error "{name} in {self} isOverride but doesn't override anything" }
           else { return localDefn } }

      if (useCandidates.size == 1) then {return useCandidates.at(1) }
      if (useCandidates.size > 1) then {return attributeAmbiguous(name) between(useCandidates) inContext(self) }
      assert {useCandidates.size == 0}

      if (inheritCandidates.size == 1) then {return inheritCandidates.at(1) }
      if (inheritCandidates.size > 1) then {return attributeAmbiguous(name) inContext(self) between(inheritCandidates)}
      assert {inheritCandidates.size == 0}

      attributeMissing(name) inContext(self)
    }
    

    // auxiliary method to search a single parent 
    // handling excludes and alias subclases
    method findCandidates(name)parents(parents) {
      def candidates = list
      for (parents) do { 
        parentNode -> processCandidate(name,parentNode,candidates) }
      candidates
      }

    method processCandidate(name,parentNode,candidates) {
      var parentName := name
      if (parentNode.aliases.containsKey(name)) 
        then { parentName := parentNode.aliases.at(name) }
        elseif { parentNode.excludes.contains(name) }
        then { return 0 }

      def parentPartObject = getLocal(parentNode.parentID) 
      def parentDefn = parentPartObject.lookupInheritance(parentName)
      //line below is the WRONG THING I think.
      //multiple abstract-> abstract (but not ambiguous) etc
      if ((!parentDefn.isMissing) && (!parentDefn.isAbstract)) 
              then {
                //def cand = (
                //   if (parentNode.aliases.containsKey(name))
                //     then {parentDefn.asPublic(false)}
                //     else {parentDefn})
                //candidates.add(cand)
                candidates.add(parentDefn)
                }
    }
      
    method isMissing(thingy) { 
      thingy.isMissing
      //match (thingy) 
      //  case { _ : interface { isMissing } -> thingy.isMissing } 
      // case { _ -> thingy}
      }


    // lookup a declaration considering both inheritance and nesting
    // used for internal requets 
    // because this is the ultimate definition for lookupInternal
    method lookupDeclaration(name) {
       if (hasLocal(name)) then {return getLocal(name)}

       def inheritanceResult = lookupInheritance(name)
       def lexicalResult = lookupEnclosingDeclaration(name)

       if (isMissing(lexicalResult) && isMissing(inheritanceResult))
         then {attributeMissing(name) inContext(self)}
         elseif {isMissing(inheritanceResult)}
         then {lexicalResult}
         elseif {isMissing(lexicalResult) || (inheritanceResult == lexicalResult)}
         then {inheritanceResult}
         else {attributeAmbiguous(name) inContext(self)}
    }

    

    method asString { "{kind}#{dbg}:({status}) {locals.keys}\n!!{ctxt.asString}" }
  }

  // represents a module 
  // same as a normal contextObject, except only looks out one more scope
  // (to the enclosing context, which should be the dialect,
  // rather than continuing transitively
  class moduleObject(body,ctxt) {
    inherit objectContext(body,ctxt)
    method kind {"moduleObject"}
    method lookupEnclosingDeclaration(name) { ctxt.lookupInheritance(name) }
    method lookupEnclosingLexical(name) { ctxt.lookupLocal(name) }
    method asString {
           "moduleObject#{dbg} {locals.keys}\n!!{ctxt.asString}" }
    method isLoaded { true }
  }


  //module containing all our intrinsic / builtin names
  method intrinsicModuleObject {

    //intrinsic module's "pseudo-dialect"
    //mostly here for testing, this sholdn't be reached from a normal module
    def intrinsicDialect = context
    intrinsicDialect.declareName("trump")
        lambda { creatio -> error "Make GRACE great AGAIN" }

    //the intrinsic module context
    def im = moduleObject(empty, intrinsicDialect)

    im.declareName("implicitUninitialised") value(ng.ngUninitialised)
    im.declareName("implicitUnknown") value(ng.ngImplicitUnknown)
    im.declareName("implicitDone") value(ng.ngImplicitDone)
    im.declareName("Unknown") value(ng.ngUnknown)

    //privacy annotations
    im.declareName("confidential") value(ng.ngBuiltinAnnotation("confidential"))
    im.declareName("public") value(ng.ngBuiltinAnnotation("public"))
    im.declareName("readable") value(ng.ngBuiltinAnnotation("readable"))
    im.declareName("writable") value(ng.ngBuiltinAnnotation("writable"))

    //inheritance annotations
    im.declareName("abstract") value(ng.ngBuiltinAnnotation("abstract"))
    im.declareName("override") value(ng.ngBuiltinAnnotation("override"))

    //basic methods
    im.declareName("print(_)") lambda { p, creatio -> print(p) }

    im.declareName("assert(_)isSubtypeOf(_)") 
          lambda { l, r, _ -> 
            if (!l.isSubtypeOf(r)) then {print "fail: {l} isSubtypeOf {r}"}
                else {print "pass: {l} isSubtypeOf {r}"}
            ngDone}

    im.declareName("assert(_)notSubtypeOf(_)") 
          lambda { l, r, _ -> 
            if (l.isSubtypeOf(r)) then {print "fail: {l} notSubtypeOf {r}"}
                else {print "pass: {l} notSubtypeOf {r}"}
            ngDone}

    im.declareName("assert(_)isEqualsType(_)") 
          lambda { l, r, _ -> 
            if (l.isTypeEquals(r)) then {print "pass: {l} isEqualsTo {r}"}
                else {print "fail: {l} NOT equals {r}"}
            ngDone}

    im.declareName("assert(_)notEqualsType(_)") 
          lambda { l, r, _ -> 
            if (l.isTypeEquals(r)) then {print "fail: {l} DOES equals {r}"}
                else {print "pass: {l} notEqualsTo {r}"}
            ngDone}

    return im
  }

}
