import "combinator-collections" as c
use c.abbreviations
import "jerrors" as errors
use errors.exports

class invocablesFamily { 
  method ngUninitialised is abstract { } 
  method ngDone is abstract { }   
  method progn(_) is abstract { } 

  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////// Invocables
  //////// Invocables
  //////// Invocables
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////

  type O = Unknown
  type Context = Unknown
  type Invocable = { 
      invoke(this: O) args(args: Sequence[[O]]) types(typeArgs: Sequence[[O]]) creatio(creatio) -> O
      isPublic -> Boolean
      isAbstract -> Boolean
      isOverride -> Boolean
      isMissing -> Boolean //usually false. TRUE if lookup failed!!
      asPublic(Boolean) -> Invocable
      context -> Context
  }


  class invocableDef(origin) properties(properties) inContext(ctxt) {
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
     method asString {"invocableDef: {origin} = {boxValue}"}
     method context { ctxt } 
  }

  class invocableVar(origin) properties(properties) inContext(ctxt)  {
     inherit invocableDef(origin) properties(properties.getter) inContext(ctxt)
       alias defInvoke(_)args(_)types(_)creatio(_) = invoke(_)args(_)types(_)creatio(_)

     def setter is public = object {
       use common.annotationsTrait(properties.setter)
       use changePrivacyAnnotations
       method invoke(this) args(args) types(typeArgs) creatio(creatio) {
          assert {args.size == 1}
          assert {typeArgs.size == 0}
          boxValue:= args.at(1)
          ngDone
         }
       method context { ctxt } 
       method asString {"invocableVar (setter): {origin} := {boxValue}"}
     }
     method asString {"invocableVar (getter): {origin} := {boxValue}"}
     method context { ctxt } 
  }

  //an invocable method..
  class invocableMethod(methodNode) properties(properties) inContext(ctxt) {
     use common.annotationsTrait(properties)
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       //print "invoke {methodNode.signature.name} on #{this.dbg} creatio:{creatio.isCreatio}"
       def params = methodNode.signature.parameters.asList
       def prognBody = progn(methodNode.body)

       def subtxt = ctxt.subcontextNamed(methodNode.signature.name)
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i -> subtxt.declareName(params.at(i).name) value(args.at(i)) }

       subtxt.addLocal(CREATIO) value(creatio) 
       subtxt.addLocal(RETURNBLOCK) 
                 slot (invocableLambda {rv, _ -> return rv} inContext(subtxt))
       subtxt.addLocal(RETURNCREATIO) value (creatio) 

       prognBody.build(subtxt)
       prognBody.eval(subtxt)
     }
     method context { ctxt } 
     method asString {"invocableMethod: {methodNode.signature.name} #{ctxt.dbg}"}
  }

  class invocableBlockMethod(blockNode) inContext(ctxt) {
     use common.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       def params = blockNode.parameters.asList
       def prognBody = progn(blockNode.body)
       def subtxt = ctxt.subcontext
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i -> subtxt.declareName(params.at(i).name) value(args.at(i)) }
       subtxt.addLocal(CREATIO) value(creatio) 
       prognBody.build(subtxt)
       prognBody.eval(subtxt)
      }
     method context { ctxt } 
     method asString {"invocableBlockMethod"}
  }


  //a ng value bound to a name in a context. already initialised! 
  class invocableValue(value') inContext(ctxt) {
     use common.confidentialAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       assert {(args.size == 0) && (typeArgs.size == 0) && (!creatio.isCreatio)}
       value
     }
     method context { ctxt } 
     method value { value' }
     method asString {"invocableValue: {value}"}
  } 
  //potentially every obejct could be invocable, so we don't need this.
  //too confusing to put in now.

  //what lookup retuns when it doesn't find anything.
  class invocableMissing(name) inContext(ctxt)  {
     //print "MISSING {name} {ctxt}"
     use common.publicAnnotations
     use changePrivacyAnnotations
     method isMissing { true }
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is missing from {ctxt}"
     }
     method context { ctxt } 
     method asString {"invocableMissing: {name} at {ctxt}"}
  }

  //what lookup retuns when it doesn't find anything.
  class invocableAmbiguous(name) between(possibilities) inContext(ctxt) {
     inherit invocableMissing(name) inContext(ctxt) 
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is ambiguous at {ctxt} between {possibilities}"
     }
     method context { ctxt } 
     method asString {"inocableAmbiguous {name} in {ctxt}"}
  }

  //old style invocable that wraps a lambda block; 
  //blocks takes arguments plus creatio. 
  //use for primitives but otherwise avoid
  class invocableLambda(lambda) inContext(ctxt) {
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
    method context { ctxt } 
    method asString { "invocableLambda {lambda}" }
  }

  //behaviour to change privacy annotations 
  trait changePrivacyAnnotations {
    method isPublic is abstract { } 
    method asPublic(shouldBePublic : Boolean) { 
      if (isPublic == shouldBePublic) 
         then {self}
         else {invocableWrapper(self) privacy(shouldBePublic)} 
    }
  }

  //proxy to change an invocable's privacy
  class invocableWrapper(subject) privacy(shouldBePublic)  {
    assert {subject.isPublic != shouldBePublic} //or else shouldn't be here
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
    method context { subject.context } 
    method asString {"invocableWrapper {subject}"}
  }
  method context { ctxt } 
}
