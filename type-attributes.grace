import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "utility" as utility
use utility.exports

//reall reall really should borrow/inherit from attributes.
trait abstractAttributes {
  method attributeDef(origin) asType(typeAnnotation) properties(properties) inContext(ctxt) is abstract { }
  method attributeVar(origin) asType(typeAnnotation) properties(properties) inContext(ctxt)  is abstract { } 
  method attributeMethod(methodNode) properties(properties) inContext(ctxt) is abstract { }
  method attributeBlockMatchMethod(blockNode) inContext(ctxt) is abstract { } 
  method attributeBlockApplyMethod(blockNode) inContext(ctxt) is abstract { } 
  method attributeValue(value') inContext(ctxt) is abstract { } 
  method attributeMissing(name) inContext(ctxt)  is abstract { } 
  method attributeAmbiguous(name) between(possibilities) inContext(ctxt) is abstract { } 
  method attributeLambda(lambda) inContext(ctxt) is abstract { } 
  method attributeLambda2(lambda) inContext(ctxt) is abstract { } 
  method attributeWrapper(subject) privacy(shouldBePublic)  is abstract { }
  method Attribute is abstract { }  //abstract type :-)

  method attributeSignature(signature) properties(properties) inContext(ctxt) is abstract { }//THIS ONE IS EXTRA
}

class attributesFamily { 
  method ngUninitialised is abstract { } 
  method ngDone is abstract { }   
  method ngImplicitUnknown is abstract { }
  method progn(_) is abstract { } 
  method ngBoolean(_) is abstract { }
  method ngUnknown is abstract { } 

  type O = Unknown
  type Context = Unknown
  type Attribute = { 
      invoke(this: O) args(args: Sequence[[O]]) types(typeArgs: Sequence[[O]]) creatio(creatio) -> O
      isPublic -> Boolean
      isAbstract -> Boolean
      isOverride -> Boolean
      isMissing -> Boolean //usually false. TRUE if lookup failed!!
      asPublic(Boolean) -> Attribute
      context -> Context
  }


  class attributeDef(origin) asType(typeAnnotation) properties(properties) inContext(ctxt) {
     use utility.annotationsTrait(properties)
     use changePrivacyAnnotations
     assert {!properties.isAbstract} because "A field can't be abstract"
     var boxValue := ngUninitialised
     method initialValue:= (initialValue) {
        if (boxValue != ngUninitialised) then { error "can't initialise initailsed box" }
        check(initialValue) isType(typeAnnotation) inContext(ctxt)
        boxValue := initialValue
     }
     method invoke(this) args(args) types(typeArgs) creatio(_) {
        assert {args.size == 0}
        assert {typeArgs.size == 0}
        //value
        typeAnnotation.eval(ctxt.withoutCreatio)
     }
     method value {
        typeAnnotation.eval(ctxt.withoutCreatio)
        }

     method asString {"attributeDef: {origin} = {boxValue}"}
     method context { ctxt } 
  }

  class attributeVar(origin) asType(typeAnnotation) properties(properties) inContext(ctxt)  {
     inherit attributeDef(origin) asType(typeAnnotation) properties(properties.getter) inContext(ctxt)
       alias varInitialValue:=(_) = initialValue:=(_)

     method initialValue:=(initialValue) {
       if (initialValue != ngUninitialised) 
          then { varInitialValue:=(initialValue) }
     }

     def setter is public = object {
       use utility.annotationsTrait(properties.setter)
       use changePrivacyAnnotations
       method invoke(this) args(args) types(typeArgs) creatio(creatio) {
          assert {args.size == 1}
          assert {typeArgs.size == 0}
          def newValue = args.at(1)
          check(newValue) isType(typeAnnotation) inContext(ctxt)
          boxValue:= newValue
          ngDone
         }
       method context { ctxt } 
       method asString {"attributeVar (setter): {origin} := {boxValue}"}
     }
     method asString {"attributeVar (getter): {origin} := {boxValue}"}
     method context { ctxt } 
  }

  //an attribute method..
  class attributeMethod(methodNode) properties(properties) inContext(ctxt) {
     use utility.annotationsTrait(properties)
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       //print "invoke {methodNode.signature.name} on #{this.dbg} creatio:{creatio.isCreatio}"

       //should this not have a creation??
       def typetxt = ctxt.subcontextNamed(methodNode.signature.name ++ "(types)")

       def typeParams = methodNode.signature.typeParameters.asList

       //print "type-invoke method {methodNode.signature.name}"
       //print "typeParams: {typeParams}"
       //print "typeArgs: {typeArgs}"

       if (typeArgs.size == typeParams.size)
          then {
            for (typeParams.indices) do { i ->
              typetxt.declareName(typeParams.at(i).name) value(typeArgs.at(i)) } }
          elseif {typeArgs.size == 0}
          then {
            for (typeParams.indices) do { i ->
              typetxt.declareName(typeParams.at(i).name) value(ngImplicitUnknown) } }
          else {error "generic arg mismatch"}


       def returnType = methodNode.signature.returnType

       def subtxt = typetxt.subcontextNamed(methodNode.signature.name)

       def params = methodNode.signature.parameters.asList

       //print "params: {params.size}\nargs: {args.size}"

       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i ->
           def par = params.at(i)
           def arg = args.at(i)
           def pta = par.typeAnnotation
           check(arg) isType(pta) inContext(typetxt)  //not sure this makes sense left in
           subtxt.declareName(par.name) value(pta.eval(typetxt))
       }

       subtxt.addLocal(CREATIO) value(creatio) 
       subtxt.addLocal(RETURNBLOCK) //invoke return block to check type of "return"
          slot (attributeLambda {rv, _ ->
                  check(rv) isType(returnType) inContext(typetxt)
                  return returnType.eval(subtxt)} inContext(subtxt))
       subtxt.addLocal(RETURNCREATIO) value (creatio) 

       //don't run the method: just return the return type
       //return type may mention type params (and apparently params)
       returnType.eval(subtxt)
     }
     method context { ctxt } 
     method asString {"attributeMethod: {methodNode.signature.name} #{ctxt.dbg}"}
  }




  //types only. make a fake method from a signa
  class attributeSignature(signature) properties(properties) inContext(ctxt) {
     use utility.annotationsTrait(properties)
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       //print "invoke {signature.name} on #{this.dbg} creatio:{creatio.isCreatio}"

       //should this not have a creation??
       def typetxt = ctxt.subcontextNamed(signature.name ++ "(types)")

       def typeParams = signature.typeParameters.asList

       if (typeArgs.size == typeParams.size)
          then {
            for (typeParams.indices) do { i ->
              typetxt.declareName(typeParams.at(i).name) value(typeArgs.at(i)) } }
          elseif {typeArgs.size == 0}
          then {
            for (typeParams.indices) do { i ->
              typetxt.declareName(typeParams.at(i).name) value(ngImplicitUnknown) } }
          else {error "generic arg mismatch"}

       def returnType = signature.returnType

       returnType.eval(typetxt)
     }
     method context { ctxt } 
     method asString {"attributeSignature: {signature.name} #{ctxt.dbg}"}
  }







  class attributeBlockMatchMethod(blockNode) inContext(ctxt) {
     use utility.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       error "shouldn't happen in typechecker(aBMM)"
       def params = blockNode.parameters.asList
       def prognBody = progn(blockNode.body)
       if (args.size != params.size) then {error "arg mismatch"}
       return (ctxt.lookupLexical "booleanLiteral").value
       }
     method context { ctxt } 
     method asString {"attributeBlockMethod"}
  }

  class attributeBlockApplyMethod(blockNode) inContext(ctxt) {
     use utility.publicAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       error "shouldn't happen in typechecker(aBAM)"
       def params = blockNode.parameters.asList
       def prognBody = progn(blockNode.body)
       def subtxt = ctxt.subcontext
       if (args.size != params.size) then {error "arg mismatch"}
       for (params.indices) do { i ->
           def par = params.at(i)
           def arg = args.at(i)
           check(arg) isType(par.typeAnnotation) inContext(ctxt)
           subtxt.declareName(par.name) value(arg)
       }
       subtxt.addLocal(CREATIO) value(creatio) 
       prognBody.build(subtxt)
       prognBody.eval(subtxt) //GOD KNOWS WHAT TO DO HERE!!!
      }
     method context { ctxt } 
     method asString {"attributeBlockMethod"}
  }



  //a ng value bound to a name in a context. already initialised! 
  class attributeValue(value') inContext(ctxt) {
     use utility.confidentialAnnotations
     use changePrivacyAnnotations
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {
       assert {(args.size == 0) && (typeArgs.size == 0) && (!creatio.isCreatio)}
       value
     }
     method context { ctxt } 
     method value { value' }
     method asString {"attributeValue: {value}"}
  } 
  //potentially every obejct could be attribute, so we don't need this.
  //too confusing to put in now.

  //what lookup retuns when it doesn't find anything.
  class attributeMissing(name) inContext(ctxt)  {
     //print "MISSING {name} {ctxt}"
     use utility.publicAnnotations
     use changePrivacyAnnotations
     method isMissing { true }
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is missing from {ctxt}"
     }
     method context { ctxt } 
     method asString {"attributeMissing: {name} at {ctxt}"}
  }

  //what lookup retuns when it's abiguous
  class attributeAmbiguous(name) between(possibilities) inContext(ctxt) {
     inherit attributeMissing(name) inContext(ctxt) 
     method invoke(this) args(args) types(typeArgs) creatio(creatio) {  
        error "{name} is ambiguous at {ctxt} between {possibilities}"
     }
     method context { ctxt } 
     method asString {"inocableAmbiguous {name} in {ctxt}"}
  }

  //old style attribute that wraps a lambda block; 
  //blocks takes arguments plus creatio. BUT NO CONTEXT
  //use for primitives but otherwise avoid
  class attributeLambda(lambda) inContext(ctxt) {
    use utility.publicAnnotations
    use changePrivacyAnnotations
    method invoke(this) args(args) types(typeArgs) creatio(creatio) {
      error "shouldn't happen in typechecker(aL)"
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
    method asString { "attributeLambda {lambda}" }
  }


  //new style attribute that wraps a lambda block; 
  //blocks takes arguments plus creatio and context. 
  //should use this one rather than the other one
  class attributeLambda2(lambda) inContext(ctxt) {
    use utility.publicAnnotations
    use changePrivacyAnnotations
    method invoke(this) args(args) types(typeArgs) creatio(creatio) {
      error "shouldn't happen in typechecker(aL2)"
      applyVarargs2(lambda,args,creatio)
    }
    //apply the block to the LIST of arguments from the interpreter
    //args are already evaluated
    method applyVarargs2(block,args,creatio) {
      def a = args.asList
      match (args.size)
        case { 0 -> block.apply(creatio,ctxt)}
        case { 1 -> block.apply(a.at(1),creatio,ctxt)}
        case { 2 -> block.apply(a.at(1),a.at(2),creatio,ctxt)}
        case { 3 -> block.apply(a.at(1),a.at(2),a.at(3),creatio,ctxt)}
        case { 4 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),creatio,ctxt)}
        case { 5 -> block.apply(a.at(1),a.at(2),a.at(3),a.at(4),a.at(5),creatio,ctxt)}
        case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }
    }
    method context { ctxt } 
    method asString { "attributeLambda {lambda}" }
  }


  //dynamic type check.
  //Takes a typeExpression and a context, not a type object
  //  because it's mostly used to check against declared types
  method test(obj) isType(typeExpression) inContext(ctxt) {
       def typeObject = typeExpression.eval(ctxt.withoutCreatio)
       //if (!typeObject.match(obj))
       //    then { error "type check failed: {obj} isnt {typeObject} from {typeExpression}" }

       typeObject.staticTypeCheck(obj)
       
       //def argCtxt = ctxt.withoutCreatio
       //def creatio = argCtxt.creatio 
       //def matchAttribute = typeObject.lookupExternal("match(_)")
       //def matchResult = matchAttribute.invoke(ctxt) args(list(obj)) types(empty) creatio(creatio)
       //return matchResult.value
  }

  method check(obj) isType(typeExpression) inContext(ctxt) {
       if (!test(obj) isType(typeExpression) inContext(ctxt))
           then { 
               def typeObject = typeExpression.eval(ctxt.withoutCreatio)
               print "type check failed: {obj} isnt {typeObject} from {typeExpression}" }
       obj
  }


  //behaviour to change privacy annotations 
  trait changePrivacyAnnotations {
    method isPublic is abstract { } 
    method asPublic(shouldBePublic : Boolean) { 
      if (isPublic == shouldBePublic) 
         then {self}
         else {attributeWrapper(self) privacy(shouldBePublic)} 
    }
  }

  //proxy to change an attribute's privacy
  class attributeWrapper(subject) privacy(shouldBePublic)  {
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
    method asString {"attributeWrapper {subject}"}
  }
  method context { ctxt } 
}
