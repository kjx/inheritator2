//typechcker module for inheritator2
//adapted from typed.grace by Tim Jones

//TODO MUST RIP OUT ASSUMPTIONS TO separate obejct

import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "utility" as utility
use utility.exports

method check (left) isSubtypeOf (right) {
  def leftObjectType = makeObjectType(left)
  def rightObjectType = makeObjectType(right)
  //print "checking: {left} isSubtypeOf {right}"
  leftObjectType.isSubtypeOf(rightObjectType)
}

method check (left) isTypeEquals (right) {
  def leftObjectType = makeObjectType(left)
  def rightObjectType = makeObjectType(right)
  print "checking: {left} isTypeEquals {right}"
  print "objectType: {leftObjectType} isTypeEquals {rightObjectType}"
  leftObjectType == (rightObjectType)
}


method makeObjectType(obj) {
  match (obj.kind)
    case { "ngUnknown" -> unknownObjectType } 
    case { "ngImplicitUnknown" -> unknownObjectType } 
    case { _ -> objectType(obj) }
}


//from Tim
type ObjectType = interface {
    methods -> Set[[MethodType]]
    methodNamed(name : String)
      ifAbsent[[T]](onAbsent : Action[[T]]) -> MethodType | T
    isUnknown -> Boolean
    isStructural -> Boolean
    isSubtypeOf(other : ObjectType) -> Boolean
    // Used for redispatch in isSubtypeOf(), and does not actually represent a
    // calculation of whether this type is a supertype of the given one.
    isSupertypeOf(other : ObjectType) -> Boolean
    |(other : ObjectType) -> ObjectType
    &(other : ObjectType) -> ObjectType
}


class abstractObjectType {
   def methods = empty

   method methodNamed(name) ifAbsent (block)  {
        for (methods) do { meth ->
          if (meth.name == name) then {
            return meth
          }
        }
        block.apply
   }

   method isSupertypeOf(_ : ObjectType) -> Boolean {
        isStructural && methods.isEmpty
   }

   method isSubtypeOf(oType : ObjectType) -> Boolean {
        if (self == oType) then {return true}
        // Let the given type have a say.
        oType.isSupertypeOf(self).orElse {
          oType.isStructural.andAlso {
            isSubtypeOf(oType) withAssumptions(dictionary)
          }
        }
   }

   method isSubtypeOf(oType : ObjectType)
          withAssumptions(assumptions :
            MutableDictionary[[ObjectType, MutableSet[[ObjectType]] ]]) -> Boolean {

      ///whyt is this needed given James' fancy fucking code!!!
      if (oType.isStructural) then {
         if (methods.isEmpty && oType.methods.isEmpty) then {return true}}

      print "OT/iStO self: {self} other: {oType}"
      print "ass: {assumptions.size}"

      if (oType.isUnknown) then {return true}
   
      if (assumptions.at(self) ifAbsent {
             def against = set[[ObjectType]]
             assumptions.at(self) put(against)
             against
             }).contains(oType) then {
        return true
      }

      assumptions.at(self).do { assume -> assume.add(oType) }

      print "ABOUT TOCHECK METHODS"

      for (oType.methods) do { oMeth ->
        def sMeth = methodNamed(oMeth.name) ifAbsent { return false }

        if (sMeth.typeParameters.size != oMeth.typeParameters.size) 
          then {return false}

        def sParamTypes = sMeth.parametersObjectTypes
        def oParamTypes = oMeth.parametersObjectTypes

        if (sParamTypes.size != oParamTypes.size)
          then { return false }

        for (sParamTypes) and(oParamTypes) do { sParam, oParam ->
            if (!oParam.isSubtypeOf(sParam)
                withAssumptions(assumptions)) then {
              return false
            }
          }

        if (!sMeth.returnObjectType.isSubtypeOf(oMeth.returnObjectType)
                withAssumptions(assumptions)  )
          then { return false }
      }
      true
   }

   def isStructural : Boolean is public = false
   def isUnknown : Boolean is public = false

   def hashCache = cache { 
     var acc := 0 
     for (methods) do { m -> acc := (acc + m.name.hash) % (2 ^ 64) }
     acc
   }
   method hash { ^ hashCache }

   method !=(other) { !(self == other) }
   method ==(other) {
     print "type=="
     ////print "self: {self}"
     //print "self: {isUnknown} {isStructural} {hash}"
     //print "other: {other}"
     //print "other: {other.isUnknown} {other.isStructural} {other.hash}"
     if (isUnknown && other.isUnknown) then {return true}
     if (isStructural != other.isStructural) then {return false}
     if (hash != other.hash) then {return false}
     return (equalsOther(other))
   }
   method equalsOther(other) { other.equalsAbstractObjectType(self) }
   method equalsAbstractObjectType(other) { error "shouldn't happen" }   
   method equalsStructuralObjectType(other) { false }   
   method equalsSingletonObjectType(other) { false }   
}


class objectType( ngInterface ) {
   inherit abstractObjectType
   
   ////print "objectType {ngInterface}"

   method equalsOther(other) { 
     //print "ot=OTHER"
     other.equalsStructuralObjectType(self) }
   method equalsStructuralObjectType(other) {
     //print "ot.eSOT"
     //print "ctxt {ctxt.dbg}  other {other.ctxt.dbg} {ctxt == other.ctxt}" 
     //print "value {value} other {other.value}"
     //print "value {value.nodeID} other {other.value.nodeID} {(value == other.value)    }"
     def rv = (ctxt == other.ctxt) && (value == other.value)    
     //print "ot.eSOT rv = {rv}"
     rv
   }


   def ctxt is public = ngInterface.context  
   def value is public = ngInterface.value

   def methods is public =  //rename as methodTypes sometime?
     for (ngInterface.value.signatures)
     map { sig -> methodType( sig, ctxt ) }

   def isStructural : Boolean is public = true
   def isUnknown : Boolean is public = false

   method asString { 
      match (methods.size)
        case { 0 -> return "interface \{\}"}
        case { 1 -> return "interface \{ {methods.at(1)} \}" }
        case { _ -> }
      var rv := "interface \{\n  "
      for (methods) do { meth -> 
        rv := rv ++ "{meth}" ++ "\n  "
      }
      rv := rv ++ "}"
      rv
   }
}

class singletonObjectType {
  inherit abstractObjectType  
  method equalsOther(other) { other.equalsSingletonObjectType(self) }
  method equalsSingletonObjectType(other) { asString == other.asString }
}

def unknownObjectType is public = object {
  inherit singletonObjectType

  def methods = empty
  method isUnknown { true }  
  method isStructural { false }
  method isSubtypeOf(_ : ObjectType) -> Boolean { true }
  method isSubtypeOf(_ : ObjectType) withAssumptions(_)-> Boolean { true }
  method isSupertypeOf(_ : ObjectType) -> Boolean { true }
  method asString { "unknownObjectType" }
}


def doneType is public = object { 
  inherit singletonObjectType
  
  def methods = empty
  method isUnknown { true }  
  method isStructural { false }
  method isSubtypeOf(other : ObjectType) -> Boolean { // Let other have a say.
        other.isSupertypeOf(self).orElse { self == other } }
  method isSupertypeOf(other : ObjectType) -> Boolean { self == other }
  method asString { "doneObjectType" }
}










class methodType ( signatureNode, ctxt ) { 
   method name { signatureNode.name }
   //def returnObjectType is public = 
   //        makeObjectType(signatureNode.returnType.eval(ctxt.withoutCreatio))
   method returnObjectType { 
           makeObjectType(signatureNode.returnType.eval(ctxt.withoutCreatio)) }
   method typeParameters { signatureNode.typeParameters }
   method parametersObjectTypes {
     for (signatureNode.parameters) 
       map { p -> makeObjectType (p.typeAnnotation.eval(ctxt.withoutCreatio)) } }
   //def parametersObjectTypes is public = 
   // for (signatureNode.parameters) 
   //   map { p -> makeObjectType (p.typeAnnotation.eval(ctxt.withoutCreatio)) }    
   method asString { "method {name}  [[{typeParameters.size}]] ({parametersObjectTypes.size})" }

   ////print "ZNK {signatureNode.name}"
   ////print "tPars: {typeParameters}"
   ////print "types: {parametersObjectTypes}"
}
