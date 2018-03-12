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
  leftObjectType.isSubtypeOf(rightObjectType)
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

      //print "iStOwA: self {self}"
      //print "iStOwA: oType {oType}"
      //print "iStOwA: assumptions {assumptions}"

      if (oType.isUnknown || 
           (assumptions.at(self) ifAbsent {
             def against = set[[ObjectType]]
             assumptions.at(self) put(against)
             against
             }).contains(oType)) then {
        return true
      }

      assumptions.at(self).do { assume -> assume.add(oType) }

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

        print "PRONT {sMeth.returnObjectType}"
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

}


class objectType( ngInterface ) {
   inherit abstractObjectType
   
   def ctxt = ngInterface.context  

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

def unknownObjectType is public = object {
  inherit abstractObjectType
  def methods = empty
  method isUnknown { true }  
  method isStructural { false }
  method isSubtypeOf(_ : ObjectType) -> Boolean { true }
  method isSupertypeOf(_ : ObjectType) -> Boolean { true }
  method asString { "unknownObjectType" }
}


def doneType is public = object { 
  inherit abstractObjectType
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
   def returnObjectType is public = 
           signatureNode.returnType.eval(ctxt.withoutCreatio) 
   method typeParameters { signatureNode.typeParameters }
   def parametersObjectTypes is public = 
     for (signatureNode.parameters) 
     map { p -> p.typeAnnotation.eval(ctxt.withoutCreatio) }    
   method asString { "method {name}  [[{typeParameters.size}]] ({parametersObjectTypes.size})" }
}
