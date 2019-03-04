import "object-model" as om
import "subtyping" as subtyping

def singleton is public = exports
def ng = singleton

class exports {
  print "type model exports"
  inherit om.exports
     alias oldintrinsicModuleObject = intrinsicModuleObject 

  class ngType ( value' ) { //the type of a "normal" value
                            //but the value argument here is
                            //a type from the subtyping module
                            //really shouldn't be called value!!!
     inherit ngPrimitive

     method kind {"ngType"}
     method value {value'}
     method asString { "ngType: {value}"}

     declareName "+(_)" lambda { other, creatio ->
                    def rv = self
                    rv } 
  }

  class ngTypeType ( value' ) { //the type of a type, mostly an interface
     //Or should this actually override ngInterface?  Does it havce a ctxt?
     inherit ngType( value' )

     method kind {"ngTypeType"}
     method asString { "ngTypeType: {value}"}

     //DUNNO what should be declared here if anything?
     declareName "match(_)" lambda2 { other, creatio, ctxt ->
                    ngBoolean(staticTypeCheck(other))
                     } 


     method staticTypeCheck( other ) {
            //print "STCself  {self} {value}"
            //print "STCother {other} {other.value}"
            (other.value).isSubtypeOf(value)
     }
  }


  def ngImplicitUnknown is public = ngTypeType(subtyping.unknownObjectType)
  def ngUnknown is public = ngTypeType(subtyping.unknownObjectType)


  method intrinsicModuleObject {
    print "type model intrinsic"
    //TODO just replace the old one with everything lifted to types?

    def im = oldintrinsicModuleObject
    im.declareName("Number") value(ngTypeType(subtyping.numberType))
    im.declareName("String") value(ngTypeType(subtyping.stringType))
    im.declareName("Boolean") value(ngTypeType(subtyping.stringType))

    //this is EVIL. there must be a better option
    im.removeLocal("implicitUnknown") 
    im.removeLocal("Unknown")

    im.declareName("implicitUnknown") value(ngTypeType(subtyping.unknownObjectType))
    im.declareName("Unknown") value(ngTypeType(subtyping.unknownObjectType))


    // print (im)
    im
  }
}
