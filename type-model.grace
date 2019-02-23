import "object-model" as om
import "subtyping" as subtyping

def singleton is public = exports
def ng = singleton

class exports {
  print "type model exports"
  inherit om.exports
     alias oldintrinsicModuleObject = intrinsicModuleObject 

  class ngType ( value' ) { //the type of a "normal" value
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


  method intrinsicModuleObject {
    print "type model intrinsic"
    def im = oldintrinsicModuleObject
    im.declareName("Number") value(ngTypeType(subtyping.numberType))
    im.declareName("String") value(ngTypeType(subtyping.stringType))
    // print (im)
    im
  }
}
