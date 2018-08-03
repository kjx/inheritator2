import "object-model" as om
import "subtyping" as subtyping

def singleton is public = exports
def ng = singleton

class exports {
  print "type model exports"
  inherit om.exports
     alias oldintrinsicModuleObject = intrinsicModuleObject 

  class ngType ( value' ) {
     inherit ngPrimitive

     method kind {"ngType"}
     method value {value'}
     method asString { "ngType: {value}"}

     declareName "+(_)" lambda { other, creatio ->  
                    def rv = ngNumber(value' + other.value)
                    rv } 
  }

  
  
  method intrinsicModuleObject {
    print "type model intrinsic"
    def im = oldintrinsicModuleObject
    im.declareName("Number") value(ngType(subtyping.numberType))
    im.declareName("String") value(ngType(subtyping.stringType))
    // print (im)
    im
  }
}
