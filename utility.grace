//common definitions
//ya'll know how this'll go down.

import "errors" as errors
use errors.exports

class exports {

  method safeFuckingMap(f)over(col) {
     def rv = list
     for (col) do { each -> rv.add(f.apply(each)) }
     rv
  }

  method map(f) over(col) { safeFuckingMap(f)over(col) }

  method for(c) map(f) { map(f) over(c) }

  //should probably switch to keysAndValuesDo, last is key == size
  method for(col) doWithLast(block2) {
     def size = col.size
     var counter := 1
     for (col) do { e -> 
       block2.apply(e, counter == size)
       counter := counter + 1 
     }
  }

  method for(a) and(b) do(f) -> Done { //thanks tim!
    var i := 1
    while { (i <= a.size) && (i <= b.size) } do {
      f.apply(a.at(i), b.at(i))
      i := i + 1
    }
  }

  method sizeOfVariadicList( l ) { 
    var s := 0
    for (l) do { _ -> s := s + 1 } 
    return s
  }

  method valueOf( block ) { block.apply }

  method done { } 

  class cache (block) {
    method asString { "a cache {alreadyAssigned} {block}" }
    var alreadyAssigned := false
    var value 
    method prefix ^ { 
      if (!alreadyAssigned) then {
         value := block.apply
         alreadyAssigned := true
      }
      value
    }
  }

  def CREATIO = "_creatio"
  def RETURNBLOCK = "_returnBlock"
  def RETURNCREATIO = "_returnCreatio"
  def RETURNTYPE = "_returnType"
  def ASSIGNMENT_TAIL = "():=(_)"  //":=(_)" wouldn't support varargs 
  def PARENT = "_parent"
  def INTRINSICMODULE = "inheritator2/intrinsics"
  def STANDARDDIALECT = INTRINSICMODULE



  //probably need a better name for these
  method default (initialValue) { default(initialValue) name ""}

  class default(initialValue) named (name) {
     var value is readable := initialValue
     var alreadyAssigned := false
     method prefix ^ { value }
     method <-(newValue) {
        if (alreadyAssigned) then { error "{name} already assigned" }
        value := newValue
        alreadyAssigned := true
     }
  }

} //end exports




trait annotationsTrait(properties) { 
     method isPublic { properties.isPublic }
     method isOverride { properties.isOverride }
     method isAbstract { properties.isAbstract }
     method isMissing { false }
     method isAmbiguous { false }
}

trait confidentialAnnotations {
     method isPublic { false }
     method isOverride { false }
     method isAbstract { false }
     method isMissing { false }
     method isAmbiguous { false }
}


trait publicAnnotations {
     method isPublic { true }
     method isOverride { false }
     method isAbstract { false }
     method isMissing { false }
     method isAmbiguous { false }
}

method processAnnotations(annots,publicByDefault) { 
   def encapsulation = default(publicByDefault) named "encapsulation" 
   def over = default(false) named "override"
   def abst = default(false) named "abstract"
   for (annots) do { ann ->
      match (ann.description) 
         case { "confidential" -> encapsulation <- false } 
         case { "public" -> encapsulation <- true } 
         case { "override" -> over <- true } 
         case { "abstract" -> abst <- true } 
         case { d -> warning "unknown method annotation {d}" }
   }
   object { 
     def isPublic is public = ^ encapsulation
     def isOverride is public = ^ over
     def isAbstract is public = ^ abst
   }
}

method processVarAnnotations(annots) { //COPY and PASTE
   def read = default(false) named "readable" 
   def writ = default(false) named "writable" 
   def over = default(false) named "override"   
   def abst = default(false) named "abstract"

   for (annots) do { ann ->
      match (ann.description) 
         case { "confidential" -> 
              read <- false 
              writ <- false }
         case { "public" ->  
              read <- true 
              writ <- true }
         case { "readable" -> read <- true } 
         case { "writable" -> writ <- true } 
         case { "override" -> over <- true } 
         case { "abstract" -> abst <- true } 
         case { d -> warning "unknown method annotation {d}" }
   }
   return object { 
     def getter is public = object { 
       def isOverride is public = ^ over
       def isAbstract is public = ^ abst
       def isPublic is public = ^ read
     }
     def setter is public = object { 
       def isOverride is public = ^ over
       def isAbstract is public = ^ abst
       def isPublic is public = ^ writ
     }
   }

}





