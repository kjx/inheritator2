//common definitions
//ya'll know how this'll go down.


class exports {

  method safeFuckingMap(f)over(col) {
     def rv = list
     for (col) do { each -> rv.add(f.apply(each)) }
     rv
  }

  //should probably switch to keysAndValuesDo, last is key == size
  method for(col) doWithLast(block2) {
     def size = col.size
     var counter := 1
     for (col) do { e -> 
       block2.apply(e, counter == size)
       counter := counter + 1 
     }
  }

  def CREATIO = "_creatio"
  def RETURNBLOCK = "_returnBlock"
  def RETURNCREATIO = "_returnCreatio"


  
  trait annotationsTrait { 
    //this trait defines shorthand accessors for the annotations slot.
    method annotationNames is confidential { abstract } 
    method isConfidential { annotationNames.contains "confidential" }
    method isPublic       { ! isConfidential }
    method isAbstract     { annotationNames.contains "abstract" }
    method isConcrete     { ! isAbstract }
    method isReadable     { isPublic || annotationNames.contains "readable" }
    method isWritable     { isPublic || annotationNames.contains "writable" }
    method isFinal        { annotationNames.contains "final" }
    method isOverrides    { annotationNames.contains "overrides" }
  }


}

method processAnnotations(annots,publicByDefault) { 
   def encapsulation = default(publicByDefault) named "encapsulation" 
   for (annots) do { ann -> 
      match (ann.description) 
         case { "confidential" -> encapsulation <- false } 
         case { "public" -> encapsulation <- true } 
         case { d -> error "unknown method annotation {d}" } // should be error
   }
   ^ encapsulation
}

method processVarAnotations(annots) { 
   def read = default(false) named "readable" 
   def writ = default(false) named "writable" 
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
         case { d -> error "unknown method annotation {d}" } // should be error
   }
   object { 
        method isReadable { read }
        method isWritable { writ }
        }
}

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
