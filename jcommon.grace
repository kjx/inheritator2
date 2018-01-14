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
    method isWriteable    { isPublic || annotationNames.contains "writeable" }
    method isFinal        { annotationNames.contains "final" }
    method isOverrides    { annotationNames.contains "overrides" }
    //method isComplete     { annotationNames.contains "complete" }
  }


}
