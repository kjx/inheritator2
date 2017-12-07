import "jast" as jm

class jeval {
  inherit jm.jast
      alias jNodeAt(_) = nodeAt(_)

  class nodeAt( source ) -> Node { 
    inherit jNodeAt(source)
    method asString { "jNode" }
  }


}
