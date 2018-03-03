

class primitivesFamily {
  method context is abstract { }
  method lexicalContext(_) is abstract { }
  method attributeBlockMethod(_) inContext(_) is abstract { }   

  /////////////////////////////////////////////////////////////
  ////
  //// primitivies
  ////
  /////////////////////////////////////////////////////////////

  class ngPrimitive {
    inherit context
    method lookupInheritance(name) { lookupLocal(name) }  //primitives only have local slots

    method whole { self }  //HMM. should clarify actual interface
  }
  
  class ngNumber( value' ) {
     inherit ngPrimitive
     method kind {"ngNumber"}
     method value {value'}
     method asString { "ngNumber: {value}"}

     declareName "+(_)" lambda { other, creatio ->  
                    def rv = ngNumber(value' + other.value)
                    rv } 

     declareName "asString" lambda { creatio ->  
                    def rv = ngString(value'.asString)
                    rv }
  }

  class ngString( value' ) {
     inherit ngPrimitive
     method kind {"ngString"}
     method value {value'}
     method asString { "ngString: {value}"}

     declareName "++(_)" lambda { other, creatio ->  
                    def rv = ngString(value' ++ other.value)
                    rv }

  }

  class ngBoolean( value' ) {
     inherit ngPrimitive
     method kind {"ngBoolean: {value}"}
     method value {value'}
     method asString { "ngBoolean: {value}"}

     // needs some methods which will need the context
  }  


  class ngInterface( value', ctxt ) {   
            //cheating, just points to ast node - and context
     inherit ngPrimitive
     method kind {"ngINrerface"}
     method value {value'}
     method asString { 
        def sigs = safeFuckingMap { sig -> sig.name } over (value.signatures)
        "ngInterface: #{dbg} {sigs}"}


     
     
     method match(other) {//assumes other is an NGO 
        for (value.signatures) do { sig -> 
            if (other.lookupExternal(sig.name).isMissing) then { return false }
            }
        true
     }

     declareName "match(_)" lambda { other, creatio -> 
                                       ngBoolean(match(other)) }
        
  }

  class ngBlock(blockNode,ctxt) {
     inherit lexicalContext(ctxt)
     method asString { "\{a ngBlock\} #{dbg}" }
     method kind {"ngBlock"}
     def p = blockNode.parameters.asList
     def name = match (p.size)
       case { 0 -> "apply" }
       case { 1 -> "apply(_)" }
       case { 2 -> "apply(_,_)" }
       case { 3 -> "apply(_,_,_)" }
       case { 4 -> "apply(_,_,_,_)" }
       case { 5 -> "apply(_,_,_,_,_)" }
       case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }

     declareName(name) attribute (attributeBlockMethod(blockNode) inContext(ctxt))
  }

  /////////////////////////////////////////////////////////////
  ////
  //// singletons
  ////
  /////////////////////////////////////////////////////////////


  def ngDone is public = object {
     inherit ngPrimitive
     method kind{"ngDone"}
     method asString { "ngDone"}
  }

  def ngBuild is public = object {
     inherit ngPrimitive
     method kind {"ngBUild"}
     method asString { "ngBuild"} //result returned from build. always an error.
  }

  def ngUninitialised is public = object {
     inherit ngPrimitive
     method kind {"ngUninit"}
     method asString { "ngUninitialised" } //also an error if accessed
  }

  def ngImplicitUnknown is public = object {
     inherit ngPrimitive
     method kind {"ngImplicitU"}
     method asString { "ngImplicitUnknown" } 
  }

  def ngNotCreatio is public = object {
     inherit ngPrimitive
     method kind {"ngNotCreatio"}
     method asString { "ngNotCreatio" } 
     method isCreatio { false }
  }

  class ngBuiltinAnnotation(description' : String) {
     inherit ngPrimitive                         
     method kind {"ngBuiltinAnnotation"}
     method asString { "ngBuiltinAnnotation(\"{description}\")" } 
     method description { description' }
  }


}
