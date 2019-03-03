import "subtyping" as subtyping

class primitivesFamily {
  method context is abstract { }
  method lexicalContext(_) is abstract { }
  method attributeBlockApplyMethod(_) inContext(_) is abstract { }   
  method attributeBlockMatchMethod(_) inContext(_) is abstract { }   

  /////////////////////////////////////////////////////////////
  ////
  //// primitivies
  ////
  /////////////////////////////////////////////////////////////

  class ngPrimitive {
    inherit context
    method lookupInheritance(name) { lookupLocal(name) }  //primitives only have local slots

    method whole { self }  //HMM. should clarify actual interface

    method asString { "ngPrimitive (should be abstract)"}

    declareName "asString" lambda { creatio ->
                    def rv = ngString(asString)
                    rv }
  }
  
  class ngNumber( value' ) {
     inherit ngPrimitive

     method kind {"ngNumber"}
     method value {value'}
     method asString { "ngNumber: {value}"}

     declareName "+(_)" lambda { other, creatio ->  
                    def rv = ngNumber(value' + other.value)
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

     // needs some methods which will need the context -- or will it??

     declareName "prefix!" lambda { _ ->  
                    def rv = ngBoolean(! value)
                    rv }

     declareName "&&(_)" lambda { other, _ ->  
                    def rv = ngBoolean(value && other.value)
                    rv }

     declareName "ifTrue(_)" lambda2 { block, creatio, ctxt -> 
         if (value) then { 
           def argCtxt = ctxt.withoutCreatio
           def creatio = argCtxt.creatio 
           def matchAttribute = block.lookupExternal("apply")
           def matchResult = matchAttribute.invoke(ctxt) args(empty) types(empty) creatio(creatio)
           matchResult
         } else {ngDone} 
      }
  }


  class ngInterface( value', ctxt ) {   
            //cheating, just points to ast node - and context
     inherit ngPrimitive
     method kind {"ngInterface"}
     method value {value'}
     method context { ctxt }
     method asString { 
        def sigs = safeFuckingMap { sig -> sig.name } over (value.signatures)
        "ngInterface: #{dbg} n{value.nodeID} {sigs}"}
     
     
     method match(other) {//assumes other is an NGO 
        for (value.signatures) do { sig -> 
            if (other.lookupExternal(sig.name).isMissing) then { return false }
            }
        true
     }

     method isSubtypeOf(other) {
        subtyping.check(self) isSubtypeOf(other)
     }

     method isTypeEquals(other) { 
        subtyping.check(self) isTypeEquals(other)
     }

     declareName "match(_)" lambda { other, creatio -> 
                                       ngBoolean(match(other)) }

     declareName "<:(_)" lambda { other, creatio -> 
                                       ngBoolean(isSubtypeOf(other)) }
  }

  class ngBlock(blockNode,ctxt) {
     inherit lexicalContext(ctxt)
     method asString { "\{a ngBlock\} #{dbg}" }
     method kind {"ngBlock"}
     def params = blockNode.parameters.asList
     def suffix = match (params.size)
       case { 0 -> "" }
       case { 1 -> "(_)" }
       case { 2 -> "(_,_)" }
       case { 3 -> "(_,_,_)" }
       case { 4 -> "(_,_,_,_)" }
       case { 5 -> "(_,_,_,_,_)" }
       case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }

     declareName("apply" ++ suffix) 
         attribute (attributeBlockApplyMethod(blockNode) inContext(ctxt))
     declareName("match" ++ suffix) 
        attribute (attributeBlockMatchMethod(blockNode) inContext(ctxt))
     declareName "asString" lambda { creatio ->
                    def rv = ngString(asString)
                    rv }

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

  def ngImplicitDone is public = object {
     inherit ngPrimitive
     method kind{"ngImplicitDone"}
     method asString { "ngImplicitDone"}
  }

  def ngBuild is public = object {
     inherit ngPrimitive
     method kind {"ngBuild"}
     method asString { "ngBuild"} //result returned from build. always an error.
  }

  def ngUninitialised is public = object {
     inherit ngPrimitive
     method kind {"ngUninit"}
     method asString { "ngUninitialised" } //also an error if accessed
  }

  def ngUnknown is public = object {
     inherit ngInterface(ngUninitialised,context)
     method kind {"ngUnknown"}
     method asString { "ngUnknown" }
     method match(other) {true}
  }

  def ngImplicitUnknown is public = object {
     inherit ngInterface(ngUninitialised,context)
     method match(other) {true}
     method kind {"ngImplicitUnknown"}
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
