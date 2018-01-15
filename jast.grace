//james implementation of AST classes to more-or-less match kernan AST

//TODO method (or should they be attribute?) nodes should keep track of the ind of thing they came from --- method, class, type... 
//TODO redesign the UI with gaps in parameters (low priority) 
//TODO use a StatementSequence instand of a plan sequence
//TODO we should offer up (i.e. be parameterise by) a sequence too
//TODO rename at as origin?

class jast {    
    
    //REALLY BIG DESIGN QUESTION - SHOULD WE HAVE A CLASS NODE??
    //answer - yes but it just delegates to internal method and object nodes?
    
    //NOTHER QUETION - SHOULD WE HAVE A "self" NODE? (currently not)
   
    type Parameter = Unknown
    type Expression = Unknown 
    type Signature = Unknown
    type Visitor = Unknown
    type Sequence = Unknown

    //REALLY NEED OUR OWN SEQUENCE-
    //with the visitor
    //with an eval..    
    
    //method debug(b) {b.apply}
    method debug(b) { } 
    
    class nodeAt( source ) -> Node { 
       //method asString { "node" }
       method accept[[T]](visitor : Visitor[[T]]) -> T { }
    }

    //quetion about this one - it flattens params, relies on name to disambiguate?
    class signatureNode(name' : String,
                        typeParameters' : Sequence[[Paramater]],
                        parameters' : Sequence[[Parameter]],
                        returnType' : Expression,
                        annotations' : Sequence[[Expression]])
          at ( source ) -> Signature {
      inherit nodeAt( source ) 
     
      
      // this is kind of ugly
      // should replace ALL these defs with methods - shorter
      def name : String is public = name'
      def typeParameters : Sequence[[Parameter]] is publc = typeParameters'
      def parameters : Sequence[[Parameter]] is public = parameters'
      def returnType : Expression is public = returnType'
      def annotations : Sequence[[Expression]] = annotations'
            //not clear this is right - need to think about what annotations go where
      debug { print "sig: {name} {parameters} -> {returnType}" }
    
      method asString { name } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitSignature(self) }
    }
    
    
    class parameterNode(name' : String,
      typeAnnotation' : Expression,
      isVariadic' : Boolean)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def name : String is public = name'
      def typeAnnotation : Expression is public = typeAnnotation'
      def isVariadic : Boolean is public = isVariadic'
    
      debug { print "PARAMETER: {name} : {typeAnnotation} Varargs {isVariadic}"}
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitParameter(self) }
    }
    
    //should be methodDeclarationNode. or remove "Declaration", "Literal" below
    class methodNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]])
          at( source )  -> Method { 
      inherit nodeAt( source ) 
    
      def signature : Signature is public = signature'
      def body : Sequence[[Statement]] is public = body'
      def annotations : Sequence[[Expression]] is public = annotations'
    
      debug { print "method {signature.name} is {annotations} body {body}"}
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitMethod(self) }
    }
    
    class inheritNode(
      kind' : String,
      request' : Request,
      excludes' : List[[String]],
      aliases' : Dictionary[[String,String]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def kind : String is public = kind'
      def request : Request is public = request'
      def excludes : List[[String]] is public = excludes'
      def aliases : Dictionary[[String,String]] is public = dictionary
      
      for (aliases') do { a -> 
         aliases.at(a.at(1)) put(a.at(2)) 
         //note ignoring annotations
      }
              
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitInherit(self) }
    }
    
    
    class declarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def name : String is public = name'
      def typeAnnotation : Expression is public = typeAnnotation'
      def annotations : Sequence[[Expression]] is public = annotations'
      def value : Expression is public = value'
    
      debug { print "name {name} type {typeAnnotation} is {annotations} = {value}"}
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitDeclaration(self) }
    }
    
    class defDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit declarationNode(name', typeAnnotation', annotations', value') 
           at( source ) 
      debug { print "DEF: {name} type {typeAnnotation} is {annotations} = {value}"}
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitDefDeclaration(self) }
    }
    
    class varDeclarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit declarationNode(name', typeAnnotation', annotations', value') 
          at( source ) 
      debug { print "VAR: {name} type {typeAnnotation} is {annotations} = {value}"}
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitVarDeclaration(self) }
    }
    
    //do we want this? kernan codes as method.    
    class typeDeclarationNode(
      name' : String,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def name : String is public = name'
      def annotations : Sequence[[Expression]] is public = annotations'
      def value : Expression is public = value'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitDeclaration(self) }
    }
    
    
    class returnNode(
      value' : Expression)
          at ( source )  {
      inherit nodeAt( source ) 
    
      def value : Expression is public = value'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitReturn(self) }
    }
    
    //partial. needs more to handle inheritance. tomorrow.
    class objectConstructorNode(
      body' : Sequence[[ObjectStatement]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      def body : Sequence[[ObjectStatement]]   is public = body'
      
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitObjectConstructor(self) }
    }
    
    
    class moduleNode(
      moduleDialect' : String,
      moduleImports' : Dictionary[[String,String]],
      body' : Sequence[[ObjectStatement]] )
          at ( source ) -> Parameter {
      inherit objectConstructorNode(body') at ( source )
    
      def moduleDialect : String   is public = body'
      def moduleImports : Dictionary[[String,String]] is public = moduleImports'
        
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitModule(self) }
    }
    
    
    class requestNode(
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def name : String is public = name'
      def typeArguments : Sequence[[Expression]] is public = typeArguments'
      def arguments : Sequence[[Expression]] is public = arguments'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitRequest(self) }
    }
    
    class implicitRequestNode(
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit requestNode(name', typeArguments', arguments')
          at( source ) 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitImplicitRequest(self) }
    }
    
    
    class explicitRequestNode(
      receiver' : Expression,
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit requestNode(name', typeArguments', arguments')
          at( source ) 
    
      def receiver : Expression is public = receiver'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitExplicitReceiverRequest(self) }
    }
    
    
    class numberLiteralNode(
      value' : Number)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      method asString { "Number: {value}" }
    
      def value : Number is public = value'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitNumberLiteral(self) }
    }
    
    class stringLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def value : String is public = value'

      method asString { "String: {value}" }
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitStringLiteral(self) }
    }
    
    //Can't blocks have type parameters?  should be able to!
    class blockNode(
      parameters' : Sequence[[Parameter]],
      body' : Sequence[[Statement]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def parameters : Sequence[[Parameter]] is public = parameters'
      def body : Sequence[[Statement]] is public = body'
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitBlock(self) }
    }
    
    class interfaceNode(
        signatures' : Sequence[[Signature]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 
    
      def signatures : Sequence[[Signature]] is public = signatures'
    
      method asString {"interface: {signatures} size: {signatures.size}"}

      debug { print (asString)}

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitInterface(self) }
    }
    
    
    
    
    
    
    
    type Visitor[[T]] = type {
      visitExpression(node : Expression) -> T
      visitSignature(node : Signature) -> T
      visitSignaturePart(node : SignaturePart) -> T
      visitOrdinarySignaturePart(node : OrdinarySignaturePart) -> T
      visitParameter(node : Parameter) -> T
      visitMethod(node : Method) -> T
      visitDialect(node : Dialect) -> T
      visitImport(node : Import) -> T
      visitInherit(node : Inherit) -> T
      visitDeclaration(node : Declaration) -> T
      visitDefDeclaration(node : DefDeclaration) -> T
      visitVarDeclaration(node : VarDeclaration) -> T
      visitReturn(node : Return) -> T
      visitObjectConstructor(node : ObjectConstructor) -> T
      visitRequest(node : Request) -> T
      visitImplicitReceiverRequest(node : ImplicitReceiverRequest) -> T
      visitExplicitReceiverRequest(node : ExplicitReceiverRequest) -> T
      visitRequestPart(node : RequestPart) -> T
      visitNumberLiteral(node : NumberLiteral) -> T
      visitStringLiteral(node : StringLiteral) -> T
      visitBlock(node : Block) -> T
      visitType(node : Type) -> T
      visitImplicitDone(node : ImplicitDone) -> T
      visitImplicitUnknown(node : ImplicitUnknown) -> T
      visitImplicitUninitialised(node : ImplicitUninitialised) -> T
    }
    
}    
    
