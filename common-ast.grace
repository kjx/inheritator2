//james implementation of "common" AST classes


//method error(arg) is required { } 

class jastFamily {
    method asStringPrefix { "jast." }

    print "{asStringPrefix} instantiated {self}"        

    //REALLY BIG DESIGN QUESTION - SHOULD WE HAVE A CLASS NODE??
    //answer - yes but it just delegates to internal method and object nodes?
    //answer - no, but method/object nodes that are from classes
    // should have that information...

    type Unknown = interface { } 
      
    type Parameter = Unknown
    type Expression = Unknown 
    type Signature = Unknown
    type Sequence = Unknown
    type Method = Unknown
    type Return = Unknown
    type Statement = Unknown
    type Node = Unknown
    type Request = Unknown
    type ObjectStatement = Unknown
    type SignaturePart = Unknown
    type OrdinarySignaturePart = Unknown
    type Dialect = Unknown
    type Import = Unknown
    type Inherit = Unknown
    type Declaration = Unknown
    type DefDeclaration = Unknown
    type VarDeclaration = Unknown
    type ObjectConstructor = Unknown
    type ImplicitReceiverRequest = Unknown
    type ExplicitReceiverRequest = Unknown
    type RequestPart = Unknown
    type NumberLiteral = Unknown
    type StringLiteral = Unknown
    type Block = Unknown

    //method debug(b) {b.apply}
    method debug(b) { } 
    
    class nodeAt( source' ) -> Unknown { 
       method asString { asStringPrefix ++ asStringBody }
       method asStringBody { "Node" }
       method accept[[T]](visitor : Visitor[[T]]) -> T { }
       method source { source' }

       method dump {
              print "DUMPING"
              def os = ostream
              dump(os)
              os.printout
       }
       method dump(os) { os.print "_node()" }
    }

    //quetion about this one - it flattens params, relies on name to disambiguate?
    //quetion about this one - it flattens params, relies on name to disambiguate?
    class signatureNode(name' : String,
                        typeParameters' : Sequence[[Parameter]],
                        parameters' : Sequence[[Parameter]],
                        returnType' : Expression,
                        annotations' : Sequence[[Expression]])
          at ( source ) -> Signature {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_signature(\"{name}\","
             os.printList(typeParameters)
             os.printList(parameters)
             returnType.dump(os)
             os.printList(annotations)
             os.print ") //signature {name}"
      }
      
      // this is kind of ugly
      // should replace ALL these defs with methods - shorter
      def name : String is public = name'
      def typeParameters : Sequence[[Parameter]] is public = typeParameters'
      def parameters : Sequence[[Parameter]] is public = parameters'
      def returnType : Expression is public = returnType'
      def annotations : Sequence[[Expression]] = annotations'
            //not clear this is right - need to think about what annotations go where
      debug { print "sig: {name} {parameters} -> {returnType}" }
    
      method asStringBody { "signatureNode {name}..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitSignature(self) }
    }
    

    class parameterNode(name' : String,
      typeAnnotation' : Expression,
      isVariadic' : Boolean)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_parameter(\"{name}\","
             typeAnnotation.dump(os)
             os.print "{isVariadic}"
             os.print ") //parameter {name}"
      }


      def name : String is public = name'
      def typeAnnotation : Expression is public = typeAnnotation'
      def isVariadic : Boolean is public = isVariadic'
    
      debug { print "PARAMETER: {name} : {typeAnnotation} Varargs {isVariadic}"}

      method asStringBody { "parameterNode {name}..." } 

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitParameter(self) }
    }
    
    //should be methodDeclarationNode. or remove "Declaration", "Literal" below
    class methodNode(
      signature' : Signature,
      body' : Sequence[[Statement]],
      annotations' : Sequence[[Expression]],
      kind' : String)
          at( source )  -> Method { 
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_method(\"{signature.name}\","
             signature.dump(os)
             os.printList(body)
             os.printList(annotations)
             os.print "\"{kind}\","
             os.print ") //method {signature.name}"
      }


      def signature : Signature is public = signature'
      def body : Sequence[[Statement]] is public = body'
      def annotations : Sequence[[Expression]] is public = annotations'
      def kind : String is public = kind'
    
      debug { print "method {signature.name} is {annotations} body {body}"}

      method asStringBody { "methodNode {signature.name}..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitMethod(self) }
    }
    

    class inheritNode( //TODO rename as parent
      kind' : String,
      request' : Request,
      excludes' : List[[String]],
      aliases' : Dictionary[[String,String]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 


      method dump(os) {
             os.print "_parent(\"{kind}\",\"{request.name}\","
             request.dump(os)
             os.print "\"{excludes.asString}\"" //TODO should literalise as list of strings
             os.print "\"{aliases.asString}\""  //TODO shoudl litearlise as list of strings
             os.print ") //parent {request.name}"
      }


      def kind : String is public = kind'
      def request : Request is public = request'
      def excludes : List[[String]] is public = excludes'
      def aliases : Dictionary[[String,String]] is public = dictionary
      
      for (aliases') do { a -> 
         aliases.at(a.at(1)) put(a.at(2)) 
         //note ignoring annotations
      }

      method asStringBody { "inheritNode {kind} {request.name} ..." } 
              
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitInherit(self) }
    }

    
    class importNode(
      path' : String,
      name' : String,
      typeAnnotation' : Expression)
          at ( source ) -> Node {
      inherit nodeAt( source )

      method dump(os) {
             os.print "_import(\"{path}\",\"{name}\","
             //typeAnnotation.dump(os)
             os.print "typeANNOTATION not dumped, causes crash"
             os.print ") //import {name}"
      }

      def path : String is public = path'
      def name : String is public = name'
      def typeAnnotation : Expression is public = typeAnnotation'
    
      method asStringBody { "importNode {name}..." } 

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitImport(self) }
    }


    class declarationNode(
      name' : String,
      typeAnnotation' : Expression,
      annotations' : Sequence[[Expression]],
      value' : Expression)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump { crash "sholdnt call" }

      def name : String is public = name'
      def typeAnnotation : Expression is public = typeAnnotation'
      def annotations : Sequence[[Expression]] is public = annotations'
      def value : Expression is public = value'
    
      debug { print "name {name} type {typeAnnotation} is {annotations} = {value}"}
      method asStringBody { "declarationNode {name}..." } 

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

      method dump(os) {
             os.print "_def(\"{name}\","
             typeAnnotation.dump(os)
             os.printList(annotations)
             value.dump(os)
             os.print ") //def {name}"
      }


      debug { print "DEF: {name} type {typeAnnotation} is {annotations} = {value}"}

      method asStringBody { "defDeclartionNode {name}..." } 

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

      method dump(os) {
             os.print "_var(\"{name}\","
             typeAnnotation.dump(os)
             os.printList(annotations)
             value.dump(os)
             os.print ") //var {name}"
      }


      method asStringBody { "varDeclarationNode {name}..." } 

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitVarDeclaration(self) }
    }
    
    

    class returnNode(
      value' : Expression)
          at ( source )  {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "return"
             value.dump(os)
             os.print ") //return"
      }

      def value : Expression is public = value'

      method asStringBody { "returnNode ..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitReturn(self) }
    }
    
    class objectConstructorNode(
      body' : Sequence[[ObjectStatement]],
      origin' : Unknown)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_object("
             os.printList(body)
             os.print ") //object"
      }


      def body : Sequence[[ObjectStatement]]   is public = body'
      method origin { origin' }

      method asStringBody { "objectConstructorNode ..." } 
      
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitObjectConstructor(self) }
    }
    
    
    class moduleNode(
      moduleDialect' : String,
      body' : Sequence[[ObjectStatement]] )
          at ( source ) -> Parameter {
      inherit objectConstructorNode(body', source) at ( source )
    
      def moduleDialect : String   is public = moduleDialect'
      def body is public = body'

      method dump(os) {
             os.print "_module(\"{moduleDialect}\","
             os.printList(body)
             os.print ") //module"
      }


      method asStringBody { "moduleNode ..." } 
        
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitModule(self) }
    }
    
    
    class requestNode(
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 


      method dump(os) {
         crash "should not call dump requestNode"
      }

      def name : String is public = name'
      def typeArguments : Sequence[[Expression]] is public = typeArguments'
      def arguments : Sequence[[Expression]] is public = arguments'

      method asStringBody { "requestNode {name}..." } 
    
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

      method dump(os) {
             os.print "_implicit(\"{name}\","
             os.printList(typeArguments)
             os.printList(arguments)
             os.print ") //implicit {name}"
      }


      method asStringBody { "implicitRequestNode {name}..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitImplicitRequest(self) }

      method implicitRequestNodeBrand { error "called brand" }

      method evilMakeBind(rval) -> ImplicitRequestNode {
         implicitRequestNode(name' ++ ASSIGNMENT_TAIL,
            typeArguments',
            arguments' ++ seq(rval))
            at(source)
      }


    }
    
    
    class explicitRequestNode(
      receiver' : Expression,
      name' : String,
      typeArguments' : Sequence[[Expression]],
      arguments' : Sequence[[Expression]])
          at ( source ) -> Parameter {
      inherit requestNode(name', typeArguments', arguments')
          at( source ) 


      method dump(os) {
             os.print "_explicit(\"{name}\","
             receiver.dump(os)
             os.printList(typeArguments)
             os.printList(arguments)
             os.print ") //explicit {name}"
      }


      def receiver : Expression is public = receiver'

      method asStringBody { "explicitRequestNode {name}..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitExplicitReceiverRequest(self) }


      method evilMakeBind(rval) -> ImplicitRequestNode {
         explicitRequestNode(receiver',
            name' ++ ASSIGNMENT_TAIL,
            typeArguments',
            arguments' ++ seq(rval))
            at(source)
      }


    }

    class numberLiteralNode(  //what to do about radix? - keep it? keep the origianl string
      value' : Number)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) { os.print "{value}" }

      method asStringBody { "Number: {value}" }
    
      def value : Number is public = value'

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitNumberLiteral(self) }
    }
    
    class stringLiteralNode(
      value' : String)
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) { os.print "\"{value}\"" }

      def value : String is public = value'

      method asStringBody { "String:<{value}>" }

      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitStringLiteral(self) }
    }
    
    //Can't blocks have type parameters?  should be able to!
    class blockNode(
      parameters' : Sequence[[Parameter]],
      body' : Sequence[[Statement]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_block("
             os.printList(parameters)
             os.printList(body)
             os.print ") //block"
      }

      def parameters : Sequence[[Parameter]] is public = parameters'
      def body : Sequence[[Statement]] is public = body'

      method asStringBody { "blockNode..." } 
    
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitBlock(self) }
    }
    
    class interfaceNode(
        signatures' : Sequence[[Signature]])
          at ( source ) -> Parameter {
      inherit nodeAt( source ) 

      method dump(os) {
             os.print "_interface("
             os.printList(signatures)
             os.print ") //interface"
      }

      def signatures : Sequence[[Signature]] is public = signatures'
    
      method asStringBody {"interface: {signatures} size: {signatures.size}"}

      debug { print (asString)}

      
      method accept[[T]](visitor : Visitor[[T]]) -> T {
        visitor.visitInterface(self) }
    }
    
    
    
    
    
    
    
    type Visitor[[T]] = interface {
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
      visitInterface(node : Interface) -> T
    }
    
}    

import "combinator-collections" as ccc //GRRR
import "utility" as uuu //GRRR
inherit uuu.exports

class ostream {
  def lines = ccc.abbreviations.list
  method print(str) {
         //actualprint "{str}"
         lines.add(str)
  }
  method printList(l) { //TODO //COMMON this shows the need for a progn aka body!!!
                      //being a list that can be dunmped, evalled, built etc
    print "list("
    for (l) doWithLast { e, last ->
                           e.dump(self) //TODO also dependency loop
                           //if (!last) then {actualprint ","}
                           if (!last) then {endLineWithComma}
                           }
    print ")//list"
  }
  method printout {
         //actualprint "PRINTED WHILE RUNNING"
         actualprint "PRINTOUT {lines.size} {self}"
         for (lines) do { l -> actualprint(l) }
         actualprint "PRINTOUT DONE"
  }
  method endLineWithComma {
     def lastLine = lines.removeLast
     lines.add(lastLine ++ ",")
  }
}

method actualprint(s) {print(s)} //OOPS
