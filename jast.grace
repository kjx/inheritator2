//james implementation of AST classes to more-or-less match kernan AST


import "ast" as ast //Kernan Grace ast types
type Parameter = ast.Parameter
type Expression = ast.Expression 
type Signature = ast.Signature

type Sequence = Unknown


class nodeAt( source ) -> Node { 
   method accept[[T]](visitor : ast.Visitor[[T]]) -> T { }
}

class signatureNode(name' : String,
                    typeParameters' : Sequence[[Paramater]],
                    parameters' : Sequence[[Parameter]],
                    returnType' : Expression,
                    annotations' : Sequence[[Expression]])
      at ( source ) -> Signature {
  inherit nodeAt( source ) 

  def name : String is public = name'
  def typeParameters : Sequence[[Parameter]] is publc = typeParametrs'
  def parameters : Sequence[[Parameter]] is public = parameters'
  def returnType : Expression is public = returnType'
  def annotations : Sequence[[Expression]] = annotations

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
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

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitParameter(self) }
}


class methodNode(
  signature' : Signature,
  body' : Sequence[[Statement]],
  annotations' : Sequence[[Expression]]) -> Method { 
  inherit nodeAt( source ) 

  def signature : Signature is public = signature'
  def body : Sequence[[Statement]] is public = body'
  def annotations : Sequence[[Expression]] is public = annotations'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitMethod(self) }
}

class inheritNode(
  request' : Request,
  name' : String)
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  print "I AM BROKEN BEYOND REPAIR"

  def request : Request is public = request'
  def name : String is public = name'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitInheritance(self) }
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

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
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

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
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

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitVarDeclaration(self) }
}


class typeDeclarationNode(
  name' : String,
  annotations' : Sequence[[Expression]],
  value' : Expression)
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def name : String is public = name'
  def annotations : Sequence[[Expression]] is public = annotations'
  def value : Expression is public = value'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitDeclaration(self) }
}


class returnNode(
  value' : Expression)
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def value : Expression is public = value'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitReturn(self) }
}

class objectConstructorNode(
  body' : Sequence[[ObjectStatement]])
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def body : Sequence[[ObjectStatement]]   is public = body'
  
  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitObjectConstructor(self) }
}


class moduleNode(
  moduleDialect' : String,
  moduelImports' : Dictionary[[String,String]],
  body' : Sequence[[ObjectStatement]] )
      at ( source ) -> Parameter {
  inherit objectConstructorNode(body') at ( source )

  def moduleDialect : String   is public = body'
  def moduleImports : Dictionary[[String,String]] is public = body'
    
  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
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

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitRequest(self) }
}

class implicitRequestNode(
  name' : String,
  typeArguments' : Sequence[[Expression]],
  arguments' : Sequence[[Expression]])
      at ( source ) -> Parameter {
  inherit requestNode(name', typeArguments', arguments')
      at( source ) 

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitImplicitRequest(self) }
}


class explicitReceiverRequestNode(
  receiver' : Expression,
  name' : String,
  typeArguments' : Sequence[[Expression]],
  arguments' : Sequence[[Expression]])
      at ( source ) -> Parameter {
  inherit requestNode(name', typeArguments', arguments')
      at( source ) 

  def receiver : Expression is public = receiver'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitExplicitReceiverRequest(self) }
}


class numberLiteralNode(
  value' : Number)
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def value : Number is public = value'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitNumberLiteral(self) }
}

class stringLiteralNode(
  value : String)
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def value : String is public = value'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitStringLiteral(self) }
}

class blockNode(
  parameters' : Sequence[[Parameter]],
  body' : Sequence[[Statement]])
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def parameters : Sequence[[Parameter]] is public = parameters'
  def body : Sequence[[Statement]] is public = body'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
    visitor.visitBlock(self) }
}

class interfaceNode(
    signatures' : Sequence[[Signature]])
      at ( source ) -> Parameter {
  inherit nodeAt( source ) 

  def signatures : Sequence[[Signature]] is public = signatures'

  method accept[[T]](visitor : ast.Visitor[[T]]) -> T {
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


