type Node = type {
  accept[[T]](visitor : Visitor[[T]]) -> T
}

type Signature = Node & type {
  parts -> Sequence[[SignaturePart]]
  returnType -> Expression
  annotations -> Sequence[[Expression]]
}

type OrdinarySignaturePart = Node & type {
  name -> String
  typeParameters -> Sequence[[Parameter]]
  parameters -> Sequence[[Parameter]]
}

type SignaturePart = OrdinarySignaturePart

type Parameter = Node & type {
  name -> String
  typeAnnotation -> Expression
  isVariadic -> Boolean
}

type ObjectStatement
  = Dialect
  | Import
  | Inherits
  | Method
  | Statement

type Method = Node & type {
  signature -> Signature
  body -> Sequence[[Statement]]
  annotations -> Sequence[[Expression]]
}

type Dialect = Node & type {
  path -> String
}

type Import = Node & type {
  path -> String
  name -> String
  typeAnnotation -> Expression
}

type Inherits = Node & type {
  request -> Request
  name -> String
}

type Statement
  = Declaration
  | Return
  | Expression

type Declaration = Node & type {
  name -> String
  value -> Expression
  annotations -> Sequence[[Expression]]
}

type DefDeclaration = Declaration & type {
  typeAnnotation -> Expression
}

type VarDeclaration = Declaration & type {
  typeAnnotation -> Expression
}

type Return = Node & type {
  value -> Expression
}

type Expression
  = ObjectConstructor
  | Request
  | NumberLiteral
  | StringLiteral
  | Block
  | Type
  | ImplicitDone
  | ImplicitUnknown
  | ImplicitUninitialised

type ObjectConstructor = Node & type {
  body -> Sequence[[ObjectStatement]]
}

type Request = Node & type {
  parts -> Sequence[[RequestPart]]
}

type ImplicitReceiverRequest = Request

type ExplicitReceiverRequest = Request & type {
  receiver -> Expression
}

type RequestPart = Node & type {
  name -> String
  typeArguments -> Sequence[[Expression]]
  arguments -> Sequence[[Expression]]
}

type NumberLiteral = Node & type {
  value -> Number
}

type StringLiteral = Node & type {
  value -> String
}

type Block = Node & type {
  parameters -> Sequence[[Parameter]]
  body -> Sequence[[Statement]]
}

type Type = Node & type {
    signatures -> Sequence[[Signature]]
}

type ImplicitDone = Node & pattern.singleton

type ImplicitUnknown = Node & pattern.singleton

type ImplicitUninitialised = Node & pattern.singleton

type Visitor[[T]] = type {
  visitExpression(node : Expression) -> T
  visitSignature(node : Signature) -> T
  visitSignaturePart(node : SignaturePart) -> T
  visitOrdinarySignaturePart(node : OrdinarySignaturePart) -> T
  visitParameter(node : Parameter) -> T
  visitMethod(node : Method) -> T
  visitDialect(node : Dialect) -> T
  visitImport(node : Import) -> T
  visitInherits(node : Inherits) -> T
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

