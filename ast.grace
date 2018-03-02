type Node = interface {
  accept[[T]](visitor : Visitor[[T]]) -> T
}

type Signature = Node & interface {
  parts -> Sequence[[SignaturePart]]
  returnType -> Expression
  annotations -> Sequence[[Expression]]
}

type OrdinarySignaturePart = Node & interface {
  name -> String
  typeParameters -> Sequence[[Parameter]]
  parameters -> Sequence[[Parameter]]
}

type SignaturePart = OrdinarySignaturePart

type Parameter = Node & interface {
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

type Method = Node & interface {
  signature -> Signature
  body -> Sequence[[Statement]]
  annotations -> Sequence[[Expression]]
}

type Dialect = Node & interface {
  path -> String
}

type Import = Node & interface {
  path -> String
  name -> String
  typeAnnotation -> Expression
}

type Inherits = Node & interface {
  request -> Request
  name -> String
}

type Statement
  = Declaration
  | Return
  | Expression

type Declaration = Node & interface {
  name -> String
  value -> Expression
  annotations -> Sequence[[Expression]]
}

type DefDeclaration = Declaration & interface {
  typeAnnotation -> Expression
}

type VarDeclaration = Declaration & interface {
  typeAnnotation -> Expression
}

type Return = Node & interface {
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

type ObjectConstructor = Node & interface {
  body -> Sequence[[ObjectStatement]]
}

type Request = Node & interface {
  parts -> Sequence[[RequestPart]]
}

type ImplicitReceiverRequest = Request

type ExplicitReceiverRequest = Request & interface {
  receiver -> Expression
}

type RequestPart = Node & interface {
  name -> String
  typeArguments -> Sequence[[Expression]]
  arguments -> Sequence[[Expression]]
}

type NumberLiteral = Node & interface {
  value -> Number
}

type StringLiteral = Node & interface {
  value -> String
}

type Block = Node & interface {
  parameters -> Sequence[[Parameter]]
  body -> Sequence[[Statement]]
}

type Type = Node & interface {
    signatures -> Sequence[[Signature]]
}

type ImplicitDone = Node & pattern.singleton

type ImplicitUnknown = Node & pattern.singleton

type ImplicitUninitialised = Node & pattern.singleton

type Visitor[[T]] = interface {
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

