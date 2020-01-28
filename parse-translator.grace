//NOTES needs to run with clean-git kernan
//because that's got correct kernan-compiler parse-nodes patch

import "combinator-collections" as c
inherit c.abbreviations


import "utility" as utility
inherit utility.exports

import "platform/KernanCompiler" as kc
def pn = kc.parseNodes
def parseNodes = pn

import "errors" as errors
inherit errors.exports

var ast is public //evil evil dependency inversion



import "loader" as loader
import "evaluator" as eval
import "object-model" as runtime

ast := eval.singleton
eval.ng := runtime.singleton
def objectModel = runtime.singleton


type ASTNode = interface { }

def modules = dictionary[[String, ASTNode]]

def moduleIsBeingLoaded = object { method isLoaded { false } }

def indent = ""
def breakLines = true

loader.installIntrinsicModule( objectModel.intrinsicModuleObject )


method mapCommon(c) { map { each -> common(each) } over(c) }

method translateArray(arr) {
    //print "TA {arr}"
    if (arr.isNull) then {return empty}
    def ret = list
    def size = arr.get_Count
    for (0 .. (size - 1)) do { i ->
        ret.add(translate(arr.at(i)))
    }
    ret
}

method nonNullArray(arr) {
   if (arr.isNull) then {empty} else {arr}
}

method toArray(arr) {
    if (arr.isNull) then {return empty}
    def ret = list
    def size = arr.get_Count
    for (0 .. (size - 1)) do { i ->
        ret.add(arr.at(i))
    }
    ret
}

method translateType(typeProxy) {
   //print "translateType:{typeProxy}"
   //print "translateType:{typeProxy.isNull}"
   if (!typeProxy.isNull)
      then {translate(typeProxy)}
      else {ast.implicitRequestNode("implicitUnknown", empty, empty) at("nosource")}
}

method translateValue(valueProxy) { 
   if (!valueProxy.isNull)
      then {translate(valueProxy)}
      else {ast.implicitRequestNode("implicitUninitialised", empty, empty) at("nosource")}
}

//method source(n) { "{n.get_Column} @ {n.get_Line}" }
method source(n) { n.get_Token }


def pnObject = parseNodes.Object
def pnNumber = parseNodes.Number
def pnStringLiteral = parseNodes.StringLiteral
def pnInterpolatedString = parseNodes.InterpolatedString
def pnIdentifier = parseNodes.Identifier
def pnOperator = parseNodes.Operator
def pnMethodDeclaration = parseNodes.MethodDeclaration
def pnSignature = parseNodes.Signature
def pnSignaturePart = parseNodes.SignaturePart
def pnClassDeclaration = parseNodes.ClassDeclaration
def pnTraitDeclaration = parseNodes.TraitDeclaration
def pnImplicitReceiverRequest = parseNodes.ImplicitReceiverRequest
def pnExplicitReceiverRequest = parseNodes.ExplicitReceiverRequest
def pnTypedParameter = parseNodes.TypedParameter
def pnBlock = parseNodes.Block
def pnVarDeclaration = parseNodes.VarDeclaration
def pnDefDeclaration = parseNodes.DefDeclaration
def pnParenthesised = parseNodes.Parenthesised
def pnComment = parseNodes.Comment
def pnReturn = parseNodes.Return
def pnInherits = parseNodes.Inherits
def pnUses = parseNodes.Uses
def pnAlias = parseNodes.Alias
def pnExclude = parseNodes.Exclude
def pnBind = parseNodes.Bind
def pnDialect = parseNodes.Dialect
def pnImport = parseNodes.Import
def pnVarArgsParameter = parseNodes.VarArgsParameter
def pnPrefixOperator = parseNodes.PrefixOperator
def pnAnnotations = parseNodes.Annotations
def pnExplicitBracketRequest = parseNodes.ExplicitBracketRequest
def pnInterface = parseNodes.Interface
def pnTypeStatement = parseNodes.TypeStatement




for (kc.args) do { fileName ->
    def nameSize = fileName.size
    def baseName =
      if ((fileName.substringFrom(nameSize - 5) to(nameSize)) == ".grace")
        then { fileName.substringFrom(1)to(nameSize - 6) }
        else { fileName }
    loadModule( baseName )
}


method loadModule(name : String) {
  def mod = modules.at(name) ifAbsent {
      modules.at(name) put(moduleIsBeingLoaded)
      def newModuleParseTree = kc.parseFile(name ++ ".grace")
      def newModuleCommonTree = translate(newModuleParseTree)
      //print "translated: {name} {newModuleCommonTree}"
      def newModule = newModuleCommonTree.eval(objectModel.intrinsicModuleObject)
      modules.at(name) put(newModule)
      return newModule
  }

  if (!mod.isLoaded) then {error "Module {name} is Loading - circular import" }
  return mod
}






// Examine a node and decide where to send it. Each of the methods
// below addresses one kind of node and converts it into a string,
// concatenating its child nodes in as required.
method translate(obj) {
    //print "translate: {obj}"
    match (obj)
        case { n : pn.Number -> translateNumber(n) }
        case { n : pn.StringLiteral -> translateStringLiteral(n) }
        case { n : pn.InterpolatedString ->
            translateInterpolatedString(n) }
        case { o : pn.Operator -> translateOperator(o) }
        case { o : pn.MethodDeclaration ->
            translateMethodDeclaration(o) }
        case { o : pn.ClassDeclaration ->
            translateClassDeclaration(o) }
        case { o : pn.TraitDeclaration ->
            translateTraitDeclaration(o) }
        case { p : pn.SignaturePart -> translateSignaturePart(p) }
        case { s : pn.Signature -> translateSignature(s) }
        case { o : pn.Object -> translateObject(o) }
        case { r : pn.ImplicitReceiverRequest ->
            translateImplicitReceiverRequest(r) }
        case { r : pn.ExplicitReceiverRequest ->
            translateExplicitReceiverRequest(r) }
        case { o : pn.Identifier -> translateIdentifier(o) }
        case { r : pn.TypedParameter ->
            translateTypedParameter(r) }
        case { b : pn.Block -> translateBlock(b) }
        case { b : pn.VarDeclaration -> translateVarDeclaration(b) }
        case { b : pn.DefDeclaration -> translateDefDeclaration(b) }
        case { b : pn.Parenthesised -> translateParenthesised(b) }
        case { b : pn.Comment -> translateComment(b) }
        case { b : pn.Return -> translateReturn(b) }
        case { b : pn.Inherits -> translateInherits(b) }
        case { b : pn.Uses -> translateUses(b) }
        case { b : pn.Bind -> translateBind(b) }
        case { b : pn.Dialect -> translateDialect(b) }
        case { b : pn.Import -> translateImport(b) }
        case { b : pn.VarArgsParameter ->
            translateVarArgsParameter(b) }
        case { b : pn.PrefixOperator ->
            translatePrefixOperator(b) }
        case { b : pn.Annotations -> translateAnnotations(b) }
        case { r : pn.ExplicitBracketRequest ->
            translateExplicitBracketRequest(r) }
        case { b : pn.Interface -> translateInterface(b) }
        case { b : pn.TypeStatement -> translateTypeStatement(b) }
        case { _ ->
                error "Printer does not support node type {obj}"
                "<<Untranslated: {obj}>>"
            }
}

method translateStatement(o) {
    //mwh's pp had this  - not sure we need it but keeping it for now
    //this is NOT a leg of  translate(_) above!
    //seems to be about comments mostly
    //TODO //COMMENT need to decide about comments - make a ast.comment decorator?
    translate(o)
}

method translateNumber(n) {
  //hacked from mwh's AST.cs

            def numbase = n.get_NumericBase
            var integral := 0
            var fractional := 0
            var size := 1
            var frac := false
            for (n.get_Digits) do { c ->
                if (c == ".")
                  then { frac := true }
                  elseif {!frac}
                  then {
                    integral := integral * numbase
                    integral := integral + digit(c)
                     }
                  else {
                    size := size / numbase
                    fractional := fractional + (size * digit(c))
                }
            }
            def val = integral + fractional

            ast.numberLiteralNode(val) at(source(n))
}

method ord(c) { c.codepoints.at(1).codepoint }
method digit(c) {
               if ((c >= "0") && (c <= "9"))
                 then { ord(c) - ord "0" }
                 elseif {(c >= "a") && (c <= "z")}
                 then { (ord(c) - ord "a") + 10 }
                 elseif {(c >= "A") && (c <= "Z")}
                 then { (ord(c) - ord "A") + 10 }
                 else { error "FATAL BAD DIGIT {c} should be cautn in parser" }
}

method translateStringLiteral(s) {
    ast.stringLiteralNode(s.get_Raw) at(source(s))
}

method translateInterpolatedString(s) {
    def rcvr = ast.stringLiteralNode( "" ) at (source(s))

    def parts = s.get_Parts
    def partCount = parts.get_Count
    def args = list

    // Native lists are available as objects with a Count property
    // that can be indexed using .at. These lists are zero-indexed
    // as the host is, and do not yet support iteration.
    for (0 .. (partCount - 1)) do { i ->
        def part = parts.at(i)
        //print "part {i}={part}"
        args.add ( match(part)
            case { _ : pn.StringLiteral -> ast.stringLiteralNode(part.get_Raw)  at (source(s)) }
            case { _ -> ast.explicitRequestNode(translate(part), "asString", empty, empty) at (source(s)) } )
    }

    args.reverse
    var first := true
    var rv := ast.stringLiteralNode("") at(source(s))

    for (args) do { a ->
        if (first) then {
             first := false
             rv := a }
          else {
             rv :=
                 ast.explicitRequestNode(a, "++(_)", empty, list(rv)) at (source(s))           }
    }

    rv
}

method translateIdentifier(i) {
    ast.implicitRequestNode(i.get_Name, empty, empty) at (i)
}

method translateOperator(o) {
     //COMMENT - michales prettyprinter handled comments
     ast.explicitRequestNode(
        translate(o.get_Left), // receiver
        o.get_Name ++ "(_)", // name
        empty, //type arguments
        list( translate(o.get_Right) ) ) //arguments
              at(o)
}



method translateClassDeclaration(m) {  //COMMON object needs annotations; feed down from class...
    def sig = translateSignature( m.get_Signature )
    def body = 
        ast.objectConstructorNode(translateObjectBody(m.get_Body),"missing") at(source(m))
    ast.methodNode( sig,
                     translateArray( m.get_Body ),
                     translateAnnotations( m.get_Signature ),
                     "class") at(source(m))
}

method translateTraitDeclaration(m) {   //COMMON object needs annotations; feed down from class...
    def sig = translateSignature( m.get_Signature )
    def body = 
        ast.objectConstructorNode(translateObjectBody(m.get_Body),"missing") at(source(m))
    ast.methodNode( sig,
                     translateArray( m.get_Body ),
                     translateAnnotations( m.get_Signature ),
                     "trait") at(source(m))
}

method translateMethodDeclaration(m) {
    def sig = translateSignature( m.get_Signature )
    ast.methodNode( sig,
                     translateArray( m.get_Body ),
                     translateAnnotations( m.get_Signature ),
                     "method") at(source(m))
}


method translateSignature(s) {
    def parts = toArray(s.get_Parts)
    def rawReturnType = s.get_ReturnType
    def returnType = if (!rawReturnType.isNull)
      then {translate(rawReturnType)}
      else {ast.implicitRequestNode("implicitUnknown", empty, empty) at(source(s))}
    var name := ""
    var typeParameters := list
    var parameters := list

    for (parts) do { part -> 
            name := name ++ splunge(part.get_Name)
            def partParams =
               for (toArray(part.get_Parameters))
                 map { p ->
                   //print "TPPZOOB{p}"
                   translateTypedParameter(p)
                 }
            def partTypeParams = translateArray(part.get_GenericParameters)
            // if (c.sizeOfVariadicList(part.typeParameters) > 0) then {
            //   name := name ++
            //      munge(part.typeParameters, "[", "_", ",", "]") }
            if ((parts.size > 1) || (partParams.size > 0)) then {
               name := name ++ munge(partParams, "(", "_", ",", ")") }
            typeParameters := typeParameters ++ partTypeParams
            parameters := parameters ++ partParams
            }
    //print "&&&&BONZO{parameters}"
    ast.signatureNode(name, typeParameters, parameters, returnType, empty) at(0) //TODO - hmm
    }





method translateObjectBody(body) {
    def ret = list
    def count = body.get_Count

    //print "translateObejctBody size={count} body={body}"
    for (0 .. (count - 1)) do {i->
        def node = body.at(i)
        //print "  node{i}={node}"
        ret.add(translate(node))
        //print "  ret{i}={ret.size}"
        //print "  ret{i}={ret}"
    }

    //print "translatdObjectObject={ret}"
    ret
}

method translateObject(o) {
    def comment = o.get_Comment //ignoring COMMENT
    def origin = "missing" ///to do with brands/annotations, see evaluator
    def source = source(o)
    ast.objectConstructorNode(translateObjectBody(o.get_Body),origin) at(source)
}

method translateImplicitReceiverRequest(r) {
    //cut and pasted from visitSignature
    //there should be some commonality, should reafactor
    var name := ""
    var typeArguments := list
    var arguments := list

    def nameParts = r.get_NameParts
    def argLists = r.get_Arguments
    def genArgLists = r.get_GenericArguments
    def size = nameParts.get_Count

    //handle each part
    for (0 .. (size - 1)) do { i ->
        def partName = nameParts.at(i).get_Name
        name := name ++ splunge(partName)
        def args = translateArray(argLists.at(i))
        def argCount = args.size

        //Generic args aren't part of request names apparently...

        if ((size > 1) || (argCount > 0)) then {
           name := name ++ munge(args, "(", "_", ",", ")") }

        typeArguments := typeArguments ++ translateArray(genArgLists.at(i))
        arguments := arguments ++ args
    }

    ast.implicitRequestNode(name, typeArguments, arguments) at(source(r))
}

method translateExplicitReceiverRequest(r) {
    def irr = translateImplicitReceiverRequest(r)   //EVIL
    def receiver = r.get_Receiver
    ast.explicitRequestNode( translate(receiver),
        irr.name, irr.typeArguments, irr.arguments) at(irr.source)
}

method translateTypedParameter(p) {
    var name
    var typeAnn := ast.implicitRequestNode("implicitUnknown", empty, empty) at(source(p))
    var variadic := false

    //look art line 116 of ExecutionTree.cs
    match (p)
       case { ptp : pnTypedParameter ->
            //print "--TPP TYPED"
            name := p.get_Name.get_Name
            typeAnn := translateIdentifier(p.get_Type)
            }
       case { pid : pnIdentifier ->
            //print "--TPP ID"
            name := p.get_Name
            }
       case { _ -> crash "WHAT THE FUCK translateTypedParameter{name} needs another case for variadic perhaps?"
                   error}
    //print "XXTPP{name}:{p}:{typeAnn}"
    ast.parameterNode(name,typeAnn,variadic) at(source(p))
}

method translateBlock(b) {
    ast.blockNode(
        translate(b.get_Parameters),
        translate(b.get_Body)) at (b)
}

method translateVarDeclaration(v) {
    def name = v.get_Name.get_Name
    def typeAnnotation = translateType( v.get_Type )
    def annotations = translateAnnotations( v )
    def value = translateValue ( v.get_Value )    

    ast.varDeclarationNode(name, typeAnnotation, annotations, value) at(source(v))
}

method translateDefDeclaration(v) {
    def name = v.get_Name.get_Name
    def typeAnnotation = translateType( v.get_Type )
    def annotations = translateAnnotations( v )
    def value = translateValue ( v.get_Value )    
    ast.defDeclarationNode(name, typeAnnotation, annotations, value) at(source(v))
}

method translateParenthesised(p) {
    translate(p.get_Expression)
}

method translateComment(c) {  //COMMENT //COMMON
    //have to decide what to do about comments
    //quite like the idea of a comment decorator
      //has a value that's the thing decorated
      //or just foo
    //or just steal parse tree design
    //print "COMMENT {c} comment{c.get_Comment} value{c.get_Value}"

    ast.implicitRequestNode("implicitDone", empty, empty) at(source(p))
}


method translateReturn(p) { 
    def rawReturn = p.get_ReturnValue
    if (!rawReturn.isNull)
      then { ast.returnNode(translate(rawReturn)) at(source(p))}
      else { ast.returnNode(
                ast.implicitRequestNode("implicitDone", empty, empty) at(source(p))) at(source(p))}
}

method translateInherits(p) {   //TODO
    def newIndent = indent ++ "    "
    def aliases = p.get_Aliases
    def excludes = p.get_Excludes
    var ret := "inherit {translate(p.get_From, newIndent)}"
    if (aliases.get_Count > 0) then {
        ret := ret ++ translateAliases(aliases, newIndent)
    }
    if (excludes.get_Count > 0) then {
        ret := ret ++ translateExcludes(excludes, newIndent)
    }
    ret
}

method translateUses(p) {   //TODO
    def newIndent = indent ++ "    "
    def aliases = p.get_Aliases
    def excludes = p.get_Excludes
    var ret := "use {translate(p.get_From, newIndent)}"
    if (aliases.get_Count > 0) then {
        ret := ret ++ translateAliases(aliases, newIndent)
    }
    if (excludes.get_Count > 0) then {
        ret := ret ++ translateExcludes(excludes, newIndent)
    }
    ret
}

method translateAliases(aliases) {   //TODO
    def ac = aliases.get_Count
    var ret := ""
    def newIndent = indent ++ "    "
    for (0 .. (ac - 1)) do { i ->
        def a = aliases.at(i)
        ret := ret ++ "\n" ++ indent ++ "alias "
            ++ translate(a.get_NewName, newIndent)
            ++ " = "
            ++ translate(a.get_OldName, newIndent)
    }
    ret
}

method translateExcludes(excludes) {   //TODO
    def ac = excludes.get_Count
    var ret := ""
    def newIndent = indent ++ "    "
    for (0 .. (ac - 1)) do { i ->
        def a = excludes.at(i)
        ret := ret ++ "\n" ++ indent ++ "exclude "
            ++ translate(a.get_Name, newIndent)
    }
    ret
}

method translateBind(o) {
    translate(o.get_Left).evilMakeBind(translate(o.get_Right))
}

method translateDialect(d, ind) {   //currently handled specially
    crash "dialect \"{d.get_Path.get_Raw}\""
    
}

method translateImport(d) {
    ast.importNode(d.get_Path.get_Raw, d.get_Name, d.get_Type)
               at(source(d))

    //"import \"{d.get_Path.get_Raw}\" as {translate(d.get_Name, ind)}"
}

method translateVarArgsParameter(v) { 
    ast.parameterNode(p.get_Name,translate(p.get_Type),true)
        at(source)
}

method translatePrefixOperator(o) {   
     ast.explicitRequestNode(
        translate(o.get_Receiver), // receiver
        "prefix" ++ o.get_Name, // name
        empty, //type arguments
        empty) //arguments
              at(o)
}

method translateAnnotations(o) {
    def annNode = o.get_Annotations
    if (annNode.isNull) then {return empty}
    def axs = annNode.get_Annotations
    translateArray(axs)
}

method translateExplicitBracketRequest(b) { //TODO
    crash "for a[x] indexing. no longer supported"
       
    var ret := "{translate(b.get_Receiver)}{b.get_Token.get_Name}"
    def args = b.get_Arguments
    def argc = args.get_Count
    for (0 .. (argc - 1)) do { i ->
        if (i > 0) then {
            ret := ret ++ ", "
        }
        ret := ret ++ translate(args.at(i))
    }
    "{ret}{b.get_Token.get_Other}"
}

method translateInterface(o) {
    ast.interfaceNode(translateArray(o.get_Body)) at(source(o))
}

method translateTypeStatement(t) {
    ast.methodNode(
       ast.signatureNode(t.get_BaseName.get_Name,
                          translateArray(t.get_GenericParameters),
                          empty,
                          ast.implicitRequestNode("implicitUnknown", empty, empty) at(source(t)),
                          empty) at(source(t)),
       seq( translateInterface( t.get_Body )),  
       empty,
       "type") at(source(t))
}





//return a string of arguments in canonical names
method munge( spart, left, mid, sep, right ) {
        if (spart.size == 0) then { return left ++ right }
        var result := left
        for (1 .. (spart.size - 1)) do { p ->
          result := result ++ mid ++ sep
          }
        result ++ mid ++ right
}

//return only the name part of a name with parenthesis
method splunge( namewithparens ) {
       var idx := 0
       for ( namewithparens ) do { c ->
           if (c == "(")
              then { return namewithparens.substringFrom(1)to(idx) }
           idx := idx + 1
       }
       namewithparens
}

