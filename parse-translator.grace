//NOTES needs to run with clean-git kernan
//because that's got correct kernan-compiler parse-nodes patch

import "combinator-collections" as c
inherit c.abbreviations

import "platform/KernanCompiler" as kc


method here {
   //HERE

    //there is also the import/inherit bug to do

    //HERE HERE HERE HERE HERE
    //wihout a no-op node, could I guess just return string literal "no-op"
    //rather e.g. than filtering these nodesw out properly.

    //todo - generator or DSL to generate code to "lift" all kernan & extension operations into the interp


    //common ASt renaming; comments, progn

    //THEN need to sort out tests
    // (move to old-tests directoy, collage tests w/ output, automate with both parsers?)
    "here"

    //HERE HEREH HEH
}


import "utility" as utility
inherit utility.exports

import "platform/KernanCompiler" as kc
def pn = kc.parseNodes
def parseNodes = pn

import "errors" as errors
inherit errors.exports





type ASTNode = interface { }

def modules = dictionary[[String, ASTNode]]

def moduleIsBeingLoaded = object { method isLoaded { false } }

def indent = ""
def breakLines = true





method forArray(arr)do(blk) {
    if (arr.isNull) then {return done}
    def size = arr.get_Count
    for (0 .. (size - 1)) do { i ->
        blk.apply((arr.at(i)))
    }
    done
}

method translateArray(arr) {
    def ret = list
    forArray (arr) do { e -> ret.add(translate(e)) }
    ret
}

method translateArrayWithoutComments(arr) {
    def ret = list
    forArray (arr) do { e ->
      if (! (pn.Comment.match(e).succeeded)) then {ret.add(translate(e))}
    }
    ret
}


method listFromArray(arr) {
    def ret = list
    forArray (arr) do { e -> ret.add(e) }
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
method source(n) {n.get_Token.asString}



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



//want for() case() case() case()  along with method() case() case() case

method translateFilename(fileName) {
    def nameSize = fileName.size
    def baseName =
      if ((fileName.substringFrom(nameSize - 5) to(nameSize)) == ".grace")
        then { fileName.substringFrom(1)to(nameSize - 6) }
        else { fileName }
    loadModule( baseName )
}


method translateFile(name) {
      def newModuleKernanTree = kc.parseFile(name ++ ".grace")
      def newModuleCommonTree = translateModule(newModuleKernanTree)
      return newModuleCommonTree
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
                error "parse-translator does not support node type {obj}"
                "<<Untranslated: {obj}>>"
            }
}

method translateStatement(o) {
    //mwh's pp had this  - not sure we need it but keeping it for now
    //this is NOT a leg of  translate(_) above!
    //seems to be about comments mostly, attaching them to the right **statement**
    //COMMENT need to decide about comments - make a ast.comment decorator?
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

method translateInterpolatedString(s) { //TODO //HERE
  //cribbed from Kernan's ExecutionTree.cs
  var ret := false
  forArray(s.get_Parts) do { part ->
     if (false == ret)
       then { ret := translate(part) }
       else {
          var arg 
          if (pn.StringLiteral.match(part))
             then { arg :=  translate(part) }
             else { arg := ast.explicitRequestNode(translate(part), "asString", empty, empty) at(source(s)) }
          ret := ast.explicitRequestNode(ret, "++(_)", empty, list(arg)) at (source(s))
       }
  }
  ret
}


method translateIdentifier(i) {
    ast.implicitRequestNode(i.get_Name, empty, empty) at (source(i))
}

method translateOperator(o) {
     //COMMENT - michales prettyprinter handled comment
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
                     list(body), //COMMON progn?
                     translateAnnotations( m.get_Signature ),
                     "class") at(source(m))
}

method translateTraitDeclaration(m) {   //COMMON object needs annotations; feed down from class...
    def sig = translateSignature( m.get_Signature )
    def body = 
        ast.objectConstructorNode(translateObjectBody(m.get_Body),"missing") at(source(m))
    ast.methodNode( sig,
                     list(body), //COMMON progn?
                     translateAnnotations( m.get_Signature ),
                     "trait") at(source(m))
}

method translateMethodDeclaration(m) {
    def sig = translateSignature( m.get_Signature )
    ast.methodNode( sig,
                     translateArrayWithoutComments( m.get_Body ),
                     translateAnnotations( m.get_Signature ),
                     "method") at(source(m))
}


method translateSignature(s) {
    def parts = listFromArray(s.get_Parts)
    def rawReturnType = s.get_ReturnType
    def returnType = if (!rawReturnType.isNull)
      then {translate(rawReturnType)}
      else {ast.implicitRequestNode("implicitUnknown", empty, empty) at(source(s))}
    def anns = translateAnnotations(s)      
    var name := ""
    var typeParameters := list
    var parameters := list

    for (parts) do { part -> 
            name := name ++ splunge(part.get_Name)
            def partParams = translateTypedParameterList(part.get_Parameters)
            def partTypeParams = translateArray(part.get_GenericParameters)
            // if (c.sizeOfVariadicList(part.typeParameters) > 0) then {
            //   name := name ++
            //      munge(part.typeParameters, "[", "_", ",", "]") }
            if ((parts.size > 1) || (partParams.size > 0)) then {
               name := name ++ munge(partParams, "(", "_", ",", ")") }
            typeParameters := typeParameters ++ partTypeParams
            parameters := parameters ++ partParams
            }
    ast.signatureNode(name, typeParameters, parameters, returnType, anns) at(0)
    }



method translateTypedParameterList(l) {
            for (listFromArray(l))
                 map { p ->
                   //print "TPPZOOB{p}"
                   translateTypedParameter(p)
                 }
}

method translateObjectBody(body) { translateArrayWithoutComments(body) }

method translateObject(o) {
    def comment = o.get_Comment //ignoring COMMENT
    def origin = "missing" ///to do with brands/annotations, see evaluator
    def source = source(o)
    ast.objectConstructorNode(translateObjectBody(o.get_Body),origin) at(source)
}


method translateModule(mod) {
    var moduleDialectName := ""
    def commonModuleBody = list
    forArray (mod.get_Body) do { e ->
      match(e)         
        case { dia : pn.Dialect ->
           print "DOING DIALECT {dia}"
           if (moduleDialectName == "")
             then { moduleDialectName := dia.get_Path.get_Value }
             else { error "TOO MANY DIALECTS" } }
        case { _ : pn.Comment -> }
        case { _ ->
           print "translating {e.get_Token}"
           commonModuleBody.add( translate(e) ) }
    }
    
    print "MODULE DIALECT: {moduleDialectName}"
    print "MODULE SIZE   : {commonModuleBody.size}"

    if (moduleDialectName == "") 
       then { moduleDialectName := STANDARDDIALECT }

    ast.moduleNode(moduleDialectName, commonModuleBody) at(0) 
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
    print "TTP{p}"
    match (p)
       case { ptp : pnTypedParameter ->
            //print "--TPP TYPED"
            name := p.get_Name.get_Name
            typeAnn := translate(p.get_Type)
            }
       case { pid : pnIdentifier ->
            //print "--TPP ID"
            name := p.get_Name
            }
       case { _ ->
            // what I really want to do:
            // crash "WHAT THE FUCK translateTypedParameter{p} needs another case for variadic perhaps?"
            // but this presumably is a matching expression...
            name := "_"  
            typeAnn := translate(p)
            }

    ast.parameterNode(name,typeAnn,variadic) at(source(p))
}

method translateBlock(b) {
    def blockParams = translateTypedParameterList(b.get_Parameters)

    ast.blockNode(
        blockParams,
        translateArrayWithoutComments(b.get_Body)) at (b) //TODO progn?
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
    //or just steal parse tree design(
    //print "COMMENT {c} comment{c.get_Comment} value{c.get_Value}"

    //ast.stringLiteralNode(c.get_Comment) at(source(c))

    crash "trnslateComment shoudlb'e be called"
}


method translateReturn(p) { 
    def rawReturn = p.get_ReturnValue
    if (!rawReturn.isNull)
      then { ast.returnNode(translate(rawReturn)) at(source(p))}
      else { ast.returnNode(
                ast.implicitRequestNode("implicitDone", empty, empty) at(source(p))) at(source(p))}
}

method translateParent(p, kind) {  // kind is "inherit" or "use"

    def excludes = list[[String]]
    forArray (p.get_Excludes)
      do { e -> excludes.add(translateSignature(e.get_Name).name) }

    //def aliases = dictionary[[String,String]]
    //forArray (p.get_Aliases)
    //  do { a -> aliases.at(translateSignature(a.get_OldName).name) put(translateSignature(a.get_NewName).name)}

    def aliases = list[[list]] //list of lists of length 2!! x
    forArray (p.get_Aliases)
      do { a -> aliases.add( list(translateSignature(a.get_NewName).name, translateSignature(a.get_OldName).name))}

    ast.inheritNode(kind, translate(p.get_From), excludes, aliases) at(source(p))
    
    //COMMON consider renaming inheritNode as parentNode
}

method translateInherits(p) {translateParent(p, "inherit")}
method translateUses(p)    {translateParent(p, "use")}


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

method XXtranslateExplicitBracketRequest(b) { //COMMON
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
       seq( translate( t.get_Body )),
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

