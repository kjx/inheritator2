import "jeval" as jm
import "combinator-collections" as c
inherit c.abbreviations
//import "platform/KernanCompiler" as k 

// import "ast"as kernanAST  // seems this is broken. we define our own types.

def jast = jm.jeval
def jeval = jast

//pity the underlying list API doesn't have an accept() method
method map(f) over(col) {
  def l = list
  for (col) do { e -> l.add( f.apply(e) ) }
  l
}


//print "loading jkernandialect"


def visitor = object {  //be careful here. someimes need to refer to visitor
    inherit defaultVisitor

    //be extra careful here - again about the use of "visitor"
    method mapCommon(c) { map { each -> common(each) } over(c) }
    method common(j) { j.accept(visitor) }


    //could concievably convert these back again like inherits nodes
    //see what looks simpler overall
    method visitImport(d) {
         print "ERROR visitDialect shouldn't happen"
    }

    method visitDialect(d) {
         print "ERROR visitDialect shouldn't happen"
    }


    method visitInherits(i) {

        def doAlias = { a -> 
           def al = c.list.withAll( a )
           list(al.at(1), al.at(2), mapCommon(al.at(3)) )
        }

        jast.inheritNode(i.kind,
                common(i.request),
                c.list.withAll(i.excludes),
                map (doAlias) over (i.aliases) ) at(0)
    }



    method visitDeclaration(d) {
            //print "visitDeclaration {d} name {d.name} annots {d.annotations}"
            object { 
               method name { d.name }
               method typeAnnotation { common (d.typeAnnotation) }
               method annotations { mapCommon (d.annotations) }
               method value { common (d.value) }
            }
    }

    method visitDefDeclaration(dd) is override {
            def d = visitDeclaration(dd)
            jast.defDeclarationNode(d.name, 
                                    d.typeAnnotation,
                                    d.annotations,
                                    d.value) at(0) 
     }


    method visitVarDeclaration(dd) is override {
            def d = visitDeclaration(dd)
            jast.varDeclarationNode(d.name, 
                                    d.typeAnnotation,
                                    d.annotations,
                                    d.value) at(0) 
     }


    method visitForDoRequest(fdr) is evil {
       visitImplicitReceiverRequest(fdr)
    }

    method visitIfThenRequest(itr) is evil {
       visitImplicitReceiverRequest(itr)
       
    }

    method visitReturn(r) {
          jast.returnNode(common(r.value)) at(0)
    }


    method visitType(t) {  
        jast.interfaceNode(mapCommon(t.signatures)) at(0)
    }
    
    method visitImplicitReceiverRequest(irr) {
        //cut and pasted from visitSignature
        //there should be some commonality, should reafactor
        var name := ""
        var typeArguments := list
        var arguments := list

        for (irr.parts) do { part -> 
            name := name ++ splunge(part.name)
            if (c.sizeOfVariadicList(part.typeArguments) > 0) then {
               name := name ++ 
                  munge(part.typeArguments, "[", "_", ",", "]") }
            if ((c.sizeOfVariadicList(irr.parts) > 1) || 
                (c.sizeOfVariadicList(part.arguments) > 0)) then {
               name := name ++ munge(part.arguments, "(", "_", ",", ")") }
            typeArguments := typeArguments ++ mapCommon(part.typeArguments)
            arguments := arguments ++ mapCommon(part.arguments)
            }

        jast.implicitRequestNode(name, typeArguments, arguments) at(0)
    }

    method visitExplicitReceiverRequest(err) { 
       def irr = visitImplicitReceiverRequest(err) 
       jast.explicitRequestNode(common(err.receiver),
                irr.name, irr.typeArguments, irr.arguments) at(0)
    }
    

    method visitObjectConstructor(e) is override {
        jast.objectConstructorNode(mapCommon(e.body)) at(0)
    }

    method visitBlock(b) {
        jast.blockNode(mapCommon(b.parameters), mapCommon(b.body)) at(0)
    }


    method visitStringLiteral(sl) {
        jast.stringLiteralNode(sl.value) at(0)
    }

    method visitNumberLiteral(nl) {
        jast.numberLiteralNode(nl.value) at(0)
    }

    method visitImplicitUnknown(iu) {
        jast.implicitRequestNode("implicitUnknown", empty, empty) at(0)
    }

    method visitImplicitDone(id) {
        jast.implicitRequestNode("implicitDone", empty, empty) at(0)
    }

    method visitImplicitUninitialised (iu) {
        jast.implicitRequestNode("implicitUninitialised", empty, empty) at(0)
    }

    method visitMethod(m) {        
        jast.methodNode( common(m.signature),
                         mapCommon(m.body),
                         mapCommon(m.annotations)) at(0) 
        }


    //return a string of arguments in canonical names
    method munge( spart, left, mid, sep, right ) {
        if (c.sizeOfVariadicList(spart) == 0) then { return left ++ right }
        
        var result := left

        for (1 .. (c.sizeOfVariadicList(spart) - 1)) do { p ->
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



    method visitSignature(sig) {
        var name := ""
        var typeParameters := list
        var parameters := list

        for (sig.parts) do { part -> 
            name := name ++ splunge(part.name)
            if (c.sizeOfVariadicList(part.typeParameters) > 0) then {
               name := name ++ 
                  munge(part.typeParameters, "[", "_", ",", "]") }
            if ((c.sizeOfVariadicList(sig.parts) > 1) || 
                (c.sizeOfVariadicList(part.parameters) > 0)) then {
               name := name ++ munge(part.parameters, "(", "_", ",", ")") }
            typeParameters := typeParameters ++ mapCommon(part.typeParameters)
            parameters := parameters ++ mapCommon(part.parameters)
            }

        jast.signatureNode(name, typeParameters, parameters, common(sig.returnType), empty) at(0)
    }




    method visitParameter(p) {
           jast.parameterNode(p.name, common(p.typeAnnotation),p.isVariadic) at(0)
    }


}

method defaultVisitor {
    object {
        method visitSignature(sig) {
            for (sig.parts) do { p ->
                p.accept(self)
            }
            sig.returnType.accept(self)
            for (sig.annotations) do { a ->
                a.accept(self)
            }
        }

        method visitSignaturePart(sp) {
        }

        method visitOrdinarySignaturePart(osp) {
            visitSignaturePart(osp)
            for (osp.typeParameters) do { tp ->
                tp.accept(self)
            }
            for (osp.parameters) do { p ->
                p.accept(self)
            }
        }

        method visitParameter(p) {
            p.typeAnnotation.accept(self)
        }

        method visitMethod(m) {
            m.signature.accept(self)
            for (m.body) do { b ->
                b.accept(self)
            }
        }

        method visitDialect(d) {}

        method visitImport(i) {
            i.typeAnnotation.accept(self)
        }

        method visitInherits(i) {
            i.request.accept(self)
        }

        method visitDefDeclaration(dd) {
            visitDeclaration(dd)
            dd.typeAnnotation.accept(self)
        }

        method visitVarDeclaration(vd) {
            visitDeclaration(vd)
            vd.typeAnnotation.accept(self)
        }

        method visitReturn(r) {
            r.value.accept(self)
        }

        method visitExpression(_) {}

        method visitObjectConstructor(oc) {
            visitExpression(oc)
            for (oc.body) do { b ->
                b.accept(self)
            }
        }

        method visitRequest(r) {
            visitExpression(r)
            for (r.parts) do { p ->
                p.accept(self)
            }
        }

        method visitImplicitReceiverRequest(irr) {
            visitRequest(irr)
        }

        method visitExplicitReceiverRequest(err) {
            err.receiver.accept(self)
            visitRequest(err)
        }

        method visitRequestPart(rp) {
            for (rp.typeArguments) do { ta ->
                ta.accept(self)
            }
            for (rp.arguments) do { a ->
                a.accept(self)
            }
        }

        method visitNumberLiteral(n) {
            visitExpression(n)
        }

        method visitStringLiteral(sl) {
            visitExpression(sl)
        }

        method visitBlock(b) {
            visitExpression(b)
            for (b.parameters) do { p ->
                p.accept(self)
            }
            for (b.body) do { s ->
                s.accept(self)
            }
        }

        method visitType(t) {
            visitExpression(t)
            for (t.signatures) do { s ->
                s.accept(self)
            }
        }

        method visitImplicitDone(id) {}

        method visitImplicitUnknown(iu) {}

        method visitImplicitUninitialised(iu) {}
    }
}

method checker(module) {
    //assume moduleect is top-levle OC, becomes module:
    def moduleBody = list
    def moduleImports = dictionary
    var moduleDialect := ""
    for (module.body) do { e ->
     match(e)         
      case { imp : kast.Import -> 
               moduleImports.at( imp.name ) put ( imp.path ) }
      case { dia : kast.Dialect -> 
           if (moduleDialect == "") 
             then {moduleDialect := dia.path}
             else {print "ERROR TOO MANY DIALECTS"} }
      case { _ -> moduleBody.add( e.accept(visitor) ) }
    }
    
    print "MODULE DIALECT: {moduleDialect}"
    print "MODULE SIZE   : {moduleBody.size}"
    print "MODULE IMPORTS: {moduleImports}"

    jast.moduleNode( moduleDialect, moduleImports, moduleBody ) at(0) 

    print "EXECUTION EXECUTION EXECUTION EXECUTION"

    //technically this is the "dialect" context  
    //surrounding the module object
    def ctxt = jeval.newEmptyContext
    //should I move these into newEmptyContext?
    ctxt.declare("self") asDef(ctxt)
    ctxt.declare("implicitUninitialised") asDef(jm.ngUninitialised)
    ctxt.declare("print(_)") asMethod (jm.ngMethodLambda{ p, creatio -> print(p) })
    ctxt.declare("_creatio") asMethod(false)

    //def moduleObject = jm.ngObject(list, ctxt)  //hmmm
    
    //for (moduleBody) do { e ->
    //    print (e.eval(moduleObject))
    //}


    def moduleObject = jm.ngObject(moduleBody, ctxt)  //hmmm
       
    }


//since we can't import types we do this
//note MUST test in this order!
def kast = object {
  type Import = {
          path
          name
          typeAnnotation
          accept(visitor)
     }
  type Dialect = {
          path
          accept(visitor)
     }
  type Inherit = { 
     name
     request
     excludes
     inherits
     }
}

