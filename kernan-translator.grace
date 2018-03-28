import "combinator-collections" as c
inherit c.abbreviations

import "utility" as utility
inherit utility.exports

var jast is public //evil evil dependency inversion

//print "loading jkernandialect"


def visitor = object {  //be careful here. someimes need to refer to visitor
    inherit defaultVisitor

    //be extra careful here - again about the use of "visitor"
    method mapCommon(c) { map { each -> common(each) } over(c) }
    method common(j) { j.accept(visitor) }


    method visitImport(i) {
       jast.importNode(i.path, i.name, i.typeAnnotation) 
               at(0)
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


    method visitInterface(t) {  
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
            // if (c.sizeOfVariadicList(part.typeArguments) > 0) then {
            //   name := name ++
            //      munge(part.typeArguments, "[", "_", ",", "]") }
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
        print "visitOC {e}"
        print "visitOC {e.origin}"
        print "visitOC {e.origin.KJXOrigin}"
        print "visitOC {e.origin.KJXKind}"
        jast.objectConstructorNode(mapCommon(e.body),e.origin) at(0)
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
                         mapCommon(m.annotations),
                         m.kind) at(0) 
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
            // if (c.sizeOfVariadicList(part.typeParameters) > 0) then {
            //   name := name ++
            //      munge(part.typeParameters, "[", "_", ",", "]") }
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




method translate(kernanParseTree) {
    var moduleDialectName := ""
    def commonModuleBody = list
    for (kernanParseTree.body) do { e ->
      match(e)         
        case { _ : kast.Import -> //make sure dialects aren't treated as imports
           commonModuleBody.add( e.accept(visitor) ) }
        case { dia : kast.Dialect -> 
           if (moduleDialectName == "")
             then { moduleDialectName := dia.path }
             else { error "TOO MANY DIALECTS" } }
        case { _ -> 
           commonModuleBody.add( e.accept(visitor) ) }
    }
    
    print "MODULE DIALECT: {moduleDialectName}"
    print "MODULE SIZE   : {commonModuleBody.size}"

    if (moduleDialectName == "") 
       then { moduleDialectName := STANDARDDIALECT }

    jast.moduleNode(moduleDialectName, commonModuleBody) at(0) 
}



//type defs for the case above
//since we can't import types we do this
//note MUST test in this order!
def kast = object {
  type Import = interface {
        path
        name
        typeAnnotation
        accept(visitor)
   }
  type Dialect = interface {
        path
        accept(visitor)
   }
  type Inherit = interface { 
   name
   request
   excludes
   inherits
   }
}

