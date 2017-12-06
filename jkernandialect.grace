import "jast" as jast
import "combinator-collections" as c
inherit c.abbreviations
// import "ast"as kernanAST  // seems this is broken. we define our own types.

//pity the underlying list API doesn't have an accept() method
method map(f) over(c) {
  def l = list
  for (c) do { e -> l.add( f.apply(e) ) }
  list
}


print "loading jkernandialect"


def visitor = object {  //be careful here. someimes need to refer to visitor
    inherit defaultVisitor

    //be extra careful here - again about the use of "visitor"
    method mapCommon(c) { map { each -> common(each) } over(c) }
    method common(j) { j.accept(visitor) }

    method visitImport(d) {
         print "ERROR visitDialect shouldn't happen"
    }

    method visitDialect(d) {
         print "ERROR visitDialect shouldn't happen"
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
       print "Jesus IWAS EVIL"
       visitImplicitReceiverRequest(fdr)
    }

    method visitIfThenRequest(itr) is evil {
       print "Jesus IFWS EVIL"
       visitImplicitReceiverRequest(itr)
       
    }

    method visitType(t) {
             //print "skipping type {t}"
             return 0
             visitExpression(t)
             for (t.signatures) do { s ->
                s.accept(self)
            }
    }
    
    method visitImplicitReceiverRequest(irr) {
        print "visiting irr:"
        for (irr.parts) do { p ->
            print "  {p.name}"
            for (p.arguments) do { a ->
                print "    {a}"
            }
        }
    }

    method visitObjectConstructor(e) is override {
           print "OC: {e}"
           print "OC body: {e.body}"
           e.body.do { f ->
                        print "OCBodyNode: {f}"
                        f.accept(self)
                        }
    }

    method visitStringLiteral(sl) {
        print "string literal: {sl.value}"
    }

    method visitImplicitUnknown(iu) {
        print "ERROR! Must give types to everything."
    }

    method visitMethod(m) {        
        jast.methodNode( common(m.signature),
                         mapCommon(m.body),
                         mapCommon(m.annotations)) at(0) 
        }

    method visitSignature(sig) {
        jast.signatureNode("hack", list, list, 0, list) at(0)
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
}
