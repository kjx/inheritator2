import "jast" as jast
import "combinator-collections" as c
inherit c.abbreviations
// import "ast"as kernanAST  // seems this is broken. we define our own types.


print "loading jkernandialect"

def visitor = object {
    inherit defaultVisitor

    method visitImport(d) {
         print "ERROR visitDialect shouldn't happen"
    }

    method visitDialect(d) {
         print "ERROR visitDialect shouldn't happen"
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
        for (m.annotations) do { a ->
            print "Annotation: {a}"
        }
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

        method visitDeclaration(d) {
            d.value.accept(self)
            for (d.annotations) do { a ->
                a.accept(self)
            }
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
