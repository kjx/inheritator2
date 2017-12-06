def visitor = object {
    inherit defaultVisitor

    method visitDialect(d) {
        print "This is in dialect: {d.path}"
    }

    method visitImplicitReceiv1erRequest(irr) {
        print "visiting irr:"
        for (irr.parts) do { p ->
            print "  {p.name}"
            for (p.arguments) do { a ->
                print "    {a}"
            }
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

method checker(obj) {
    print(obj)
    obj.accept(visitor)
}
