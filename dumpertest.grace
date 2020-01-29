import "common-ast" as ast
inherit ast.jastFamily

def o = ast.ostream

print(o)

nodeAt("").dump(o)

o.printout
