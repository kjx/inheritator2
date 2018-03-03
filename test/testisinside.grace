import "jruntime" as objectModel
use objectModel.exports

def c1 = context
def c2 = lexicalContext(c1)
def c3 = lexicalContext(c2)
def oo = lexicalContext(c3)
def c5 = lexicalContext(oo)

print "c1.isInside(c2) {c1.isInside(c2)}"
print "c2.isInside(c1) {c2.isInside(c1)}"

print "oo.isInside(c1)  {oo.isInside(c1)}"
print "oo.isInside(c2)  {oo.isInside(c2)}"
print "oo.isInside(c3)  {oo.isInside(c3)}"
print "oo.isInside(c5)  {oo.isInside(c5)}"

print "c1.isInside(oo)  {c1.isInside(oo)}"
print "c2.isInside(oo)  {c2.isInside(oo)}"
print "c3.isInside(oo)  {c3.isInside(oo)}"
print "c5.isInside(oo)  {c5.isInside(oo)}"
