type A = interface {}
type B = interface { x }
type BProducer = interface { foo -> B }
def o : BProducer = object {
    method foo -> A { object {} }
}
o.foo

