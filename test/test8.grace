class superclass {
    def a = 45
}

class subclass {
    inherit superclass
    method test {
        def a = 5
        print(a)
    }
    method test1 {
        print(a)
        def a = 5
        print(a)
    }
}

subclass.test
subclass.test1
