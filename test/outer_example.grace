def x = object {
    def y = object {
     
        method asString {
            return "outer from y"
        }

        method z {
            object {
                print(self)
                print(outer)
                print(outer.outer)

                method asString {
                    "outer from z"
                }
            }
        }
    }

    method asString {
        "outer from x"
    }


}

x.y.z
