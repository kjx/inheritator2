class a {
    method c is confidential {
        print "c"
    }
    method run {
        c            // implicit receiver request 
        self.c     // self request
        (((self))).c   // parethensides self request
        def x = self 
        x.c        // explicit receiver request
    }
}
a.run
