dialect "../static"

self.print "Hello World"
def x = 6
method concat(s:String) and (t:String) -> String { s ++ t }

self.print (concat "Hello" and 6)
