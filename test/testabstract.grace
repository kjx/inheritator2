print "A"

class sup {
    method x {y}
}

class sub {
    inherit sup
    method y { print "y" } 
}
sub.x

print "X"

