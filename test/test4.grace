var counter := 0
def bump = { print "BUMP!"
             counter := counter + 2
             print "THUMP!"
             counter }
print (bump.apply)

print "DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE "
