class one { print "one" } 
method two { one } 
method three { two }
method four { three }
class five {
   inherit four 
}

five
