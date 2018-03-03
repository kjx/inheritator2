class firstFamily {
   class top {
     print "firstTop"
     method test {print "firstTopMethod"}
   }
   class bot {
     inherit top
     print "firstbot"
   }
}

class secondFamily {
   inherit firstFamily
      alias firstBot = bot
   class top {
     print "secondtop"
     method test {print "secondTopMethod"}

   }
}

secondFamily.bot.test



