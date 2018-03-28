import "platform/KernanCompiler" as kc                                  
def t = kc.translateFile "brand.grace"
for (t.body) do { each ->        
   print(each) 
   print(each.kind)
   print(each.origin)
}

