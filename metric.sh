for f in *.grace
  do
    l=`cat $f | wc -l`
    m=`grep method $f | wc -l`
    c=`grep class  $f | wc -l`
    t=`grep trait $f | wc -l`
    o=`grep object $f | wc -l`
    let "ratio=((l*100)/(m+c+t+o))"
    echo "$f: $ratio (methods: $m   classes: $c  traits $t  objects $o)"
done 
