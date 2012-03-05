load "TextIO";

val infile = "problem67.txt" ;

fun readlist infile = let 
  val ins = TextIO.openIn infile 
  fun loop ins = 
   case (TextIO.inputLine ins) of 
      SOME line => line :: loop ins 
    | NONE      => [] 

in 

  loop ins before TextIO.closeIn ins 

end ;

val pureGraph =  readlist(infile);

load "problem8.ml";

