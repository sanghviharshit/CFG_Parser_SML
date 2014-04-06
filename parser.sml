val infile = "x3.cfg" ;

exception Scanerror
datatype terminal =
	 Plus | Minus | Scol | Lpar | Rpar | Real of real | GLOBAL | HOST | COMMENT
	 | KEY | Equal | HOSTID
	 | ERR | Type of char | Value | EOF | valval of string | keyval of string | hostidval of string


fun isempty c = (c = [])
fun isnewline c = (c = #"\n")
fun isblank c = (c = #" " orelse c = #"\t" orelse c = #"\n")
fun isdigit c = (#"0" <= c andalso c <= #"9")
fun islower c = (#"a" <= c andalso c <= #"z")
fun isupper c = (#"A" <= c andalso c <= #"Z")
fun isletter c = (islower c orelse isupper c)
fun isletterdigit c = (isletter c orelse isdigit c)
fun scname (cs, value) =
    case cs of
	c :: cr => if isblank c then
		       if isempty(value) then scname(cr,[])
		       else (cs, implode (rev value))
		   else if isletterdigit c then scname(cr, c :: value)
		   else (cs, implode (rev value))
    | [] => (cs, implode (rev value))

fun schostid (cs,value) =
    case cs of
	c :: cr => if isblank c then
		       if isempty(value) then schostid(cr,[])
		       else (cs, implode (rev value))
		   else if isletterdigit c then schostid(cr, c :: value)
		   else (cs, implode (rev value))
      | [] => (cs, implode (rev value))
				    
fun trimblank (cs) =
    case cs of
	#" " :: cr =>  trimblank(cr)
      | #"\n" :: cr => trimblank(cr)
      | #"\t" :: cr => trimblank(cr)
      | cs =>  cs

fun scvalue (cs,value) =
    case cs of
	c :: cr => if isblank c then
		       if isempty(value) then scvalue(cr,[])
		       else (cs, implode (rev value))
		   else if isletterdigit c then scvalue(cr, c :: value)
		   else (cs, implode (rev value))
      | [] => (cs, implode (rev value))

fun scomm (cs, value) =
    case cs of
	c :: cr => if isnewline c then
		       (cs, implode (rev value))
		   else scomm (cr, c :: value)
      | []  => (cs, implode (rev value)) 
		   
fun realval c = real(ord c - ord #"0")

fun scan s = 
    let fun sc cs =
	    case cs of
		[] => (EOF, "", "" )
	      | #"+" :: cr => (Plus, "", implode(cr))
	      | #"-" :: cr => (Minus, "", implode(cr))
	      | #";" :: cr => (Scol,  "", implode(cr))
	      | #"{" :: cr => (Lpar, "", implode(cr))
	      | #"}" :: cr => (Rpar, "", implode(cr))
	      | #"=" :: cr => (Equal, "", implode(cr))
	      | #"#" :: cr => let val (cs3, n3) = scomm(cr, [])
			      in case n3 of
				     _ =>   sc cs3
			      end
	      | c :: cr => 	if isblank c then sc cr
			   			else if isletter c then
					       	let val (cs1, n) = scname(cr, [c])
			       			in case n of
				   			"global" 	=> (GLOBAL, n, implode(cs1))
				    		| "host"   	=> (HOST, n, implode(cs1))
				    		| "key"		=> (KEY, n, implode(cs1))
				    		| "val"		=> (Value, n, implode(cs1))
				    		| "hostid"	=> (HOSTID, n, implode(cs1))
				    		| _ => (ERR, n, implode(cs1))
			       		end
			   			else raise Scanerror
    in sc (explode s) end

fun readfile (filename: string): string =
    let open TextIO
	val infile = openIn filename
	val str = inputAll infile
    in closeIn infile; str end

val filestream = readfile(infile);

fun parseHost (cs) = 
	let val (ts,value, cr) = scan (cs)
		fun parseKeyVal (cs) =
		let val (ts,value,cr) = scan (cs)
		in 
		case ts of
		KEY => ( print "    "; print value; print ":"; parseKeyVal(cr) )
		| Value => ( print value; print "\n"; parseKeyVal(cr) )
		| Equal => ( parseKeyVal(cr) )
		| Rpar => ("\n"; parseKeyVal(cr) )
		| HOST => (print "HOST "; parseHost(cr))
		| EOF => (print "\n")
		| _ => (print "OTHER\n"; print value)
		end
	in 
		case ts of
		HOSTID => ( print "hostid:\n"; parseHost(cr) )
		| Lpar => ( parseKeyVal(cr) )
		| _ => print "OTHER\n"
	end


fun parseGlobal (cs) =
	let val (ts,value,cr) = scan (cs)
	fun parseKeyVal (cs) =
		let val (ts,value, cr) = scan (cs)
		in 
		case ts of
		KEY => ( print "    "; print value; print ":"; parseKeyVal(cr) )
		| Value => ( print value; print "\n"; parseKeyVal(cr) )
		| Equal => ( parseKeyVal(cr) )
		| Rpar => ("\n"; parseKeyVal(cr) )
		| HOST => (print "HOST "; parseHost(cr))
		| EOF => (print "\n")
		| _ => (print "OTHER\n"; print value)
		end
	in 
		case ts of
		Lpar => ( parseKeyVal(cr) )
		| _ => print "OTHER\n"
	end

fun parseFile (filestream) =
    let val (ts,value,cr) = scan (filestream)
    in case ts of 
	   	GLOBAL => (print "GLOBAL:\n"; parseGlobal(cr) )
	 	| _ => print "OTHER\n"
    end

val it = parseFile(filestream);


fun scan s =
    let fun sc cs =
	    case cs of
		[] => []
	      | #"+" :: cr => Plus :: sc cr
	      | #"-" :: cr => Minus :: sc cr
	      | #";" :: cr => Scol :: sc cr
	      | #"{" :: cr => Lpar :: sc cr
	      | #"}" :: cr => Rpar :: sc cr
	      | #"=" :: cr => Equal ::
			      let val (cs3, n3) = scvalue(cr, [])
			      in case n3 of
					  _ => valval n3 :: sc cs3
			      end
	      | #"#" :: cr => let val (cs3, n3) = scomm(cr, [])
			      in case n3 of
				     _ =>   sc cs3
			      end
	      | c :: cr => if isblank c then sc cr
			   else if isletter c then
			       let val (cs1, n) = scname(cr, [c])
			       in case n of
				      "global" => GLOBAL  :: sc cs1
				    | "host"   => HOST :: 
						  let val(cs2, n2) = schostid(cs1,[])
						  in case n2 of
							 _ =>  hostidval  n2 :: sc cs2
						  end
				    | _        => keyval n  :: sc cs1
			       end
			   else raise Scanerror
    in sc (explode s) end


val ts = scan (readfile(infile));

