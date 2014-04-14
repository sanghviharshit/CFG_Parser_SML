Control.Print.stringDepth := 2000;


val inFile = "x1.cfg" ;

exception Scanerror


datatype Token =
	 PROG | Plus | Minus | SCOL | OBRACE | CBRACE | Real of real | GLOBAL | HOST | COMMENT
	 | KEY | EQ | HOSTID | ERR | TYPE | VAL | EOF | valval of string | keyval of string | hostidval of string

datatype ScanReturnError = 
	ERROR
	| NOERROR

fun Tok2Str(token) =
	case token of
		PROG => "PROG"
		| SCOL => "SCOL"
		| OBRACE => "OBRACE"
		| CBRACE => "CBRACE"
		| GLOBAL => "GLOBAL"
		| HOST => "HOST"
		| HOSTID => "HOSTID"
		| KEY => "KEY"
		| EQ => "EQ"
		| VAL => "VAL"
		| _ => "OTHER"

type lexInfo = string * Token

fun realval c = real(ord c - ord #"0")

(*
fun IsSame (item1, item2) = 
	let 
		val itemX = Tok2Str(item1)
		val itemY = Tok2Str(item2)
	in
		case itemX of
		itemY => true
		_ => false
	end
*)
fun IsAscii c = (0 <= ord c andalso ord c <= 127)
fun IsLower c = (#"a" <= c andalso c <= #"z")
fun IsUpper c = (#"A" <= c andalso c <= #"Z")
fun IsEmpty c = (c = [])
fun IsNewLine c = (ord c = 10)
(* fun IsBlank c = (c = #" " orelse c = #"\t") *)
fun IsDigit c = (#"0" <= c andalso c <= #"9")
fun IsLetter c = (IsLower c orelse IsUpper c)
fun IsLetterDigit c = (IsLetter c orelse IsDigit c)
fun IsStringStart c = (IsLetter c orelse c = #"/")
fun IsStringChar c = (IsLetter c orelse IsDigit c orelse c = #"_" orelse c = #"/" orelse c = #"." orelse c = #"-")
fun IsHostIdChar c = (IsLetter c orelse IsDigit c orelse c = #"." orelse c = #"_" orelse c = #"-")
fun IsHostIdStart c = (IsLetter c orelse IsDigit c orelse c = #"." orelse c = #"_" orelse c = #"-")
fun IsKeyChar c = (IsLetter c orelse IsDigit c orelse c = #"_")
fun IsKeyStart c = (IsLetter c orelse c = #"_")
fun IsQuoteChar c = (IsAscii c andalso ord c <> 0 andalso c <> #"\"" andalso c<> #"\\" andalso ord c <> 10)

(*)
fun TrimBlanks (str) =
    case str of
	#" " :: cr =>  TrimBlanks(cr)
      | #"\t" :: cr => TrimBlanks(cr)
      | str =>  str
*)
fun ScanNumber (str, tValue) =
	let fun ScanFirst (str, tValue) =
	    case str of
		c :: cr => 	if c = #"." then ScanFloat(cr, #"." :: tValue)
					else if IsDigit c then ScanFirst(cr, c :: tValue)
		   				else (str, implode (rev tValue), "I", NOERROR)
 	   	| [] => (str, implode (rev tValue), "I", NOERROR)
	and ScanFloat (str, tValue) =
	    case str of
		c :: cr => 	if IsDigit c then ScanFloat(cr, c :: tValue)
		   			else (str, implode (rev tValue), "F", NOERROR)
 	   | [] => (str, implode (rev tValue), "F", NOERROR)
 	in
 		ScanFirst (str, tValue)
 	end
(*

Test float value 3.
0.
and -1

*)

(*)
fun SkipSign (str, tValue) =
	case str of
		#"-" :: cr 	=>  ScanNumber (cr, [#"-"])
		| c :: cr 	=>	(cr, implode (rev tValue))

*)

fun ScanKey (str, tValue) =
	let fun CheckFirstChar (str, tValue) =
		case str of
		c :: cr => 	if IsKeyStart c then ScanRest(cr, c :: tValue)
					else (str, implode (rev tValue), ERROR)
	and ScanRest (str, tValue) =
	    case str of
		c :: cr => 	if IsKeyChar c then ScanRest(cr, c :: tValue)
		   			else (str, implode (rev tValue), NOERROR)
 	   | [] => (str, implode (rev tValue), NOERROR)
 	in
 		CheckFirstChar (str, tValue)
 	end

fun ScanHostId (str,tValue) =
	let fun CheckFirstChar (str, tValue) =
		case str of
		c :: cr => 	if IsHostIdStart c then ScanRest(cr, c :: tValue)
					else (str, implode (rev tValue), ERROR)
	and ScanRest (str, tValue) =
	    case str of
		c :: cr => 	if IsHostIdChar c then ScanRest(cr, c :: tValue)
		   			else (str, implode (rev tValue), NOERROR)
 	   | [] => (str, implode (rev tValue), NOERROR)
 	in
 		CheckFirstChar (str, tValue)
 	end

fun ScanString (str,tValue) =
	let fun CheckFirstChar (str, tValue) =
		case str of
		c :: cr => 	if IsStringStart c then ScanRest(cr, c :: tValue)
					else (str, implode (rev tValue), ERROR)
	and ScanRest (str, tValue) =
	    case str of
		c :: cr => 	if IsStringChar c then ScanRest(cr, c :: tValue)
		   			else (str, implode (rev tValue), NOERROR)
 	   | [] => (str, implode (rev tValue), NOERROR)
 	in
 		CheckFirstChar (str, tValue)
 	end

fun ScanQuote (str,tValue) =
	let fun ScanQ (str, tValue) =
	    case str of

		c :: cnext :: cr => if IsQuoteChar c then ScanQ(cnext::cr, c :: tValue)
							else if (ord c = 92 andalso ord cnext = 92) then ScanQ(cr, cnext :: tValue)
							else if (ord c = 92 andalso ord cnext = 110) then ScanQ(cr, chr 10 :: tValue)
							else if (ord c = 92 andalso ord cnext = 114) then ScanQ(cr, chr 13 :: tValue)
							else if (ord c = 92 andalso ord cnext = 34) then ScanQ(cr, cnext :: tValue)
							else if (ord c = 92) andalso IsQuoteChar cnext then ScanQ(cr, cnext :: tValue)
							else if (ord c = 92) then (cnext::cr, implode (rev tValue), ERROR)
							else if (ord c = 10) then (cnext::cr, implode (rev tValue), ERROR)
							else if c = #"\"" then (cnext::cr, implode (rev tValue), NOERROR)
		   					else (str, implode (rev tValue), ERROR)
 	   | [] => (str, implode (rev tValue), NOERROR)
 	in
 		ScanQ (str, tValue)
 	end

(* Old/WIP Code
	      | #"-" :: c :: cr => 	if IsDigit c then
	      							let val (newStr, tValue, ScError) = ScanNumber(c::cr, [#"-"])
					    			in if ScError = NOERROR then (VAL, tValue, implode(newStr), lineNumber) 
					    				else (print "ERR:L:0\n"; raise Scanerror)
					    			end
					    		else (print "ERR:L:0\n"; raise Scanerror)
	      | #"_" :: cr => 	case prevToken of
	   					HOST 	=> 	let val (newStr, tValue, ScError) = ScanHostId(cr, [#"_"])
			    					in
			   							(HOSTID, tValue, implode(newStr), lineNumber)
			 						end
			 			| OBRACE =>	let val (newStr, tValue, ScError) = ScanKey(cr, [#"_"])
			    					in
			   							(KEY, tValue, implode(newStr), lineNumber)
			 						end
			 			| VAL => 	let val (newStr, tValue, ScError) = ScanKey(cr, [#"_"])
			    					in
			   							(KEY, tValue, implode(newStr), lineNumber)
			 						end
			 			| _		=> 	(print "ERR:L:0\n"; raise Scanerror)

if IsLetter #"c" then 
	      						case prevToken of
	      						HOST => let val (newStr, tValue, ScError) = ScanKey(cr, [#"-"])
					    			in 	if ScError = NOERROR then (VAL, tValue, implode(newStr), lineNumber) 
					    				else (print "ERR:L:0\n"; raise Scanerror)
					    			end
					    		| _ => (print "ERR:L:0\n"; raise Scanerror)
					    	else
					    		(print "ERR:L:0\n"; raise Scanerror)
					    		end
	      | #"-" :: cr => 	let val (newStr, tValue, ScError) = ScanNumber(cr, [#"-"])
					    		in 	if ScError = NOERROR then 	(
					    										if (List.exists (fn: y => (y = #".")) newStr) then (VAL, tValue, implode(newStr),  "F",  lineNumber)
					    										else (VAL, tValue, implode(newStr),  "I",  lineNumber)
					    										)
					    			else (print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
					    		end


#comment 

*)

fun Inc x = x + 1;

fun ScanComment (str, tValue) =
    case str of
	c :: cr => 	if IsNewLine c then
		       		(str, implode (rev tValue), NOERROR)
		   		else ScanComment (cr, c :: tValue)
    | []  => (str, implode (rev tValue), NOERROR) 
		   
fun Scan (str,prevToken,lineNumber) = 
    let	val strArray = explode str
    	fun sc strArray =
	    case strArray of
		[] => (EOF, "", "",  "",  lineNumber )
		  | #" " :: cr => Scan (implode(cr), prevToken,lineNumber)
		  | #"\t" :: cr => Scan (implode(cr), prevToken,lineNumber)
		  | #"\n" :: cr => ((*print ">>>"; print (implode(cr)); print (Int.toString (Inc lineNumber)); *)Scan (implode(cr), prevToken,Inc(lineNumber)))
	      | #"+" :: cr => 	let val (newStr, tValue, tType, ScError) = ScanNumber(cr, [#"+"])
					    		in 	if ScError = NOERROR then (VAL, tValue, implode(newStr),  tType,  lineNumber) 
					    			else (print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
					    		end
	      | #"-" :: cr => 	let val (newStr, tValue, tType, ScError) = ScanNumber(cr, [#"-"])
					    		in 	if ScError = NOERROR then (VAL, tValue, implode(newStr),  tType,  lineNumber) 
					    			else (print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
					    		end
	      | #"." :: cr => 	let val (newStr, tValue, ScError) = ScanHostId(cr, [#"."])
				    			in
				   					(HOSTID, tValue, implode(newStr),  "",  lineNumber)
				 				end
	      | #"_" :: cr => 	(
	      					case prevToken of
							HOST 	=> 	let val (newStr, tValue, ScError) = ScanHostId(cr, [#"_"])
				    					in
				   							(HOSTID, tValue, implode(newStr),  "",  lineNumber)
				 						end
				 			| OBRACE =>	let val (newStr, tValue, ScError) = ScanKey(cr, [#"_"])
				    					in
				   							(KEY, tValue, implode(newStr),  "",  lineNumber)
				 						end
				 			| VAL => 	let val (newStr, tValue, ScError) = ScanKey(cr, [#"_"])
				    					in
				   							(KEY, tValue, implode(newStr),  "",  lineNumber)
				 						end
				 			| _		=> 	(print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
				 			)
	      | #";" :: cr => (* (SCOL,  "", implode(cr)) *) 	(	case prevToken of 
	      														OBRACE	=>	(ERR, "", "",  "",  lineNumber)
	      														| VAL 	=> 	(ERR, "", "",  "",  lineNumber)
	      														| SCOL	=> 	(ERR, "", "",  "",  lineNumber)
	      														| CBRACE=> 	Scan (implode(cr), SCOL,lineNumber)
	      														| _		=> 	(ERR, "", "",  "",  lineNumber)
	      													)
	      | #"{" :: cr => (OBRACE, "", implode(cr),  "",  lineNumber)
	      | #"}" :: cr => (CBRACE, "", implode(cr),  "",  lineNumber)
	      | #"=" :: cr => (EQ, "", implode(cr),  "",  lineNumber)
	      | #"#" :: cr => 	let val (newStr, tValue, ScError) = ScanComment(cr, [])
	      					in Scan (implode(newStr), prevToken,lineNumber) end
	      | #"/" :: cr => 	let val (newStr, tValue, ScError) = ScanString(cr, [#"/"])
					    	in
					    		(VAL, tValue, implode(newStr),  "S",  lineNumber)
					    	end
		  | #"\"" :: cr =>  let val (newStr, tValue, ScError) = ScanQuote(cr, [])
					    	in
					    		case ScError of
					    		NOERROR => 	(VAL, tValue, implode(newStr),  "Q",  lineNumber)
					    		| _		=>	(ERR, "", "",  "",  lineNumber)
					    	end
	      | c :: cr => 	if IsLetter c then
			   				case prevToken of
			   					PROG 	=> 	let val (newStr, tValue, ScError) = ScanKey(cr, [c])
				       						in 
				       						case tValue of
					   						"global" 	=> (GLOBAL, tValue, implode(newStr),  "",  lineNumber)
					    					| _ => (ERR, tValue, implode(newStr),  "",  lineNumber)
					    					end
								| CBRACE => let val (newStr, tValue, ScError) = ScanKey(cr, [c])
				       						in
				       						case tValue of
					   						"host" 	=> (HOST, tValue, implode(newStr),  "",  lineNumber)
					    					| _ => (ERR, tValue, implode(newStr),  "",  lineNumber)
					    					end
					    		| SCOL 	=> 	let val (newStr, tValue, ScError) = ScanKey(cr, [c])
				       						in
				       						case tValue of
					   						"host" 	=> (HOST, tValue, implode(newStr),  "",  lineNumber)
					    					| _ => (ERR, tValue, implode(newStr),  "",  lineNumber)
					    				end
					    		| OBRACE => let val (newStr, tValue, ScError) = ScanKey(cr, [c])
				       						in (KEY, tValue, implode(newStr),  "",  lineNumber)
					    					end
					    		| VAL =>	let
					    					val (newStr, tValue, ScError) = ScanKey(cr, [c])
					    					in
					   				 			(KEY, tValue, implode(newStr),  "",  lineNumber)
					 				   		end
					    		| EQ =>		let val (newStr, tValue, ScError) = ScanString(cr, [c])
					    					in
					    						(VAL, tValue, implode(newStr),  "S",  lineNumber)
					    					end
					    		| HOST => 	let val (newStr, tValue, ScError) = ScanHostId(cr, [c])
					    					in
					    						(HOSTID, tValue, implode(newStr),  "",  lineNumber)
					    					end
					    		| _ => 		((*print ">>>\n"; print (implode(c::[#"\n"]));*)print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
					    else if IsDigit c then
					    	let val (newStr, tValue, tType, ScError) = ScanNumber(c::cr, [])
					    	in (VAL, tValue, implode(newStr),  tType,  lineNumber) end
			   			else ((*print (implode(cr)); print (implode(c::[#"\n"]));*)print "ERR:L:"; print (Int.toString lineNumber); print "\n"; raise Scanerror)
    in sc (strArray) end
    handle Scanerror => OS.Process.exit(OS.Process.success);

fun Readfile (filename: string): string =
    let open TextIO
	val inFile = openIn filename
	val str = inputAll inFile
    in closeIn inFile; str end

val fileStream = Readfile(inFile);

fun PrintQuotes(valType) = 
	if valType = "Q" then print "\"\"\""
	else print ""

fun PrintGroup (hostId, groupType, keyValList) =
	let fun PrintKeyValPairs (keyValList) = 
		case keyValList of 
			(valType, overRide, keyValue, tValue) :: cr => 	(
															print "    ";
															print valType;
															print ":";
															print overRide;
															print ":";
															print keyValue;
															print ":";
															PrintQuotes(valType);
															print tValue;
															PrintQuotes(valType);
															print "\n";
															PrintKeyValPairs (cr)
															)
			| [] => ()
	in
		if groupType = #"G" then (print "GLOBAL:\n")
		else (print "HOST "; print hostId; print ":"; print "\n");													
		PrintKeyValPairs(rev keyValList)
	end

fun ParseHost (hostId, globalKeys, str, prevToken, lineNumber) = 
	let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
		val (kType, overRide, keyName, keyValue) = ("S", "O", "keyName", "keyValue")
	in 
		case token of
		HOST => ParseHost(hostId, globalKeys, cr, HOST, lineNumber)
		| HOSTID => ( ParseHost(tValue, globalKeys, cr, HOSTID, lineNumber) )
		| OBRACE => ( ParseKeyVal(hostId, globalKeys, #"H", cr, OBRACE, lineNumber) )
		| EOF => ()
		| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n")
	end
and ParseKeyVal (hostId: string, globalKeys: string list, groupType: char, str: string, prevToken: Token, lineNumber: int) =
	let	fun ParseKey (keyValList, globalKeys, curHostKeys, str, prevToken, lineNumber)=
		let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
		in 
		case token of
		KEY => ( (*print (Int.toString lineNumber); print "    "; print ":"; print tValue; *)ParseEq(keyValList, globalKeys, curHostKeys, tValue, cr, KEY, lineNumber))
		| CBRACE => ( (*print (Int.toString lineNumber); print "    "; print ":"; print tValue; print ":"; print "\n"; *) ParseHost("", globalKeys, cr, CBRACE, lineNumber) )
		| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n"(*; print tValue*))
		end
	and ParseEq (keyValList, globalKeys, curHostKeys, keyValue, str, prevToken, lineNumber) =
		let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
		in 
		case token of
		EQ => ((*print "="; *) ParseVal (keyValList, globalKeys, curHostKeys, keyValue, cr, EQ, lineNumber))
		| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n"(*; print tValue*))
		end
	and ParseVal (keyValList, globalKeys, curHostKeys, keyValue, str, prevToken, lineNumber)=
		let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
		in 
		case token of
		VAL => 	(
				if ((List.exists (fn y => (y = keyValue)) globalKeys) orelse (List.exists (fn y => (y = keyValue)) curHostKeys)) then 
					if groupType = #"G" then ParseNext((valType, "O", keyValue, tValue)::keyValList, keyValue::globalKeys, curHostKeys, cr, VAL, lineNumber)
					else  ParseNext((valType, "O", keyValue, tValue)::keyValList, globalKeys, keyValue::curHostKeys, cr, VAL, lineNumber)
				else
					if groupType = #"G" then ParseNext((valType, "", keyValue, tValue)::keyValList, keyValue::globalKeys, curHostKeys, cr, VAL, lineNumber)
					else  ParseNext((valType, "", keyValue, tValue)::keyValList, globalKeys, keyValue::curHostKeys, cr, VAL, lineNumber)
				)
		| _ => ( (*print (Tok2Str(token)); *) print "ERR:P:"; print (Int.toString lineNumber); print "\n"(*; print tValue*))
		end
	and ParseNext (keyValList, globalKeys, curHostKeys, str, prevToken, lineNumber)=
		let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
		in 
		case token of
		CBRACE => ( (*print (Int.toString lineNumber); print "    "; print ":"; print tValue; print ":"; print "\n"; *)PrintGroup(hostId, groupType, keyValList); ParseHost("", globalKeys, cr, CBRACE, lineNumber) )
		| KEY => ( (*print (Int.toString lineNumber); print "    "; print ":"; print tValue; print ":"; *) ParseEq(keyValList, globalKeys, curHostKeys, tValue, cr, KEY, lineNumber) )
		| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n"(*; print tValue*))
		end
	in
		ParseKey ([], globalKeys, [], str, prevToken, lineNumber)
	end
and ParseGlobal (str, prevToken, lineNumber) =
	let val (token,tValue,cr,valType,lineNumber) = Scan (str, prevToken, lineNumber)
	in 
		case token of
		OBRACE => ( ParseKeyVal("", [], #"G", cr, OBRACE, lineNumber) )
		| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n")
	end
	
fun ParseFile (fileStream) =
    let val (token,tValue,cr,valType,lineNumber) = Scan (fileStream, PROG, 1)
    in case token of 
	   	GLOBAL => (ParseGlobal(cr, GLOBAL, lineNumber) )
	 	| _ => (print "ERR:P:"; print (Int.toString lineNumber); print "\n")
    end

val it = ParseFile(fileStream);

(*

fun Scan s =
    let fun sc str =
	    case str of
		[] => []
	      | #"+" :: cr => Plus :: sc cr
	      | #"-" :: cr => Minus :: sc cr
	      | #";" :: cr => SCOL :: sc cr
	      | #"{" :: cr => OBRACE :: sc cr
	      | #"}" :: cr => CBRACE :: sc cr
	      | #"=" :: cr => EQ ::
			      let val (str3, n3) = ScanString(cr, [])
			      in case n3 of
					  _ => valval n3 :: sc str3
			      end
	      | #"#" :: cr => let val (str3, n3) = ScanComment(cr, [])
			      in case n3 of
				     _ =>   sc str3
			      end
	      | c :: cr => if IsBlank c then sc cr
			   else if IsLetter c then
			       let val (str1, n) = ScanKey(cr, [c])
			       in case n of
				      "global" => GLOBAL  :: sc str1
				    | "host"   => HOST :: 
						  let val(str2, n2) = ScanHostId(str1,[])
						  in case n2 of
							 _ =>  hostidval  n2 :: sc str2
						  end
				    | _        => keyval n  :: sc str1
			       end
			   else raise Scanerror
    in sc (explode s) end


val token = Scan (Readfile(inFile));

*)