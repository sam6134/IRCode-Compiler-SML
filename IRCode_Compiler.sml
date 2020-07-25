val code = CommandLine.arguments();
val infile = hd code ;
val ins = TextIO.openIn infile;
val outfile1 = hd (tl code)
fun getNumber() = 
	let
    	val str = valOf (TextIO.inputLine TextIO.stdIn)
    	val i : int = valOf (Int.fromString str)
	in
        i
	end

fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	

fun readInstruction (index, ins) = 
	if(index = 1) then
		case (TextIO.inputLine ins) of 
        	SOME (chunk) => chunk
		|	NONE => (TextIO.closeIn ins; "NULL")
	else 
		case (TextIO.inputLine ins) of 
        	SOME (chunk) => readInstruction(index-1, ins)
		|	NONE => (TextIO.closeIn ins; "NULL")

fun consumeEquality(s: string) =
	case String.substring(s,0,1) of
		" " => s 
		| _ => consumeEquality(String.extract(s,1,NONE))

fun extractNumeral(s: string, acc) = 
	let
	  val s1 = String.substring(s,0,1);
	in
	  if(s1>="0" andalso s1<="9") then extractNumeral(String.extract(s,1,NONE),acc^s1)
	  else (valOf(Int.fromString(acc)))
	end

fun extractOperator(s: string, acc) = 
	let
	  val s1 = String.substring(s,0,1);
	in
	  if(s1>="0" andalso s1<="9") then acc
	  else if(s1="t") then acc 
	  else extractOperator(String.extract(s,1,NONE),acc^s1)
	end

fun extractNextfew(s: string, acc, flag) =
	if(s="") then acc 
	else 
		let
		  val s1 = String.substring(s,0,1);
		in
		  if(flag=true) then extractNextfew(String.extract(s,1,NONE),acc^s1, true)
		  else if(s1>="0" andalso s1<="9") then extractNextfew(String.extract(s,1,NONE),acc, false)
		  else  extractNextfew(String.extract(s,1,NONE),acc^s1, true)
		end

fun getTillNumeral(s: string) = 
	let
	  val s1 = String.substring(s,0,1);
	in
	  if(s1="t") then s 
	  else if(s1>="0" andalso s1<="9") then s
	  else getTillNumeral(String.extract(s,1,NONE))
	end

fun gettoNextVar(s: string) = 
	let
	  val s1 = String.substring(s,0,1);
	in
	  if(s1="t") then s 
	  else if(s1>="0" andalso s1<="9") then s 
	  else getTillNumeral(String.extract(s,1,NONE))
	end

fun extractLabel(s: string) = 
	let
	  val s1 = String.substring(s,0,1);
	in
	  if(s1="l") then s
	  else extractLabel(String.extract(s,1,NONE))
	end

fun jmpInst(s: string, ins) =
	let
	  val line1 = TextIO.inputLine ins;
	in
	  if(valOf(line1) = s) then ins 
	  else jmpInst(s,ins)
	end


fun performInstruction(flag, symTab, ins) =
	if(flag = true) then 
		let
		  val instruction = readInstruction(1, ins);
		in
		  case String.substring(instruction,0,1) of
		  	"t" => 
			  let
				val indexT = extractNumeral(String.extract(instruction,1,NONE),"") ;
				val nextString = consumeEquality(String.extract(instruction,3,NONE));
				val s1 = String.substring(nextString,1,1);
			  in
				if(s1>="0" andalso s1<="9") then 
					let
					  val value = extractNumeral(String.extract(nextString,1,NONE),"");
					in
					  performInstruction(true, (List.take(symTab,indexT))@[value]@(List.drop(symTab,indexT+1)),ins)
					end
				else 
					let
					  val value = extractNumeral(String.extract(nextString,2,NONE),"") ;
					  val value1 = List.nth(symTab,value) ;
					  val chkString = extractNextfew(String.extract(nextString,3,NONE),"", false) ;
					in
						if(chkString="\n") then performInstruction(true, (List.take(symTab,indexT))@[value1]@(List.drop(symTab,indexT+1)), ins)
						else
							let
							  val operator = extractOperator(chkString,"");
							  val nextString2 = getTillNumeral(String.extract(nextString,3,NONE));
							  val s2 = String.substring(nextString2,0,1);
							in
								if(s2="t") then
									let
									  val value2 = extractNumeral(String.extract(nextString2,1,NONE),"");
									  val value3 = List.nth(symTab,value2) ;
									in
									  case operator of 
							  			"+" => let
													val value4 = value1 + value3 ;
								  				in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
								 				 end
										| "-" => let
													val value4 = value1 - value3 ;
								  				in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
								  				end
										| "*" => let
													val value4 = value1*value3 ;
												in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)),ins)
												end
										| "/" => let
													val value4 = value1 div value3 ;
												in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
												end
										| "<" => 
											if(value1<value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| ">" => 
											if(value1>value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| ">=" => 
											if(value1 >= value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "<=" => 
											if(value1 <= value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "=" => 
											if(value1=value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "<>" => 
											if(value1 = value3) then performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
									end
								else
									let
									  val value3 = extractNumeral(nextString2,"");
									in
									  case operator of 
							  			"+" => let
													val value4 = value1 + value3 ;
								  				in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
								 				 end
										| "-" => let
													val value4 = value1 - value3 ;
								  				in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
								  				end
										| "*" => let
													val value4 = value1*value3 ;
												in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)),ins)
												end
										| "/" => let
													val value4 = value1 div value3 ;
												in
													performInstruction(true, (List.take(symTab,indexT))@[value4]@(List.drop(symTab,indexT+1)), ins)
												end
										| "<" => 
											if(value1<value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| ">" => 
											if(value1>value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| ">=" => 
											if(value1 >= value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "<=" => 
											if(value1 <= value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "=" => 
											if(value1=value3) then performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
										| "<>" => 
											if(value1 = value3) then performInstruction(true, (List.take(symTab,indexT))@[0]@(List.drop(symTab,indexT+1)), ins)
											else performInstruction(true, (List.take(symTab,indexT))@[1]@(List.drop(symTab,indexT+1)), ins)
									end
							end
					end
				end
			| "w" =>
					let
					  val s1 = String.substring(instruction,6,1);
					in
					  if(s1="t") then 
					  	  let
							val value = extractNumeral(String.extract(instruction,7,NONE),"") ;
							val value1 = List.nth(symTab, value) ;
						  in
							(printStringtoFile(Int.toString(value1)^"\n", outfile1); performInstruction(true, symTab, ins))
						  end
					else
						let
						  val value = extractNumeral(String.extract(instruction,6,NONE),"");
						in
						  (printStringtoFile(Int.toString(value)^"\n", outfile1); performInstruction(true, symTab, ins))
						end
					end
			| "i" =>
				let
				  val nextString = gettoNextVar(instruction);
				  val value = extractNumeral(String.extract(nextString,1,NONE), "");
				  val value1 = List.nth(symTab,value) ;
				  val label = extractLabel(nextString);
				in
					if(value1=1) then 
						let
						  val ins1 = TextIO.openIn infile;
						  val jmpStream = jmpInst(label, ins1) ;
						in
						  performInstruction(true, symTab, ins1)
						end
					else performInstruction(true, symTab, ins)
				end
			| "g" => 
				let
				  val label = extractLabel(instruction) ;
				  val ins1 = TextIO.openIn infile;
				  val jmpStream = jmpInst(label, ins1) ;
				in
				  performInstruction(true, symTab, ins1)
				end
			| "r" =>
				let
				  val nextString = gettoNextVar(instruction)
				  val indexT = extractNumeral(String.extract(nextString,1,NONE),"");
				  val value = getNumber() ;
				in
				  performInstruction(true, (List.take(symTab,indexT))@[value]@(List.drop(symTab,indexT+1)), ins)
				end 

			| "l" => performInstruction(true, symTab, ins)
			| "N" => performInstruction(false, symTab, ins) 
		end
	else print("**** DONE *****\n") ;

performInstruction(true,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], ins);

OS.Process.exit(OS.Process.success);