open Unix;;
open Stock;;
open Netdate;;
open Yojson;;
open Yojson.Basic.Util;;

let mongoprocess = ref None

let stockQuoteToJSON (stockQuote:stockQuote) : string = 
	let insertString = String.concat "" ["{";"Ticker: '";stockQuote.stock.ticker;"',\n ";"Exchange: '";
	"NYSE";"',\n ";"Price: '";string_of_float stockQuote.price;"',\n ";"Change: '";string_of_float stockQuote.change;
	"',\n ";"Change_Percent: '";string_of_float stockQuote.change_percent;"',\n ";"Last_Trade_Time"] in
	insertString
	
let keyValueListToJSON (kvlist:(string*string*string) list) : string = 
	let querystring = ref ""; in
	let rec loop list =
	begin match list with
		| last :: [] -> 
			begin match last with
				| (fst,snd,thd) -> querystring := (String.concat "" [!querystring;fst;": ";thd;snd;thd]);
			end
		| head :: tail -> 
			begin match head with
				| (fst,snd,thd) -> querystring := (String.concat "" [!querystring;fst;": ";thd;snd;thd;",\n "]); loop tail
			end
		| [] -> print_endline "Failure: empty list"
	end in
	loop kvlist;
	querystring := (String.concat "" ["{";!querystring;"}"]);
	!querystring
	

let insertKeyValue (key:string) (value:string) (collection:string) : unit =
	let writeme = Unix.open_process_out "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	output_string writeme (String.concat "" ["db.";collection;".save({\"";key;"\":\"";value;"\"})\n"])

let insertStockQuote (stockQuote:stockQuote) : unit =
	let writeme = Unix.open_process_out "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	let exchange =
		begin match stockQuote.stock.exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let divstring = 
		begin match stockQuote.dividend with
			| None -> "None"
			| Some(x) -> string_of_float x
		end in
	let yieldstring =
		begin match stockQuote.yield with
			| None -> "None"
			| Some(x) -> string_of_float x
		end in
	let t = stockQuote.last_trade_time_object in
	let datestring = (String.concat "" ["ISODate('";(Netdate.format "%Y-%m-%d %H:%M:%S" t);"')"]) in
	let insertString = keyValueListToJSON [("Ticker",stockQuote.stock.ticker,"'");("Exchange",exchange,"'");
	("Price",string_of_float stockQuote.price,"'");("Change",string_of_float stockQuote.change,"'");("Change_Percent",string_of_float stockQuote.change_percent,"'");
	("Last_Trade_time",datestring,"");("Dividend",divstring,"'");("Yield",yieldstring,"'")] in
	output_string writeme (String.concat "" ["db.stocks.";stockQuote.stock.ticker;".insert(";insertString;")"]);
	flush writeme;
	output_string writeme "exit\n";
	flush writeme;
	close_out writeme

let stockQuoteJSONtoStockQuoteOption (jsonstring:string) : stockQuote option = 
	let jsonstring2 = String.concat "" ["[";jsonstring;"]"] in
	let json =
			try Some((Safe.to_basic (Safe.from_string jsonstring2)))
			with Yojson.Json_error(msg) -> None in
	begin match json with
		| None -> print_endline "stockQuoteJSONtoStockQuoteOption: could not parse json"; None
		| Some(x) ->
			print_endline "stockQuoteJSONtoStockQuoteOption: JSON parsed!";
			let basic = Yojson.Basic.Util.index 0 x in
			try
				let e =  basic |> member "Exchange" |> Yojson.Basic.Util.to_string in
			let exc =
				begin match e with
					| "NASDAQ" -> NASDAQ
					| "NYSE" -> NYSE
					| "CME" -> CME
					| "INDEXSP" -> INDEXSP
					| _ -> OTHER
				end in
				print_endline (String.concat "" ["Exchange: ";e]);
			let t = basic |> member "Ticker" |> Yojson.Basic.Util.to_string in
			print_endline (String.concat "" ["Ticker: ";t]);
			let stock = {ticker = t;exchange = exc} in
			let price = float_of_string (basic |> member "Price" |> Yojson.Basic.Util.to_string) in
			print_endline "Price recorded";
			let change = float_of_string (basic |> member "Change" |> Yojson.Basic.Util.to_string) in
			print_endline "Change recorded";
			let changepercent = float_of_string (basic |> member "Change_Percent" |> Yojson.Basic.Util.to_string) in
			print_endline "Change percent recorded";
			let dividend = 
				try Some(float_of_string (basic |> member "Dividend" |> Yojson.Basic.Util.to_string)) 
				with _ -> None in
			print_endline "Dividend recorded";
			let lasttradetime = basic |> member "Last_Trade_time" |> Yojson.Basic.Util.to_string in
			let r = Str.regexp "\\(.*\\)'\\(.*\\)'\\(.*\\)" in
			Str.string_match r lasttradetime 0;
			let lasttradedate = Str.matched_group 2 lasttradetime in
			let yield = 
				try Some(float_of_string (basic |> member "Yield" |> Yojson.Basic.Util.to_string))
				with _ -> None in
			print_endline "Yield recorded";
			Some({stock = stock;price = price;change = change;change_percent = changepercent;last_trade_time = (Netdate.format "%b %e %Y, %l:%M%p %z" (Netdate.parse lasttradedate));last_trade_time_object = Netdate.parse lasttradedate; dividend = dividend;yield = yield})
			with Yojson.Basic.Util.Type_error(msg,i) -> None
	end
	

let stockIsPresent (ticker:string) (exchange:exchange) : (stockQuote option*bool) =
	let readme,writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	let exchangestring = 
		begin match exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let querystring = String.concat "" ["db.stocks.findOne({Ticker:'";ticker;"',Exchange:'";exchangestring;"'})\n"] in
	let resultlist = ref [] in
	let replaceNum line =
		let r = Str.regexp "\\(.*\n*.*: \\)\\([0-9-].*\\)\\(,.*\n*.*\\)" in
		begin match Str.string_match r line 0 with
			| true -> Str.global_replace r "\\1\"\\2\"\\3" line
			| false -> line
		end
	in
	let r = Str.regexp "\\(.*\n*.*\\)ISODate(\"\\(.*\\)\")\\(.*\n*.*\\)" in
	let rec loop line = 
		let line2 = replaceNum line in
		begin match Str.string_match r line2 0 with
			| true -> resultlist := Str.global_replace r "\\1\"ISODate('\\2')\"\\3" line2 :: !resultlist;
			| false -> resultlist := line2 :: !resultlist;
		end;
		loop (input_line readme) in
	output_string writeme querystring;
	flush writeme;
	output_string writeme "exit\n";
	flush writeme;
	try loop (input_line readme)
	with End_of_file -> ();
	try resultlist := (List.tl (List.tl (List.rev (List.tl !resultlist))));
	resultlist := 
		begin match !resultlist with
			| fst::snd::tail -> fst::tail
			| rest -> rest
		end;
	let resultstring = String.concat "\n" !resultlist in
			print_endline (String.concat "" ["Resultstring in StockIsPresent: "; resultstring]);
			ignore (Unix.close_process (readme,writeme));
			print_endline "StockIsPresent mongo closed";
			begin match stockQuoteJSONtoStockQuoteOption resultstring with
				| None -> (None,false)
				| Some(x) -> (Some(x),true)
			end
	with Failure(msg) -> print_endline "stockIsPresent: failed to process resultlist"; (None,false)

let insertStock (stockQuote:stockQuote) : unit =
	print_endline "Started insert Stock";
	let readme,writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	let exchange =
		begin match stockQuote.stock.exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let divstring,divsep = 
		begin match stockQuote.dividend with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let (yieldstring,yieldsep) =
		begin match stockQuote.yield with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let t = stockQuote.last_trade_time_object in
	let datestring = (String.concat "" ["ISODate('";(Netdate.format "%Y-%m-%d %H:%M:%S" t);"')"]) in
	let insertString = keyValueListToJSON [("Ticker",stockQuote.stock.ticker,"'");("Exchange",exchange,"'");
	("Price",string_of_float stockQuote.price,"");("Change",string_of_float stockQuote.change,"");("Change_Percent",string_of_float stockQuote.change_percent,"");
	("Last_Trade_time",datestring,"");("Dividend",divstring,divsep);("Yield",yieldstring,yieldsep)] in
	print_endline insertString;
	let soption,present = stockIsPresent stockQuote.stock.ticker stockQuote.stock.exchange in
	match present with
		| true -> print_endline "Stock Present"; output_string writeme (String.concat "" ["db.stocks.update({Ticker:'";stockQuote.stock.ticker;"',Exchange:'";exchange;"'},";
	"{$set:";insertString;"},false,true)\n"]); flush writeme; output_string writeme "exit\n"; flush writeme;
		| false -> print_endline "Stock Not Present"; output_string writeme (String.concat "" ["db.stocks.insert(";insertString;")\n"]); flush writeme; output_string writeme "exit\n";
			flush writeme;
	ignore (Unix.close_process (readme,writeme))
	
let insertStockQuote (stockQuote:stockQuote) : unit = 
	let readme, writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	insertStock stockQuote;
	print_endline "insertStock finished";
	let exchange =
		begin match stockQuote.stock.exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let divstring,divsep = 
		begin match stockQuote.dividend with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let yieldstring,yieldsep =
		begin match stockQuote.yield with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	print_endline "Yield reached";
	let t = stockQuote.last_trade_time_object in
	let datestring = (String.concat "" ["ISODate('";(Netdate.format "%Y-%m-%d %H:%M:%S" t);"')"]) in
	let insertString = keyValueListToJSON [("Ticker",stockQuote.stock.ticker,"'");("Exchange",exchange,"'");
	("Price",string_of_float stockQuote.price,"");("Change",string_of_float stockQuote.change,"");("Change_Percent",string_of_float stockQuote.change_percent,"");
	("Last_Trade_time",datestring,"");("Dividend",divstring,divsep);("Yield",yieldstring,yieldsep)] in
	print_endline "keyvalue reached";
	output_string writeme (String.concat "" ["db.stocks.";stockQuote.stock.ticker;".insert(";insertString;")\n"]);
	flush writeme;
	output_string writeme "exit\n";
	flush writeme;
	print_endline "Output done";
	ignore (Unix.close_process (readme,writeme))
	
	
let getStocks () : string list option = 
	let readme,writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	output_string writeme "db.stocks.distinct('Ticker')\n";
	flush writeme;
	output_string writeme "exit\n";
	flush writeme;
	let resultlist = ref [] in
	let rec loop line =
		print_endline line;
		resultlist := line :: !resultlist;
		print_endline "in loop";
		loop (input_line readme)
		in
	begin
		try loop (input_line readme) 
		with End_of_file -> ();
	end;
	try
		try resultlist := (List.tl (List.tl (List.rev (List.tl !resultlist))));
		let json = Yojson.Safe.from_string (String.concat "\n" !resultlist) in
		ignore (Unix.close_process (readme,writeme));
		Some(Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_string (Yojson.Safe.to_basic json))
		with _ -> None
	with Yojson.Json_error(msg) -> None
	
	
let clearDatabases () : unit = 
	let readme, writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	let stocklist = getStocks () in
	let removefun ticker =
		print_endline ticker;
		output_string writeme (String.concat "" ["db.stocks.";ticker;".remove()\n"])
		in
	begin match stocklist with
		| None -> (); print_endline "No stocks found"
		| Some(x) -> List.iter removefun x; 
	end;
	output_string writeme "db.stocks.remove()";
	close_out writeme;
	ignore (Unix.close_process (readme,writeme))
	
let insertStockQuotes (tickerlist:string list) : unit =
	let rec loop list =
		begin match list with
			| hd :: tl ->
				print_endline (String.concat "" ["Current Ticker: ";hd]); 
				let stockquoteoption = getStockQuote hd in
				begin match stockquoteoption with
					| None -> ()
					| Some(stockquote) -> insertStockQuote stockquote;
				end; loop tl
			| _ -> ()
		end in
	loop tickerlist
	
let stockIsPresentWithChannels (ticker:string) (exchange:exchange) (readme:in_channel) (writeme:out_channel) : (stockQuote option*bool) =
	let exchangestring = 
		begin match exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let querystring = String.concat "" ["db.stocks.findOne({Ticker:'";ticker;"',Exchange:'";exchangestring;"'})\n"] in
	let resultlist = ref [] in
	let replaceNum line =
		let r = Str.regexp "\\(.*\n*.*: \\)\\([0-9-].*\\)\\(,.*\n*.*\\)" in
		begin match Str.string_match r line 0 with
			| true -> Str.global_replace r "\\1\"\\2\"\\3" line
			| false -> line
		end
	in
	let r = Str.regexp "\\(.*\n*.*\\)ISODate(\"\\(.*\\)\")\\(.*\n*.*\\)" in
	let rec loop line =
		let line2 = replaceNum line in
		print_endline (String.escaped line2);
		begin match Str.string_match r line2 0 with
			| true -> resultlist := Str.global_replace r "\\1\"ISODate('\\2')\"\\3" line2 :: !resultlist;
			| false -> resultlist := line2 :: !resultlist;
		end;
		begin match line2 with
			| "}" -> print_endline "end bracket"; raise End_of_file
			| _ -> print_endline "not empty string"
		end;
		print_endline "inloop";
		loop (input_line readme) in
	output_string writeme querystring;
	flush writeme;
	print_endline "entering try loop";
	begin
		try loop (input_line readme);
		with End_of_file -> ();
	end;
	print_endline "out of loop";
	try resultlist := (List.rev (!resultlist));
	resultlist := 
		begin match !resultlist with
			| fst::snd::tail -> fst::tail
			| rest -> rest
		end;
	let resultstring = String.concat "\n" !resultlist in
			print_endline (String.concat "" ["Resultstring in StockIsPresent: "; resultstring]);
			print_endline "StockIsPresent mongo closed";
			begin match stockQuoteJSONtoStockQuoteOption resultstring with
				| None -> (None,false)
				| Some(x) -> (Some(x),true)
			end
	with Failure(msg) -> print_endline "stockIsPresent: failed to process resultlist"; (None,false)	
	
let insertStockWithChannels (stockQuote:stockQuote) (readme:in_channel) (writeme:out_channel) : unit =
	print_endline "Started insert Stock";
	let exchange =
		begin match stockQuote.stock.exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let divstring,divsep = 
		begin match stockQuote.dividend with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let (yieldstring,yieldsep) =
		begin match stockQuote.yield with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let t = stockQuote.last_trade_time_object in
	let datestring = (String.concat "" ["ISODate('";(Netdate.format "%Y-%m-%d %H:%M:%S" t);"')"]) in
	let insertString = keyValueListToJSON [("Ticker",stockQuote.stock.ticker,"'");("Exchange",exchange,"'");
	("Price",string_of_float stockQuote.price,"");("Change",string_of_float stockQuote.change,"");("Change_Percent",string_of_float stockQuote.change_percent,"");
	("Last_Trade_time",datestring,"");("Dividend",divstring,divsep);("Yield",yieldstring,yieldsep)] in
	print_endline insertString;
	print_endline "begin stockispresent";
	let soption,present = stockIsPresentWithChannels stockQuote.stock.ticker stockQuote.stock.exchange readme writeme in
	match present with
		| true -> print_endline "Stock Present"; output_string writeme (String.concat "" ["db.stocks.update({Ticker:'";stockQuote.stock.ticker;"',Exchange:'";exchange;"'},";
	"{$set:";insertString;"},false,true)\n"]); flush writeme
		| false -> print_endline "Stock Not Present"; output_string writeme (String.concat "" ["db.stocks.insert(";insertString;")\n"]); flush writeme
			
let insertStockQuoteWithChannels (stockQuote:stockQuote) (readme:in_channel) (writeme:out_channel) : unit = 
	insertStockWithChannels stockQuote readme writeme;
	print_endline "insertStock finished";
	let exchange =
		begin match stockQuote.stock.exchange with
			| NYSE -> "NYSE"
			| NASDAQ -> "NASDAQ"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER -> "OTHER"
		end in
	let divstring,divsep = 
		begin match stockQuote.dividend with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	let yieldstring,yieldsep =
		begin match stockQuote.yield with
			| None -> "None","'"
			| Some(x) -> string_of_float x,""
		end in
	print_endline "Yield reached";
	let t = stockQuote.last_trade_time_object in
	let datestring = (String.concat "" ["ISODate('";(Netdate.format "%Y-%m-%d %H:%M:%S" t);"')"]) in
	let insertString = keyValueListToJSON [("Ticker",stockQuote.stock.ticker,"'");("Exchange",exchange,"'");
	("Price",string_of_float stockQuote.price,"");("Change",string_of_float stockQuote.change,"");("Change_Percent",string_of_float stockQuote.change_percent,"");
	("Last_Trade_time",datestring,"");("Dividend",divstring,divsep);("Yield",yieldstring,yieldsep)] in
	print_endline "keyvalue reached";
	output_string writeme (String.concat "" ["db.stocks.";stockQuote.stock.ticker;".insert(";insertString;")\n"]);
	flush writeme;
	print_endline "Output done"
	
	
let gatherStockData (interval:int) : unit = 
	let stocklist = getStocks() in
	let readme,writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo" in
	input_line readme;
	input_line readme;
	let stockfun stockTicker =
			begin match getStockQuote stockTicker with
				| None -> ()
				| Some(x) -> 
					print_endline "Inserting Stock"; 
					insertStockQuoteWithChannels x readme writeme;
			end in
	let rec mainloop list =
		List.iter stockfun list;
		Unix.sleep interval;
		mainloop list;
		in
	let rec runloop list = 
		try mainloop list
		with Unix_error(a,b,c) -> print_endline "********************UNIX ERROR********************"; print_endline (error_message a); print_endline b; print_endline c; Unix.sleep 5; runloop list
		in
	begin match stocklist with
		| None -> ()
		| Some(x) -> print_endline "Starting runloop"; runloop x
	end

(*;;insertStockQuotes ["AAPL";"GOOG";"FB";"ZNGA";"MSFT";"QCOM";"EMC";"DOW";"AMZN";"IBM";"SAP";"BIDU";"HPQ";*)
(*"NVDA";"ARMH";"CMG"]                                                                                     *)
(*;;insertStockQuotes [".INX"]*)
(*;;insertStockQuotes ["GOOG"]*)
(*;;insertStockQuotes (getNASDAQTickers ())*);
(*;;begin match getStockQuote "MGR" with |None -> () |Some(x) -> insertStockQuote x end*)
(*;;print_endline (String.concat "\n" (getNYSETickers ()))  *)
(*;;print_endline (String.concat "\n" (getNASDAQTickers ()))*)
(*;;clearDatabases ()*)
(*;;let t = Netdate.create (Unix.time ())       *)
(*;;print_endline (Netdate.format "%l:%M:%S" t) *)
(*;;let x = getStocks()                                                                                      *)
(*;;begin match x with |None -> print_endline "None"; () |Some(y) -> print_endline (String.concat "\n" y) end*)
(*;;begin match x with |None ->() |Some(y) -> print_endline (String.concat "\n" y) end*)
(*;;begin match x with |None -> () |Some(y) -> print_endline (String.concat " " y) end*)

(*;;begin match getStockQuote "GOOG" with |None -> () |Some(x) -> insertStockQuote x end*)
;;gatherStockData(5)
(*;;let readme,writeme = Unix.open_process "/Users/adam/Downloads/mongodb-osx-x86_64-2.0.6/bin/mongo"*)
(*;;let (a,b) = stockIsPresentWithChannels "AAPL" NASDAQ readme writeme*)
(*;;(input_line readme)                                                                              *)
(*;;(input_line readme)                                                                              *)
(*;;begin match getStockQuote "AAPL" with                                                            *)
(*				| None -> ()                                                                               *)
(*				| Some(x) -> print_endline "Inserting Stock"; insertStockQuoteWithChannels x readme writeme*)
(*			end                                                                                          *)
(*;;begin match stockQuoteJSONtoStockQuoteOption "{           *)
(*	\"Ticker\" : \"GOOG\",                                    *)
(*	\"Exchange\" : \"NASDAQ\",                                *)
(*	\"Price\" : \"727.5\",                                    *)
(*	\"Change\" : \"9.22\",                                    *)
(*	\"Change_Percent\" : \"1.28\",                            *)
(*	\"Last_Trade_time\" : \"ISODate('2012-09-19T16:00:00Z')\",*)
(*	\"Dividend\" : \"None\",                                  *)
(*	\"Yield\" : \"None\"                                      *)
(*}" with                                                     *)
(*	| None -> print_endline "Not Present"                     *)
(*	| Some(x) -> print_endline "Present"                      *)
(*end                                                         *)
