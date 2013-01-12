open Http_client.Convenience;;
open Yojson;;
open Yojson.Basic.Util;;
open Printf;;
open Netdate;;

let currentYear = "2012"

type exchange = NASDAQ | NYSE | CME | INDEXSP | OTHER

type stock = {
	ticker : string;
	exchange : exchange;
}

type stockQuote = {
	stock : stock;
	price : float;
	change : float;
	change_percent : float;
	last_trade_time : string;
	last_trade_time_object: Netdate.t;
	dividend : float option;
	yield : float option;
}

let parseDateString (date:string) : Netdate.t = 
	let r = Str.regexp "," in
	let num = Str.search_forward r date 0 in
	let parsestring = (String.concat "" [Str.string_before date (num+2);currentYear;Str.string_after date (num+1)]) in
	Netdate.parse parsestring

(*let unixFloatFromString (s:string) : float =                                                                                          *)
(*	let r = Str.regexp "\\([A-Z][a-z]+\\) \\([0-9][0-9]?\\), \\([0-9][0-9]?\\):\\([0-9][0-9]?\\)\\([A-Z][a-z]+\\)" in                   *)
(*	let monthnum =                                                                                                                      *)
(*		begin match (Str.matched_group 1 s) with                                                                                          *)
(*			| "Jan" -> 1                                                                                                                    *)
(*			| "Feb" -> 2                                                                                                                    *)
(*			| "Mar" -> 3                                                                                                                    *)
(*			| "Apr" -> 4                                                                                                                    *)
(*			| "May" -> 5                                                                                                                    *)
(*			| "Jun" -> 6                                                                                                                    *)
(*			| "Jul" -> 7                                                                                                                    *)
(*			| "Aug" -> 8                                                                                                                    *)
(*			| "Sep" -> 9                                                                                                                    *)
(*			| "Oct" -> 10                                                                                                                   *)
(*			| "Nov" -> 11                                                                                                                   *)
(*			| "Dec" -> 12                                                                                                                   *)
(*			| _ -> -1                                                                                                                       *)
(*		end in                                                                                                                            *)
(*	let time = Unix.mktime {Unix.tm_sec = 0; tm_min = int_of_string (Str.matched_group 4 s);                                            *)
(*	tm_hour = int_of_string (Str.matched_group 3 s); tm_mday = int_of_string (Str.matched_group 2 s); tm_mon = monthnum; tm_year = 112; *)
(*	tm_wday = -1; tm_yday = -1; tm_isdst = false;} in                                                                                   *)
(*	(fst time)                                                                                                                          *)

let getquotejson (ticker: string) : Yojson.Safe.json option = 
	let query = String.concat "" ["http://www.google.com/finance/info?client=ig&q=";ticker] in
	let pagetext =
		try
			Str.string_after (http_get query) 3
		with _ -> 
			"" in
		let json =
			try Some(Safe.from_string pagetext)
			with Yojson.Json_error(msg) -> None in
		json
		
let getStockQuote (ticker: string) : stockQuote option =
	let json = getquotejson ticker in
	begin match json with
		| None -> None
		| Some(x) -> 
			let basic = Yojson.Basic.Util.index 0 (Yojson.Safe.to_basic x) in
			let e = basic |> member "e" |> to_string in
			let exc =
				begin match e with
					| "NASDAQ" -> NASDAQ
					| "NYSE" -> NYSE
					| "CME" -> CME
					| "INDEXSP" -> INDEXSP
					| _ -> OTHER
				end in
			begin match (exc != OTHER) with
				| true ->
					let commareg = Str.regexp "\\(.*\\),\\(.*\\)" in 
					let t = basic |> member "t" |> Yojson.Basic.Util.to_string in
					let stock = {ticker = t;exchange = exc} in
					let price = float_of_string (Str.replace_first commareg "\\1\\2" (basic |> member "l_cur" |> Yojson.Basic.Util.to_string)) in
					let change = float_of_string (basic |> member "c" |> Yojson.Basic.Util.to_string) in
					let changepercent = float_of_string (basic |> member "cp" |> Yojson.Basic.Util.to_string) in
					let dividend = 
						try Some(float_of_string (Str.replace_first commareg "\\1\\2" (basic |> member "div" |> Yojson.Basic.Util.to_string))) 
						with _ -> None in
					let lasttradetime = basic |> member "lt" |> Yojson.Basic.Util.to_string in
					let yield = 
						try Some(float_of_string (basic |> member "yld" |> Yojson.Basic.Util.to_string))
						with _ -> None in
					let timeobject = parseDateString lasttradetime in
					let timeobject2 = Netdate.create (Unix.time ()) in
					Some({stock = stock;price = price;change = change;change_percent = changepercent;last_trade_time = Netdate.format "%b %e %Y, %l:%M%p %z" timeobject2;last_trade_time_object = timeobject2; dividend = dividend;yield = yield})
				| false -> None
			end
end
	
(*;;print_endline (begin match getquotejson "AAPL" with                                       *)
(*	| None -> "None"                                                                          *)
(*	| Some(x) -> Yojson.Basic.Util.index 0 (Yojson.Safe.to_basic x) |> member "e" |> to_string*)
(*end)                                                                                        *)
	
let printJSONoption (j:Yojson.Safe.json option) : unit =
	begin match j with
		| None -> print_endline "None"
		| Some(x) -> print_endline (Yojson.Safe.to_string x)
	end

let printStock (s:stock) : unit =
	print_endline (String.concat "" ["Ticker: "; s.ticker]);
	let e =
		begin match s.exchange with
			| NASDAQ -> "NASDAQ"
			| NYSE -> "NYSE"
			| CME -> "CME"
			| INDEXSP -> "INDEXSP"
			| OTHER ->"Other"
		end in
	print_endline (String.concat "" ["Exchange: "; e])

let printStockQuoteOption (j:stockQuote option) : unit =
	begin match j with
		| None -> print_endline "None"
		| Some(x) ->
			printStock x.stock;
			print_endline (String.concat "" ["Price: "; string_of_float x.price]);
			print_endline (String.concat "" ["Change: ";string_of_float x.change]);
			print_endline (String.concat "" ["Change Percent: ";string_of_float x.change_percent]);
			print_endline (String.concat "" ["Last Trade Time: ";x.last_trade_time]);
			print_endline (String.concat "" ["Dividend: ";(begin match x.dividend with |None -> "None" |Some(x) -> string_of_float x end)]);
			print_endline (String.concat "" ["Yield: ";(begin match x.yield with |None -> "None" |Some(x) -> string_of_float x end)])
	end


                                                                                    

