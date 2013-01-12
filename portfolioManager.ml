open Unix;;
open Stock;;
open IndividualStockTracker;;

type portfolio = {
	stocks : stockQuote list;
	subportfolios: portfolio list option;
}

let trackPortfolio (p:portfolio) (interval:int) : unit = 
	let stockfun stockQuote =
			begin match getStockQuote stockQuote.stock.ticker with
				| None -> ()
				| Some(x) -> insertStockQuote x
			end in
	let rec mainloop list =
		List.iter stockfun list;
		Unix.sleep interval;
		mainloop list;
		in
	let rec runloop list = 
		try mainloop list
		with Unix_error(a,b,c) -> print_endline "********************UNIX ERROR********************"; Unix.sleep 20; runloop list
		in
	runloop p.stocks
	

;;let p = begin match (getStockQuote "AAPL",getStockQuote "GOOG") with
	| Some(x),Some(y) -> Some({stocks = [x;y];subportfolios = None})
	| _ -> None
end
;;begin match p with
	| Some(x) -> trackPortfolio x 4
	| None -> ()
end