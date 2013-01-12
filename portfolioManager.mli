open Stock;;

type portfolio = {
	stocks : stockQuote list;
	subportfolios: portfolio list option;
}