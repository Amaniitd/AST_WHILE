signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create : 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a stack into a list *)
end

structure FunStack : STACK =
struct 
    exception EmptyStack
    exception Error of string
    type 'a Stack = 'a list
    val create = [];
    fun push(a, st) = List.concat[[a], st];
    fun pop(hd::tl) = tl;
    fun top(hd::tl) = hd;
    fun empty(st) = List.null(st);
    fun poptop(hd::tl) = SOME(hd, tl);
    fun nth(st, n) = List.nth(st, n);
    fun drop(st, n) = List.drop(st, n);
    fun depth(st) = List.length(st);
    fun app f st = (app f st);
    fun map f st = (map f st);
    fun mapPartial f st = (mapPartial f st);
    fun find f st = (find f st);
    fun filter f st = (filter f st);
    fun foldr f a st = (foldr f a st);
    fun foldl f a st = (foldl f a st);
    fun exists f st = (exists f st);
    fun all f st = (all f st);
    fun list2stack(l) = l;
    fun stack2list(st) = st; 
end



