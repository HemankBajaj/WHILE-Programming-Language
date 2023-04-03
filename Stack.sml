signature STACK =
sig
  type 'a Stack
  exception EmptyStack
  exception Error of string
  val create : 'a Stack 
  val push : 'a * 'a Stack -> 'a Stack 
  val pop : 'a Stack -> 'a Stack 
  val top : 'a Stack -> 'a 
  val empty : 'a Stack -> bool
  val poptop : 'a Stack -> ('a * 'a Stack ) option 
  val nth : 'a Stack * int -> 'a 
  val drop : 'a Stack * int -> 'a Stack 
  val depth : 'a Stack -> int 
  val app : ('a -> unit) -> 'a Stack -> unit 
  val map : ('a -> 'b) -> 'a Stack -> 'b Stack 
  val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
  val find : ('a -> bool) -> 'a Stack -> 'a option 
  val filter : ('a -> bool) -> 'a Stack -> 'a Stack  
  val foldr :  ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val exists : ('a -> bool) -> 'a Stack -> bool
  val all : ('a -> bool) -> 'a Stack -> bool
  val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
  val stack2list :  'a Stack -> 'a list (* Convert a stack into a list *)
  val toString : ('a -> string) -> 'a Stack -> string
end

 structure FunStack :> STACK = 
    struct
        type 'a Stack = 'a list
        exception EmptyStack
        exception Error of string

        val create : 'a Stack = []
        fun push (x : 'a , stk : 'a Stack) : 'a Stack = x::stk 
        fun pop (stk : 'a Stack) : 'a Stack = 
            if (length(stk) > 0) then  
                tl(stk) 
            else 
                raise EmptyStack 
        fun top (stk : 'a Stack) : 'a = 
            if (length(stk) > 0) then  
                hd(stk) 
            else 
                raise EmptyStack 
        fun empty(stk : 'a Stack) : bool = 
            if (length(stk) = 0) then 
                true 
            else 
                false 
        fun poptop(stk : 'a Stack) : ('a * 'a Stack) option  = 
            if (length(stk) > 0) then 
                SOME(hd(stk), tl(stk))
            else 
                raise EmptyStack
        fun nth(stk : 'a Stack , n: int) : 'a  = 
            if (n < 0 orelse n >= length(stk)) then 
                raise Error("Index out of Bounds")
            else 
                List.nth(stk , n)
        fun depth(stk : 'a Stack) : int = length(stk)
        fun drop(stk : 'a Stack , n: int) : 'a Stack = 
            if (n < 0 orelse n >= length(stk)) then 
                raise Error("Index out of Bounds")
            else 
                List.drop(stk , n);
        fun app func stk  = List.app func stk 
        fun map func   stk  =  List.map func stk
        fun mapPartial func stk  = List.mapPartial func stk 
        fun find func stk  = List.find func stk
        fun filter func  stk  = List.filter func stk
        fun foldl func init  stk  = List.foldl func init stk
        fun foldr func init  stk  = List.foldr func  init  stk 
        fun exists func  stk = List.exists func stk 
        fun all func  stk = List.all func  stk 
        fun list2stack(A : 'a list) : 'a Stack = 
            if (length(A) = 0) then 
                create 
            else 
                push(hd(A), list2stack(tl(A))) 
        fun stack2list(stk : 'a Stack) : 'a list = 
            if (length(stk) = 0) then 
                []
            else 
                hd(stk) :: stack2list(tl(stk))
        fun toString func  stk  = 
            if (length(stk) = 0) then 
                ""
            else if (length(stk) = 1) then 
                func(hd(stk))
            else 
                let 
                    val x=hd(stk)
                    val y=tl(stk)
                in
                    func(x)^" "^(toString func y)
                end
    end