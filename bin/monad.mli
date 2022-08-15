module type Monad = sig
  type 'a m

  val return : 'a -> 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
end
