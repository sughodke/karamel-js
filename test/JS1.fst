module JS1

(** Basic JS backend test: buffers, constants, simple functions *)

module B = LowStar.Buffer
module U32 = FStar.UInt32
module HST = FStar.HyperStack.ST

open FStar.HyperStack.ST
open LowStar.BufferOps

inline_for_extraction
let size : U32.t = 8ul

type counter = B.lbuffer U32.t (U32.v size)

val get: c:counter -> i:U32.t{U32.v i < U32.v size} -> Stack U32.t
  (requires fun h -> B.live h c)
  (ensures fun h0 r h1 -> h0 == h1 /\ r == B.get h0 c (U32.v i))
let get c i = c.(i)

val set: c:counter -> i:U32.t{U32.v i < U32.v size} -> v:U32.t -> Stack unit
  (requires fun h -> B.live h c)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer c) h0 h1 /\
    B.get h1 c (U32.v i) == v)
let set c i v = c.(i) <- v

inline_for_extraction
let max32 (a b: U32.t) : Tot U32.t =
  if U32.gte a b then a else b

val sum_loop: c:counter -> acc:U32.t -> i:U32.t{U32.v i <= U32.v size} -> Stack U32.t
  (requires fun h -> B.live h c)
  (ensures fun h0 _ h1 -> h0 == h1)
  (decreases (U32.v size - U32.v i))
let rec sum_loop c acc i =
  if U32.lt i size then
    let v = c.(i) in
    sum_loop c (U32.add_mod acc v) (U32.add i 1ul)
  else
    acc

val sum: c:counter -> Stack U32.t
  (requires fun h -> B.live h c)
  (ensures fun h0 _ h1 -> h0 == h1)
let sum c = sum_loop c 0ul 0ul
