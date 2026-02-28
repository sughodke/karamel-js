module JS3

(** JS backend test: uint8 arrays, boolean logic, bitwise-style ops *)

module B = LowStar.Buffer
module U8 = FStar.UInt8
module U32 = FStar.UInt32
module HST = FStar.HyperStack.ST

open FStar.HyperStack.ST
open LowStar.BufferOps

inline_for_extraction
let capacity : U32.t = 64ul

type bitset = B.lbuffer U8.t (U32.v capacity)

val test_bit: s:bitset -> i:U32.t{U32.v i < U32.v capacity} -> Stack bool
  (requires fun h -> B.live h s)
  (ensures fun h0 r h1 -> h0 == h1 /\ r == (U8.v (B.get h0 s (U32.v i)) <> 0))
let test_bit s i =
  s.(i) <> 0uy

val set_bit: s:bitset -> i:U32.t{U32.v i < U32.v capacity} -> Stack unit
  (requires fun h -> B.live h s)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer s) h0 h1 /\
    B.get h1 s (U32.v i) == 1uy)
let set_bit s i =
  s.(i) <- 1uy

val clear_bit: s:bitset -> i:U32.t{U32.v i < U32.v capacity} -> Stack unit
  (requires fun h -> B.live h s)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer s) h0 h1 /\
    B.get h1 s (U32.v i) == 0uy)
let clear_bit s i =
  s.(i) <- 0uy

inline_for_extraction
let bor (a b: U8.t) : Tot U8.t =
  if a <> 0uy || b <> 0uy then 1uy else 0uy

val union_loop: dst:bitset -> src:bitset -> i:U32.t{U32.v i <= U32.v capacity} -> Stack unit
  (requires fun h -> B.live h dst /\ B.live h src /\ B.disjoint dst src)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer dst) h0 h1 /\ B.live h1 dst)
  (decreases (U32.v capacity - U32.v i))
let rec union_loop dst src i =
  if U32.lt i capacity then begin
    let d = dst.(i) in
    let s = src.(i) in
    dst.(i) <- bor d s;
    union_loop dst src (U32.add i 1ul)
  end

val union: dst:bitset -> src:bitset -> Stack unit
  (requires fun h -> B.live h dst /\ B.live h src /\ B.disjoint dst src)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer dst) h0 h1 /\ B.live h1 dst)
let union dst src = union_loop dst src 0ul

val count_loop: s:bitset -> acc:U32.t -> i:U32.t{U32.v i <= U32.v capacity} -> Stack U32.t
  (requires fun h -> B.live h s)
  (ensures fun h0 _ h1 -> h0 == h1)
  (decreases (U32.v capacity - U32.v i))
let rec count_loop s acc i =
  if U32.lt i capacity then
    let v = s.(i) in
    let acc' = if v <> 0uy then U32.add_mod acc 1ul else acc in
    count_loop s acc' (U32.add i 1ul)
  else
    acc

val popcount: s:bitset -> Stack U32.t
  (requires fun h -> B.live h s)
  (ensures fun h0 _ h1 -> h0 == h1)
let popcount s = count_loop s 0ul 0ul
