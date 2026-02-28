module JS2

(** JS backend test: structs, field access, cross-module calls *)

module B = LowStar.Buffer
module U32 = FStar.UInt32
module U8 = FStar.UInt8
module HST = FStar.HyperStack.ST

open FStar.HyperStack.ST
open LowStar.BufferOps

(** A struct with two buffer fields — tests struct-as-object generation *)
noeq type pair_buf = {
  left: B.lbuffer U32.t 4;
  right: B.lbuffer U32.t 4;
}

(** Read from struct field buffer *)
val get_left: p:pair_buf -> i:U32.t{U32.v i < 4} -> Stack U32.t
  (requires fun h -> B.live h p.left)
  (ensures fun h0 r h1 -> h0 == h1 /\ r == B.get h0 p.left (U32.v i))
let get_left p i = p.left.(i)

val get_right: p:pair_buf -> i:U32.t{U32.v i < 4} -> Stack U32.t
  (requires fun h -> B.live h p.right)
  (ensures fun h0 r h1 -> h0 == h1 /\ r == B.get h0 p.right (U32.v i))
let get_right p i = p.right.(i)

(** Swap values at index i between left and right *)
val swap_at: p:pair_buf -> i:U32.t{U32.v i < 4} -> Stack unit
  (requires fun h -> B.live h p.left /\ B.live h p.right /\ B.disjoint p.left p.right)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_union (B.loc_buffer p.left) (B.loc_buffer p.right)) h0 h1 /\
    B.get h1 p.left (U32.v i) == B.get h0 p.right (U32.v i) /\
    B.get h1 p.right (U32.v i) == B.get h0 p.left (U32.v i))
let swap_at p i =
  let l = p.left.(i) in
  let r = p.right.(i) in
  p.left.(i) <- r;
  p.right.(i) <- l

(** Merge right into left using max, tests cross-field iteration *)
val merge_loop: p:pair_buf -> i:U32.t{U32.v i <= 4} -> Stack unit
  (requires fun h -> B.live h p.left /\ B.live h p.right /\ B.disjoint p.left p.right)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer p.left) h0 h1 /\
    B.live h1 p.left)
  (decreases (4 - U32.v i))
let rec merge_loop p i =
  if U32.lt i 4ul then begin
    let l = p.left.(i) in
    let r = p.right.(i) in
    (if U32.lt l r then p.left.(i) <- r);
    merge_loop p (U32.add i 1ul)
  end

val merge: p:pair_buf -> Stack unit
  (requires fun h -> B.live h p.left /\ B.live h p.right /\ B.disjoint p.left p.right)
  (ensures fun h0 _ h1 ->
    B.modifies (B.loc_buffer p.left) h0 h1 /\
    B.live h1 p.left)
let merge p = merge_loop p 0ul
