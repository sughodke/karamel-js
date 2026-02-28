import {
  JS1_size, JS1_get, JS1_set, JS1_max32, JS1_sum
} from './JS1.js';

let pass = 0, fail = 0;
function assert(cond, msg) {
  if (cond) { pass++; }
  else { fail++; console.error("FAIL:", msg); }
}

// Basic buffer read/write
{
  const buf = new Uint32Array(8);
  JS1_set(buf, 0, 42);
  JS1_set(buf, 3, 7);
  assert(JS1_get(buf, 0) === 42, "get after set [0]");
  assert(JS1_get(buf, 3) === 7, "get after set [3]");
  assert(JS1_get(buf, 1) === 0, "unset index is 0");
}

// max32
{
  assert(JS1_max32(5, 3) === 5, "max32(5,3) = 5");
  assert(JS1_max32(3, 5) === 5, "max32(3,5) = 5");
  assert(JS1_max32(4, 4) === 4, "max32(4,4) = 4");
}

// sum over buffer
{
  const buf = new Uint32Array(8);
  assert(JS1_sum(buf) === 0, "sum of zeros");
  JS1_set(buf, 0, 10);
  JS1_set(buf, 1, 20);
  JS1_set(buf, 7, 5);
  assert(JS1_sum(buf) === 35, "sum = 10+20+5");
}

// size constant
{
  assert(JS1_size === 8, "size constant = 8");
}

console.log(`JS1: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
