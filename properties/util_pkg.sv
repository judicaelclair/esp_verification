// Copyright (c) 2022 Judicael S. E. Clair

package util_pkg;

import env_pkg::*;

// Derived from the 'create_header()' utility function defined later in this file.
function noc_preamble_type get_preamble(input noc_flit_type s);
  return s[noc_flit_size - 1 : noc_flit_size - preamble_width];
endfunction

// Derived from the 'create_header()' utility function defined later in this file.
function noc_preamble_type misc_get_preamble(input misc_noc_flit_type s);
  return s[misc_noc_flit_size - 1 : misc_noc_flit_size - preamble_width];
endfunction

// Derived from the 'create_header()' utility function defined later in this file.
function noc_msg_type get_msg_type(input noc_flit_type s);
  return s[noc_flit_size - preamble_width - 4*yx_width - 1 :
           noc_flit_size - preamble_width - 4*yx_width - msg_type_width];
endfunction

// Derived from the 'create_header()' utility function defined later in this file.
function noc_msg_type misc_get_msg_type(input misc_noc_flit_type s);
  return s[misc_noc_flit_size - preamble_width - 4*yx_width - 1 :
           misc_noc_flit_size - preamble_width - 4*yx_width - msg_type_width];
endfunction

// Derived from the 'create_header()' utility function defined later in this file.
function noc_msg_type get_reserved(input noc_flit_type s);
  return s[noc_flit_size - preamble_width - 4*yx_width - msg_type_width - 1 :
           noc_flit_size - preamble_width - 4*yx_width - msg_type_width - reserved_width];
endfunction

// Derived from the 'create_header()' utility function defined later in this file.
function noc_msg_type misc_get_reserved(input misc_noc_flit_type s);
  return s[misc_noc_flit_size - preamble_width - 4*yx_width - msg_type_width - 1 :
           misc_noc_flit_size - preamble_width - 4*yx_width - msg_type_width - reserved_width];
endfunction

// SystemVerilog version of ESP's VHDL 'create_header()' helper function.
// Note, we cannot parameterise functions by wrapping them around with
// a class as the tool goes completely haywire with warnings and
// subsequently produces bogus results (at least on the functions
// we've tried, notably this one).
function logic [noc_flit_size-1:0] create_header(
    input local_yx local_y,
    input local_yx local_x,
    input local_yx remote_y,
    input local_yx remote_x,
    input noc_msg_type msg_type,
    input reserved_field_type reserved);
  logic [noc_flit_size-1:0] header;
  logic [next_routing_width-1:0] go_left, go_right, go_up, go_down;
  header = 0;
  header[noc_flit_size - 1 :
          noc_flit_size - preamble_width] = preamble_header;
  header[noc_flit_size - preamble_width - 1 :
          noc_flit_size - preamble_width - yx_width] = local_y;
  header[noc_flit_size - preamble_width - yx_width - 1 :
          noc_flit_size - preamble_width - 2*yx_width] = local_x;
  header[noc_flit_size - preamble_width - 2*yx_width - 1 :
          noc_flit_size - preamble_width - 3*yx_width] = remote_y;
  header[noc_flit_size - preamble_width - 3*yx_width - 1 :
          noc_flit_size - preamble_width - 4*yx_width] = remote_x;
  header[noc_flit_size - preamble_width - 4*yx_width - 1 :
          noc_flit_size - preamble_width - 4*yx_width - msg_type_width] = msg_type;
  header[noc_flit_size - preamble_width - 4*yx_width - msg_type_width - 1 :
          noc_flit_size - preamble_width - 4*yx_width - msg_type_width - reserved_width] = reserved;

  if (local_x < remote_x) begin
    go_right = 'b01000;
  end else begin
    go_right = 'b10111;
  end

  if (local_x > remote_x) begin
    go_left = 'b00100;
  end else begin
    go_left = 'b11011;
  end

  if (local_y < remote_y) begin
    header[next_routing_width - 1 : 0] = 'b01110 & go_left & go_right;
  end else begin
    header[next_routing_width - 1 : 0] = 'b01101 & go_left & go_right;
  end

  if (local_y == remote_y && local_x == remote_x) begin
    header[next_routing_width - 1 : 0] = 'b10000;
  end

  return header;
endfunction

// We did not implement 'dynamic_repeat', but was instead found here:
// http://systemverilog.us/vf/SolvingComplexUsersAssertions.pdf
// This helper sequence implements s[*x], where x is a *variable* (x is required to
// be a constant if we use the standard notation). However, most unfortunately, we
// often get the following errors. In particular, it does not work in the context
// of the strong consequent of an assumed property. Even if we replace '$' with a
// constant or use non-zero shifts, it won't work. 
//
// Warning : Assertion compiler error.  This usage of first_match on the right hand
// of an implication or in an asserted sequence is not supported. Please rewrite or
// simplify the assertion..  [ac-1]
//
// Warning : Assertion compiler error.  This usage of ##0 on the right hand of the
// implication is not supported. Please rewrite or simplify the assertion.. [ac-1]
sequence dynamic_repeat(s, x);  
    int v=x;  
    (1, v=x) ##0 first_match((s, v=v-1'b1) [*1:$] ##0 v<=0);  
endsequence

// This implements s[->x], where x is a *variable*. This is our own custom modification
// of 'dynamic_repeat' (see above). Naturally, the same kind of errors that arise when
// using 'dynamic_repeat' also occur with this sequence.
sequence dynamic_non_consecutive_repeat(s, x);  
    int v=x;  
    (1, v=x) ##0 first_match((1, v=v-s) [*1:$] ##0 v<=0);  
endsequence

// This is our own custom simplified implementation of 'dynamic_repeat'. Note, we are
// are assuming the variable length is bounded (here the bound is set to 2). Also, we
// use '##1' as '##0' was unsupported by the tool. Regardless, it produces an error
// stating clearly that we can't have variable length repeat:
//
// Warning : Unsupported variable delay between local variable assign and read or
// within AND/OR/first_match/within constructs.  Outside supported local variable
// synthesis subset. [ac-17]
sequence simple_dynamic_repeat(s, x);  
    int v=x;
    (1, v=x) ##1 (s, v=v-1'b1) [*0:2] ##1 v==0;
    // WARNING: don't increase '[*0:2]' to something like '[*0:10000]' as your computer will crash.
endsequence

// This is our own custom simplified implementation of 'dynamic_non_consecutive_repeat'.
// Note, we are are assuming the variable length is bounded (here the bound is set to 2).
// Also, we use '##1' as '##0' was unsupported by the tool. However, we get the same
// errors as for 'simple_dynamic_repeat'.
sequence simple_dynamic_non_consecutive_repeat(s, x);  
    int v;
    (1, v=x) ##1 (1, v=v-s) [*0:2] ##1 v==0;
    // WARNING: don't increase '[*0:2]' to something like '[*0:10000]' as your computer will crash.
endsequence

endpackage : util_pkg