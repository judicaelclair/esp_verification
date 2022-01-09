// Copyright (c) 2022 Judicael S. E. Clair

import env_pkg::*;
import util_pkg::*;

bind axislv2noc
axislv2noc_sva #(.nmst(nmst),
                 .mem_num(mem_num))
bind_axislv2noc (.*,
                 .mosi(mosi_extended),
                 .somi(somi_extended),
                 .coherence_empty_low_cnt(fv_util_inst.coherence_empty_low_cnt),
                 .misc_empty_low_cnt(fv_util_inst.misc_empty_low_cnt),
                 .coherence_rdreq_high_cnt(fv_util_inst.coherence_rdreq_high_cnt),
                 .misc_rdreq_high_cnt(fv_util_inst.misc_rdreq_high_cnt),
                 .coherence_empty_low_mosi_r_ready_high_cnt(fv_util_inst.coherence_empty_low_mosi_r_ready_high_cnt),
                 .misc_empty_low_mosi_r_ready_high_cnt(fv_util_inst.misc_empty_low_mosi_r_ready_high_cnt),
                 .coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt(fv_util_inst.coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt),
                 .misc_empty_low_rdreq_high_mosi_r_ready_high_cnt(fv_util_inst.misc_empty_low_rdreq_high_mosi_r_ready_high_cnt),
                 .first_coherence_empty_low_had_mosi_r_ready_high(fv_util_inst.first_coherence_empty_low_had_mosi_r_ready_high),
                 .first_misc_empty_low_had_mosi_r_ready_high(fv_util_inst.first_misc_empty_low_had_mosi_r_ready_high),
                 .read_req_length(fv_util_inst.read_req_length),
                 .transaction_cnt(fv_util_inst.transaction_cnt));

module axislv2noc_sva
  #(
    parameter integer nmst = 1,
    parameter integer mem_num = 1
   )
   (
    // generics
    input integer retarget_for_dma,
    input integer mem_axi_port,
    input tile_mem_info_vector mem_info,
    input local_yx slv_y,
    input local_yx slv_x,
    // ports
    input rst,
    input clk,
    input local_yx local_y,
    input local_yx local_x,
    input axi_mosi_extended_vector mosi,
    input axi_somi_extended_vector somi,
    input coherence_req_wrreq,
    input noc_flit_type coherence_req_data_in,
    input coherence_req_full,
    input coherence_rsp_rcv_rdreq,
    input noc_flit_type coherence_rsp_rcv_data_out,
    input coherence_rsp_rcv_empty,
    input remote_ahbs_snd_wrreq,
    input misc_noc_flit_type remote_ahbs_snd_data_in,
    input remote_ahbs_snd_full,
    input remote_ahbs_rcv_rdreq,
    input misc_noc_flit_type remote_ahbs_rcv_data_out,
    input remote_ahbs_rcv_empty,
    input transaction_type transaction,
    input transaction_type transaction_reg,
    input axi_fsm current_state,
    input axi_fsm next_state,
    input selected,
    input sample_flits,
    input misc_noc_flit_type remote_ahbs_rcv_data_out_hold,
    input sample_and_hold,
    // fv_util signals -> i.e. instrumentation
    input counter_type coherence_empty_low_cnt,
    input counter_type misc_empty_low_cnt,
    input counter_type coherence_rdreq_high_cnt,
    input counter_type misc_rdreq_high_cnt,
    input counter_extended_vector coherence_empty_low_mosi_r_ready_high_cnt,
    input counter_extended_vector misc_empty_low_mosi_r_ready_high_cnt,
    input counter_extended_vector coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt,
    input counter_extended_vector misc_empty_low_rdreq_high_mosi_r_ready_high_cnt,
    input std_ulogic_extended_vector first_coherence_empty_low_had_mosi_r_ready_high,
    input std_ulogic_extended_vector first_misc_empty_low_had_mosi_r_ready_high,
    input [7:0] read_req_length,
    input counter_type transaction_cnt
);

/* sample properties */

generate
  if (`ENABLE_FIRED == 0) begin : sample_property
    ap_sample_1: assert property (@(posedge clk) disable iff(~rst) mosi[0].aw.valid |-> transaction.write);

    ap_sample_2: assert property (@(posedge clk) disable iff(~rst) (mem_axi_port == -1 || mem_axi_port == transaction.xindex) |-> transaction.dst_is_mem);

    ap_sample_3: assert property (
      @(posedge clk) disable iff(~rst)
      remote_ahbs_snd_full |-> (~remote_ahbs_snd_wrreq && ($countones(remote_ahbs_snd_data_in) == 0))
    );

    property ap_sample_4_;
      logic [axidw-1:0] random_data;
      @(posedge clk) disable iff(~rst)
      ((
      mosi[0].ar == '{default:'0, size: 3'b101, valid: 1'b1} &&
      mosi[0].aw.valid == 1'b0 &&
      mosi[1].ar.valid == 1'b0 &&
      mosi[1].aw.valid == 1'b0,
      random_data = $urandom()
      )
      |->
      (
      selected and
      ((current_state == idle) |->
        ((sample_flits && next_state == request_header) ##1
        ((current_state == request_header &&
          (next_state == request_header ||
          next_state == request_address))
          )
        )
      )
      ))
    endproperty : ap_sample_4_
    ap_sample_4: assert property (ap_sample_4_());
  end : sample_property
endgenerate

/* constrain constant inputs */

ap_retarget_for_dma_bounds: assume property (@(posedge clk) (retarget_for_dma == 1'b0 || retarget_for_dma == 1'b1));
ap_mem_axi_port_bounds: assume property (@(posedge clk) (mem_axi_port >= -1 && mem_axi_port <= (nahbslv - 1)));
ap_mem_num_bounds: assume property (@(posedge clk) (mem_num >= 1 && mem_num <= (cfg_nmem_tile + cfg_nslm_tile + cfg_nslmddr_tile)));

ap_stable_generic_param_1: assume property (@(posedge clk) ($stable(slv_y)));
ap_stable_generic_param_2: assume property (@(posedge clk) ($stable(slv_x)));
ap_stable_generic_param_3: assume property (@(posedge clk) ($stable(retarget_for_dma)));
ap_stable_generic_param_4: assume property (@(posedge clk) ($stable(mem_axi_port)));
ap_stable_generic_param_5: assume property (@(posedge clk) ($stable(mem_info)));

// local_y & local_x are effectively constant even though they are not generic parameters.
ap_stable_port_1: assume property (@(posedge clk) ($stable(local_y)));
ap_stable_port_2: assume property (@(posedge clk) ($stable(local_x)));

/* sanity checks on usage of NoC signals */

property ap_no_write_to_full_noc_1_;
  @(posedge clk) disable iff(~rst)
  coherence_req_full |-> (~coherence_req_data_in && ~coherence_req_wrreq)
endproperty : ap_no_write_to_full_noc_1_

property ap_no_write_to_full_noc_2_;
  @(posedge clk) disable iff(~rst)
  remote_ahbs_snd_full |-> (~remote_ahbs_snd_data_in && ~remote_ahbs_snd_wrreq)
endproperty : ap_no_write_to_full_noc_2_

property ap_no_read_from_empty_noc_1_;
  @(posedge clk) disable iff(~rst)
  coherence_rsp_rcv_empty |-> ~coherence_rsp_rcv_rdreq
endproperty : ap_no_read_from_empty_noc_1_

property ap_no_read_from_empty_noc_2_;
  @(posedge clk) disable iff(~rst)
  remote_ahbs_rcv_empty |-> ~remote_ahbs_rcv_rdreq
endproperty : ap_no_read_from_empty_noc_2_

generate
  if (`ENABLE_FIRED == 0) begin : noc_sanity_check
    ap_no_write_to_full_noc_1 : assert property(ap_no_write_to_full_noc_1_());
    ap_no_write_to_full_noc_2 : assert property(ap_no_write_to_full_noc_2_());
    ap_no_read_from_empty_noc_1 : assert property(ap_no_read_from_empty_noc_1_());
    ap_no_read_from_empty_noc_2 : assert property(ap_no_read_from_empty_noc_2_());
  end : noc_sanity_check
endgenerate

/* modelling the NoC to emulate servicing read requests */

// Assume that the NoC can always eventually service a request.
ap_can_eventually_send_misc_req: assume property (@(posedge clk) (s_eventually ~remote_ahbs_snd_full));
ap_can_eventually_send_coherence_req: assume property (@(posedge clk) (s_eventually ~coherence_req_full));

// The following assumption property models the NoC sending back a response via plane #3
// when the DUT sends a coherence read request to plane #1. However, the property is not 
// maintained so commented out code may not work anymore since there have been lots of
// changes to the RTL code and SVA assertions. Nevertheless, this is kept to illustrate
// the sort of stuff we tried. This property is verified when FAST is 1. This property
// is a simplified version of ap_model_noc_rsp_after_coherence_read_req_:
//
// ap_fast_model_noc_rsp_after_coherence_read_req_ assumes unrealistic behaviour of the NoC.
// Specifically, regardless of the length specified in the memory read request, the NoC's reply
// is a payload of fixed length. The reason is that the tool doesn't support the SVA constructs
// needed to implement variable length. That being said, we have actually figured out how to do
// variable length by modifying the DUT itself. See ap_model_noc_rsp_after_coherence_read_req_.
property ap_fast_model_noc_rsp_after_coherence_read_req_;
  logic [7:0] len;
  @(posedge clk) disable iff(~rst)
  (
    // Specify values of generic parameters; noting that they are
    // guaranteed to always stay the same (assumed stable).
    (retarget_for_dma == 1'b0 && mem_axi_port == -1)

    // Deassert 'coherence_req_wrreq' as this is the
    // expected state prior to sending a packet.
    ##0 ~coherence_req_wrreq

    // Send header into 'coherence_req_data_in' and simultaneously assert 'coherence_req_wrreq'.
    ##1 (coherence_req_wrreq &&
      (coherence_req_data_in[noc_flit_size-1:noc_flit_size-preamble_width-2*yx_width] == 
        create_header(local_y,
          local_x,
          mem_info[transaction_reg.xindex].y,
          mem_info[transaction_reg.xindex].x,
          req_gets_w,
          0)[noc_flit_size-1:noc_flit_size-preamble_width-2*yx_width]) &&
      (coherence_req_data_in[noc_flit_size-preamble_width-4*yx_width-1:noc_flit_size-preamble_width-4*yx_width-msg_type_width] == 
        create_header(local_y,
          local_x,
          mem_info[transaction_reg.xindex].y,
          mem_info[transaction_reg.xindex].x,
          req_gets_w,
          0)[noc_flit_size-preamble_width-4*yx_width-1:noc_flit_size-preamble_width-4*yx_width-msg_type_width]))
    
    // Send address into 'coherence_req_data_in' and simultaneously assert 'coherence_req_wrreq'
    ##1 coherence_req_wrreq[->1] ##0 (get_preamble(coherence_req_data_in) == preamble_body)
    
    // Send length into 'coherence_req_data_in' and simultaneously assert 'coherence_req_wrreq'
    ##1 coherence_req_wrreq[->1] ##0 (get_preamble(coherence_req_data_in) == preamble_tail,
      len = coherence_req_data_in[7:0]) // here we get the length of the payload, 
                                        // which we tried to use in the consequent
                                        // without success as the tool has very
                                        // limited support for local variables
                                        // in this context.
  )
  
  // unlike the 'implies' operator, the '|->' operator evaluates the
  // consequent starting from the final time step of the antecedent.
  |->
  
  // Below is a runnable version of the consequent, but the consequent
  // has to be very simple otherwise the tool will give up.
  strong(    
    // Send header, but details of it are irrelevant so not specified here.
    ##1 (~coherence_rsp_rcv_empty)

    // Send payload data with the final flit having preamble set to
    // 'preamble_tail' to denote that it is the last flit.

    // Here we tried to emulate sending data with length equal to the length specified in the read
    // request. However, the tool doesn't have support for it in this context even with several
    // simplifications to its implementation. To no avail, we tried using the following:
    // - 'dynamic_repeat'
    // - 'dynamic_non_consecutive_repeat'
    // - 'simple_dynamic_repeat'
    // - 'simple_dynamic_non_consecutive_repeat'
    // Note, the implementation of the aforementioned functions can be found in util_pkg.
    //
    // ##1 dynamic_non_consecutive_repeat((~coherence_rsp_rcv_empty && mosi[transaction_reg.xindex].r.ready), len)
    // ##0 (get_preamble(coherence_rsp_rcv_data_out) == preamble_tail)

    // Alternatively, we can just hardcode a set amount of data regardless of the requested amount.
    // Luckily, the DUT does not take into consideration the length, so this is acceptable for any
    // length. Of course, this is far from ideal, but we can't do anything too crazy as '$' is not
    // supported in this context (we tried every operator that syntatically allows '$'). Note also,
    // the goto repetition operator 's[->v]' is not supported in this context and produces the
    // error: "this form of fairness assumption is not supported". Hence, we cannot do:
    //
    // ##1 (~coherence_rsp_rcv_empty && mosi[transaction_reg.xindex].r.ready)[->5]
    // ##0 (get_preamble(coherence_rsp_rcv_data_out) == preamble_tail)

    // Instead, we can use the consecutive repetition operator 's[*v]' as follows,
    //
    // Here we cheat a bit and have all flits have preamble set to 'preamble_tail'.
    // This is so that things works for any payload size less than or equal to the
    // number of repetitions used below. This is a **very** hacky workaround and
    // is just used for this property. Accordingly, assertions can't be very
    // general when this property is assumed as this property does not cover
    // most cases. In fact, this property assumes **incorrect** behaviour of
    // the NoC.
    //
    // In contrast, ap_model_noc_rsp_after_coherence_read_req_
    // doesn't implement any of these hacky workarounds, which is
    // why it takes an extremely long time to prove properties
    // with it. 

    ##1 (~coherence_rsp_rcv_empty &&
         mosi[transaction_reg.xindex].r.ready &&
         get_preamble(coherence_rsp_rcv_data_out) == preamble_tail)[*(`MAX_PAYLOAD_LENGTH)]
  )
endproperty : ap_fast_model_noc_rsp_after_coherence_read_req_

sequence antecedent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req;
  (
    retarget_for_dma == 1'b0 && // stable signal
    mem_axi_port == -1 &&       // stable signal
    ((`ENABLE_FIRED == 0)
      ? (mosi[0].ar.len <= (`MAX_PAYLOAD_LENGTH-1))
      : (mosi[0].ar.len >= `MAX_PAYLOAD_LENGTH)
    ) &&
    (mosi[0].ar.size == 3'b011) &&  
    (mosi[0].ar.valid == 1'b1) &&  
    (0 == mosi[0].ar.id) &&
    (0 == mosi[0].ar.addr) && 
    (0 == mosi[0].ar.burst) &&  
    (0 == mosi[0].ar.lock) &&   
    (0 == mosi[0].ar.cache) &&
    (0 == mosi[0].ar.prot) && 
    (0 == mosi[0].ar.qos) &&
    (0 == mosi[0].ar.region) &&
    (0 == mosi[0].ar.user) &&
    mosi[0].aw == 0 &&
    mosi[0].b == 0 &&
    mosi[1] == 0 &&
    current_state == idle
  );
endsequence : antecedent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req

sequence consequent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req;
  (
    ##1   current_state == request_header
    ##[+] current_state == request_address
    ##[+] current_state == request_length
    ##[+] current_state == reply_header
    ##[+] current_state == reply_data
    ##1   (current_state != reply_data &&
           current_state != fv_bad_state)
  );
endsequence : consequent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req

property ap_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_;
  @(posedge clk) disable iff(~rst)
  (antecedent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req()
  |->
  strong(consequent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req()))
endproperty : ap_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_

property cp_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_;
  @(posedge clk) disable iff(~rst)
  strong(
    antecedent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req()
    ##0
    consequent_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req()
  )
endproperty : cp_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_

// Here we assume the highly constrained property that models the NoC sending back
// a response via plane #3 when the DUT sends a coherence read request to plane #1.
// We also assert and cover highly constrained properties that demonstrate the model
// working as expected.
generate
  if (`FAST == 1) begin : swift_like_the_wind
    ap_fast_model_noc_rsp_after_coherence_read_req : assume property(ap_fast_model_noc_rsp_after_coherence_read_req_());
    ap_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req : assert property(ap_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_());
    cp_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req : cover property(cp_demonstrate_correctness_of_fast_model_noc_rsp_after_coherence_read_req_());
  end : swift_like_the_wind
endgenerate

// The following assumption property models the NoC sending back a response
// via plane #3 when the DUT sends a coherence read request to plane #1. 
// This property is as general as possible. The only thing it assumes is
// an upper bound to when a reply should be received. Furthermore, this
// property **correctly** models the entire behaviour of the NoC when
// responding to a coherence read request.
property ap_model_noc_rsp_after_coherence_read_req_(integer xindex = 0,
                                                    integer max_header_delay = 2,
                                                    integer max_payload_delay = 10);
  @(posedge clk) disable iff(~rst)
  (
    // Require 'coherence_req_wrreq' to be
    // deasserted as a packet is never sent
    // to the NoC immediately after sending
    // another one. There is always at least
    // a clock cycle delay in-between.
    ##0 ~coherence_req_wrreq

    // Specify values of generic parameters so this assumption
    // is only enabled when needed; noting that the generic 
    // parameters are guaranteed to always stay the same
    // (assumed stable). This avoids assumptions from
    // conflicting with each other.
    ##1 (mem_axi_port == -1 || mem_axi_port == transaction_reg.xindex)

    // The assumption is instantiated multiple times
    // with different values for 'xindex', which is
    // the index of the AXI master that this assumption
    // considers. The reason there must be an assumption
    // for each master is because each master has its own
    // set of signals that are used in the consequent.
    // Accordingly, this assumption is only enabled if
    // the master that is sending the request is the
    // same as the master that this assumption handles.
    ##0 transaction_reg.xindex == xindex

    // Header flit is sent into 'coherence_req_data_in' and
    // 'coherence_req_wrreq' is simultaneously asserted. We
    // also require that the header is constructed correctly.
    ##0 (
      coherence_req_wrreq &&
      get_preamble(coherence_req_data_in) == preamble_header &&
      (~retarget_for_dma || get_msg_type(coherence_req_data_in) == dma_to_dev)
      &&
      (retarget_for_dma ||
      (get_msg_type(coherence_req_data_in) == req_gets_b ||
        get_msg_type(coherence_req_data_in) == req_gets_hw ||
        get_msg_type(coherence_req_data_in) == req_gets_w))
    )
    
    // Flit specifying the memory address of the data we
    // want to read is sent into 'coherence_req_data_in' 
    // and 'coherence_req_wrreq' is simultaneously asserted.
    ##1 coherence_req_wrreq[->1]
    ##0 get_preamble(coherence_req_data_in) == preamble_body
    
    // Flit specifying the amount of data we want to read
    // (i.e. payload length) is sent into 'coherence_req_data_in'
    // and 'coherence_req_wrreq' is simultaneously asserted.
    ##1 coherence_req_wrreq[->1]
    ##0 get_preamble(coherence_req_data_in) == preamble_tail

    // Lastly, the payload length must be no more
    // than what this property can assume. Without
    // the following check, properties that use a
    // length greater or equal to max_payload_delay
    // would be uncoverable but proven. In contrast,
    // with this check, they are covered and fire
    // as desired.
    ##0 coherence_req_data_in[7:0] <= `MAX_PAYLOAD_LENGTH
  )

  |->

  strong(
  (
    // Only the coherence plane is used throughout this transaction.
    misc_empty_low_cnt == 0
  )
  throughout
  (
    // Ensure that no data has yet been received from the NoC
    // since the start of the transaction (the counters are reset
    // from within the DUT at the beginning of every transaction).
    ##1 coherence_empty_low_cnt == 0

    // Within max_header_delay (predefined) clock cycles,
    // the DUT must have received the header flit.
    // Note, it is **critical** that coherence_empty_low_cnt
    // is checked at every clock cycle, not just at the end.
    ##0 (sample_flits == 1'b0 &&
         get_preamble(coherence_rsp_rcv_data_out) == preamble_header &&
         coherence_empty_low_cnt == 0)
        [*1:max_header_delay]
    ##1 coherence_empty_low_cnt == 1

    // Note, we could use transaction_reg.len instead of read_req_length but
    // the use of read_req_length ensures that the read request was actually
    // correct. The reason is that read_req_length is assigned based only on
    // the values of the NoC signals.
    
    // Within max_payload_delay (predefined) clock cycles, the DUT must
    // have received exactly payload_length flits (excluding the header flit).
    // Note, it is **critical** that coherence_empty_low_mosi_r_ready_high_cnt
    // is checked at every clock cycle, not just at the end.
    ##0 (sample_flits == 1'b0 &&
         get_preamble(coherence_rsp_rcv_data_out) == preamble_body &&
         coherence_empty_low_mosi_r_ready_high_cnt[xindex] < (read_req_length+first_coherence_empty_low_had_mosi_r_ready_high[xindex]))
        [*0:max_payload_delay-1]
    
    // Ensure the correct number of flits have been received & acknowledged.
    ##1 coherence_empty_low_mosi_r_ready_high_cnt[xindex] == (read_req_length+first_coherence_empty_low_had_mosi_r_ready_high[xindex]-1)

    // Final payload flit must have its preamble set to 'preamble_tail',
    // which denotes that it is the last flit of the packet.
    ##0 (~coherence_rsp_rcv_empty &&
         mosi[xindex].r.ready &&
         get_preamble(coherence_rsp_rcv_data_out) == preamble_tail)

    // Ensure the correct number of flits have been received & acknowledged.
    ##1 coherence_empty_low_mosi_r_ready_high_cnt[xindex] == (read_req_length+first_coherence_empty_low_had_mosi_r_ready_high[xindex])
  ))
endproperty : ap_model_noc_rsp_after_coherence_read_req_

// Here we assume ap_model_noc_rsp_after_coherence_read_req_, which
// models the NoC sending back a response via plane #3 when the DUT
// sends a coherence read request to plane #1.
generate
for (genvar genvar_3 = 0; (genvar_3 < nmst) && (`FAST == 0); genvar_3++)
  begin : genvar_3
    ap_model_noc_rsp_after_coherence_read_req : assume property(ap_model_noc_rsp_after_coherence_read_req_(genvar_3, `MAX_HEADER_DELAY, (`MAX_PAYLOAD_DELAY >= `MAX_PAYLOAD_LENGTH) ? `MAX_PAYLOAD_DELAY : `MAX_PAYLOAD_LENGTH));
  end
endgenerate

// The following assumption property models the NoC sending back a response
// via plane #5 when the DUT sends a non-coherent read request to plane #5. 
// This property is as general as possible. It only assumes that there is
// an upper bound to when a reply should be received and that the word
// size cannot be 64-bit (i.e. HSIZE_DWORD). Furthermore, this property
// **correctly** models the entire behaviour of the NoC when responding
// to a non-coherent read request with word size less than 64-bit.
property ap_model_noc_rsp_after_misc_lt_dword_read_req_(integer xindex = 0,
                                                        integer max_header_delay = 2,
                                                        integer max_payload_delay = 10);
  @(posedge clk) disable iff(~rst)
  (
    // Require 'remote_ahbs_snd_wrreq' to be
    // deasserted as a packet is never sent
    // to the NoC immediately after sending
    // another one. There is always at least
    // a clock cycle delay in-between.
    ##0 ~remote_ahbs_snd_wrreq

    // Specify values of generic parameters so this assumption
    // is only enabled when needed; noting that the generic 
    // parameters are guaranteed to always stay the same
    // (assumed stable). This avoids assumptions from
    // conflicting with each other.
    ##1 (mem_axi_port != -1 && mem_axi_port != transaction_reg.xindex)

    // The assumption is instantiated multiple times
    // with different values for 'xindex', which is
    // the index of the AXI master that this assumption
    // considers. The reason there must be an assumption
    // for each master is because each master has its own
    // set of signals that are used in the consequent.
    // Accordingly, this assumption is only enabled if
    // the master that is sending the request is the
    // same as the master that this assumption handles.
    ##0 transaction_reg.xindex == xindex

    // Header flit is sent into 'remote_ahbs_snd_data_in' and
    // 'remote_ahbs_snd_wrreq' is simultaneously asserted. We
    // also require that the header is constructed correctly.
    ##0 (remote_ahbs_snd_wrreq &&
         misc_get_preamble(remote_ahbs_snd_data_in) == preamble_header &&
         misc_get_reserved(remote_ahbs_snd_data_in)[3] == 0 && // isn't HSIZE_DWORD
         (~retarget_for_dma || misc_get_msg_type(remote_ahbs_snd_data_in) == dma_to_dev) &&
         (retarget_for_dma || misc_get_msg_type(remote_ahbs_snd_data_in) == ahb_rd))

    // Flit specifying the memory address of the data we
    // want to read is sent into 'remote_ahbs_snd_data_in' 
    // and 'remote_ahbs_snd_wrreq' is simultaneously asserted.
    ##1 remote_ahbs_snd_wrreq[->1]
    ##0 misc_get_preamble(remote_ahbs_snd_data_in) == preamble_body

    // Flit specifying the amount of data we want to read
    // (i.e. payload length) is sent into 'remote_ahbs_snd_data_in'
    // and 'remote_ahbs_snd_wrreq' is simultaneously asserted.
    ##1 remote_ahbs_snd_wrreq[->1]
    ##0 misc_get_preamble(remote_ahbs_snd_data_in) == preamble_tail

    // Lastly, the payload length must be no more
    // than what this property can assume. Without
    // the following check, properties that use a
    // length greater or equal to max_payload_delay
    // would be uncoverable but proven. In contrast,
    // with this check, they are covered and fire
    // as desired.
    ##0 remote_ahbs_snd_data_in[7:0] <= `MAX_PAYLOAD_LENGTH
  )

  |->

  strong(
  (
    // Only the miscellaneous plane is used throughout this transaction.
    coherence_empty_low_cnt == 0
  )
  throughout
  (
    // Ensure that no data has yet been received from the NoC
    // since the start of the transaction (the counters are reset
    // from within the DUT at the beginning of every transaction).
    ##1 misc_empty_low_cnt == 0

    // Within max_header_delay (predefined) clock cycles,
    // the DUT must have received the header flit.
    // Note, it is **critical** that misc_empty_low_cnt
    // is checked at every clock cycle, not just at the end.
    ##0 (sample_flits == 1'b0 &&
         misc_get_preamble(remote_ahbs_rcv_data_out) == preamble_header &&
         misc_empty_low_cnt == 0)
        [*1:max_header_delay]
    ##1 misc_empty_low_cnt == 1
    
    // Note, we could use transaction_reg.len instead of read_req_length but
    // the use of read_req_length ensures that the read request was actually
    // correct. The reason is that read_req_length is assigned based only on
    // the values of the NoC signals.
    
    // Within max_payload_delay (predefined) clock cycles, the DUT must
    // have received exactly payload_length flits (excluding the header flit).
    // Note, it is **critical** that misc_empty_low_mosi_r_ready_high_cnt
    // is checked at every clock cycle, not just at the end.
    ##0 (sample_flits == 1'b0 &&
         misc_get_preamble(remote_ahbs_rcv_data_out) == preamble_body &&
         misc_empty_low_mosi_r_ready_high_cnt[xindex] < (read_req_length+first_misc_empty_low_had_mosi_r_ready_high[xindex]))
        [*0:max_payload_delay-1]
    
    // Ensure the correct number of flits have been received & acknowledged.
    ##1 misc_empty_low_mosi_r_ready_high_cnt[xindex] == (read_req_length+first_misc_empty_low_had_mosi_r_ready_high[xindex]-1)

    // Final payload flit must have its preamble set to 'preamble_tail',
    // which denotes that it is the last flit of the packet.
    ##0 (~remote_ahbs_rcv_empty &&
         mosi[xindex].r.ready &&
         misc_get_preamble(remote_ahbs_rcv_data_out) == preamble_tail)

    // Ensure the correct number of flits have been received & acknowledged.
    ##1 misc_empty_low_mosi_r_ready_high_cnt[xindex] == (read_req_length+first_misc_empty_low_had_mosi_r_ready_high[xindex])
  ))
endproperty : ap_model_noc_rsp_after_misc_lt_dword_read_req_

generate
for (genvar genvar_6 = 0; (genvar_6 < nmst) && (`FAST == 0); genvar_6++)
  begin : genvar_6
    ap_model_noc_rsp_after_misc_lt_dword_read_req : assume property(ap_model_noc_rsp_after_misc_lt_dword_read_req_(genvar_6, `MAX_HEADER_DELAY, (`MAX_PAYLOAD_DELAY >= `MAX_PAYLOAD_LENGTH) ? `MAX_PAYLOAD_DELAY : `MAX_PAYLOAD_LENGTH));
  end
endgenerate

/* assertions & cover properties that verify DUT by leveraging the most general model of the NoC */

sequence antecedent_send_coherence_read_req_and_eventually_rcv_data(xindex);
  (
    // Specify values of generic parameters (assumed stable).
    (mem_axi_port == -1 || mem_axi_port == xindex) &&

    // Specify read request.
    mosi[xindex].ar.size <= hsize_dword &&

    // Noting that (AXI payload length) == (actual payload length - 1), then:
    ((`ENABLE_FIRED == 0)
      // If we uses a valid length as follows, then
      // the property will be **covered** and **proven**.
      ? (mosi[xindex].ar.len <= (`MAX_PAYLOAD_LENGTH-1))
      // If we instead used an invalid length as follows,
      // the property will either be **uncoverable** or
      // **fire** (depends on assumptions used).
      : (mosi[xindex].ar.len >= `MAX_PAYLOAD_LENGTH)
    ) &&

    // We are as general as possible when assigning the valid bits.
    // Nevertheless, the 'xindex'th master must be selected; noting
    // that a master with a lower index has higher priority.
    ((xindex == 0)
      // xindex has highest priority
      ? (mosi[xindex].ar.valid == 1'b1 &&
         mosi[xindex].aw.valid == 1'b0)
      
      // xindex has lowest priority
      : (mosi[xindex].ar.valid == 1'b1 &&
         mosi[xindex].aw.valid == 1'b0 &&
         mosi[1-xindex].ar.valid == 1'b0 &&
         mosi[1-xindex].aw.valid == 1'b0)
    ) &&

    // We are as general as possible when setting the state.
    next_state == request_header &&
    current_state != request_header
  );
endsequence : antecedent_send_coherence_read_req_and_eventually_rcv_data

sequence consequent_send_coherence_read_req_and_eventually_rcv_data(xindex);
  (
    ##1 (current_state == request_header)[*1:$]         
    ##1 (current_state == request_address)[*1:$]
    ##1 (current_state == request_length)[*1:$]
    ##1 (current_state == reply_header)[*1:$]

    // Ensure the DUT itself properly assigned NoC signals.
    // In particular, 'coherence_rsp_rcv_rdreq' should have
    // been set high when header received, then immediately
    // transitioned to reply_data state to receive payload.
    ##1 (current_state == reply_data &&
         coherence_rdreq_high_cnt == 1 &&
         $past(coherence_rsp_rcv_rdreq) &&
         $past(~coherence_rsp_rcv_empty) &&
         read_req_length == transaction_reg.len)
    
    // Sanity checks.
    ##0 (get_preamble($past(coherence_rsp_rcv_data_out)) == preamble_header &&
         coherence_empty_low_cnt == 1 &&
         coherence_empty_low_mosi_r_ready_high_cnt[xindex] == first_coherence_empty_low_had_mosi_r_ready_high[xindex] &&
         coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt[xindex] == first_coherence_empty_low_had_mosi_r_ready_high[xindex])

    // Stay in reply_data state until received entire packet.
    // Also do some sanity checks.
    ##0 (current_state == reply_data &&
         get_preamble(coherence_rsp_rcv_data_out) != preamble_header &&
         (($past(current_state) != reply_header) -> (get_preamble($past(coherence_rsp_rcv_data_out)) == preamble_body)))
        [*1:$]

    // Data has been fully received.
    ##1 (current_state != reply_data &&
         current_state != fv_bad_state)
    
    // Ensure the DUT itself properly assigned NoC signals.
    // Specifically, when receiving the payload, the signal
    // 'coherence_rsp_rcv_rdreq' should have been set high
    // every time the master was ready and NoC was not empty.
    // Also do some sanity checks.
    ##0 ($past(transaction_reg.len) >= 1 &&
         coherence_rdreq_high_cnt <= coherence_empty_low_cnt &&
         coherence_rdreq_high_cnt == $past(transaction_reg.len)+1 &&
         coherence_rdreq_high_cnt == (coherence_empty_low_mosi_r_ready_high_cnt[xindex]+(1'b0 == first_coherence_empty_low_had_mosi_r_ready_high[xindex])) &&
         coherence_rdreq_high_cnt == (coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt[xindex]+(1'b0 == first_coherence_empty_low_had_mosi_r_ready_high[xindex])) &&
         get_preamble($past(coherence_rsp_rcv_data_out)) == preamble_tail)
  );
endsequence : consequent_send_coherence_read_req_and_eventually_rcv_data

sequence compact_consequent_send_coherence_read_req_and_eventually_rcv_data(xindex);
  ( // This is equivalent to the non-compact version without the sanity checks.
    ##1 (current_state == request_header)[*1:$]         
    ##1 (current_state == request_address)[*1:$]
    ##1 (current_state == request_length)[*1:$]
    ##1 (current_state == reply_header)[*1:$]
    ##1 (current_state == reply_data)[*1:$]
    ##1 (current_state != reply_data &&
         current_state != fv_bad_state)
  );
endsequence : compact_consequent_send_coherence_read_req_and_eventually_rcv_data

property ap_send_coherence_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  (antecedent_send_coherence_read_req_and_eventually_rcv_data(xindex)
  |->
  strong(consequent_send_coherence_read_req_and_eventually_rcv_data(xindex)))
endproperty : ap_send_coherence_read_req_and_eventually_rcv_data_

property cp_send_coherence_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  strong(
    antecedent_send_coherence_read_req_and_eventually_rcv_data(xindex)
    ##0
    consequent_send_coherence_read_req_and_eventually_rcv_data(xindex)
  )
endproperty : cp_send_coherence_read_req_and_eventually_rcv_data_

property ap_compact_send_coherence_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  (antecedent_send_coherence_read_req_and_eventually_rcv_data(xindex)
  |->
  strong(compact_consequent_send_coherence_read_req_and_eventually_rcv_data(xindex)))
endproperty : ap_compact_send_coherence_read_req_and_eventually_rcv_data_

property cp_compact_send_coherence_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  strong(
    antecedent_send_coherence_read_req_and_eventually_rcv_data(xindex)
    ##0
    compact_consequent_send_coherence_read_req_and_eventually_rcv_data(xindex)
  )
endproperty : cp_compact_send_coherence_read_req_and_eventually_rcv_data_

// Here we assert & cover properties that leverage the assumed property that
// models, in the most general way, the NoC sending back a response via plane
// #3 when the DUT sends a coherence read request to plane #1. 
//
// It is important that we both assert and cover this property so as to detect false positives.
// Covering the property genuinely helped us a lot during debugging as the tool isn't actually
// great at detecting vacuous proofs.
generate
  for (genvar genvar_2 = 0; (genvar_2 < nmst) && (`FAST == 0); genvar_2++)
  begin : genvar_2
    ap_send_coherence_read_req_and_eventually_rcv_data: assert property (ap_send_coherence_read_req_and_eventually_rcv_data_(genvar_2));
    cp_send_coherence_read_req_and_eventually_rcv_data: cover property (cp_send_coherence_read_req_and_eventually_rcv_data_(genvar_2));

    // The compact properties are equivalent to the non-compact
    // version without the sanity checks in the consequent.
    ap_compact_send_coherence_read_req_and_eventually_rcv_data: assert property (ap_compact_send_coherence_read_req_and_eventually_rcv_data_(genvar_2));
    cp_compact_send_coherence_read_req_and_eventually_rcv_data: cover property (cp_compact_send_coherence_read_req_and_eventually_rcv_data_(genvar_2));
  end
endgenerate

sequence antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex);
  (
    // Specify values of generic parameters (assumed stable).
    mem_axi_port != -1 &&
    mem_axi_port != xindex &&

    // Specify read request.
    mosi[xindex].ar.size < hsize_dword &&

    // Noting that (AXI payload length) == (actual payload length - 1), then:
    ((`ENABLE_FIRED == 0)
      // If we uses a valid length as follows, then
      // the property will be **covered** and **proven**.
      ? (mosi[xindex].ar.len <= (`MAX_PAYLOAD_LENGTH-1))
      // If we instead used an invalid length as follows,
      // the property will either be **uncoverable** or
      // **fire** (depends on assumptions used).
      : (mosi[xindex].ar.len >= `MAX_PAYLOAD_LENGTH)
    ) &&

    // We are as general as possible when assigning the valid bits.
    // Nevertheless, the 'xindex'th master must be selected; noting
    // that a master with a lower index has higher priority.
    ((xindex == 0)
      // xindex has highest priority
      ? (mosi[xindex].ar.valid == 1'b1 &&
         mosi[xindex].aw.valid == 1'b0)
      
      // xindex has lowest priority
      : (mosi[xindex].ar.valid == 1'b1 &&
         mosi[xindex].aw.valid == 1'b0 &&
         mosi[1-xindex].ar.valid == 1'b0 &&
         mosi[1-xindex].aw.valid == 1'b0)
    ) &&

    // We are as general as possible when setting the state.
    next_state == request_header &&
    current_state != request_header
  );
endsequence : antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data

sequence consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex);
  (
    ##1 (current_state == request_header)[*1:$]         
    ##1 (current_state == request_address)[*1:$]
    ##1 (current_state == request_length)[*1:$]
    ##1 (current_state == reply_header)[*1:$]

    // Ensure the DUT itself properly assigned NoC signals.
    // In particular, 'remote_ahbs_rcv_rdreq' should have
    // been set high when header received, then immediately
    // transitioned to reply_data state to receive payload.
    ##1 (current_state == reply_data &&
         misc_rdreq_high_cnt == 1 &&
         $past(remote_ahbs_rcv_rdreq) &&
         $past(~remote_ahbs_rcv_empty) &&
         read_req_length == transaction_reg.len)
    
    // Sanity checks.
    ##0 (misc_get_preamble($past(remote_ahbs_rcv_data_out)) == preamble_header &&
         misc_empty_low_cnt == 1 &&
         misc_empty_low_mosi_r_ready_high_cnt[xindex] == first_misc_empty_low_had_mosi_r_ready_high[xindex] &&
         misc_empty_low_rdreq_high_mosi_r_ready_high_cnt[xindex] == first_misc_empty_low_had_mosi_r_ready_high[xindex])

    // Stay in reply_data state until received entire packet.
    // Also do some sanity checks.
    ##0 (current_state == reply_data &&
         misc_get_preamble(remote_ahbs_rcv_data_out) != preamble_header &&
         (($past(current_state) != reply_header) -> (misc_get_preamble($past(remote_ahbs_rcv_data_out)) == preamble_body)))
        [*1:$]

    // Data has been fully received.
    ##1 (current_state != reply_data &&
         current_state != fv_bad_state)
    
    // Ensure the DUT itself properly assigned NoC signals.
    // Specifically, when receiving the payload, the signal
    // 'remote_ahbs_rcv_rdreq' should have been set high every
    // time the master was ready and NoC was not empty.
    // Also do some sanity checks.
    ##0 ($past(transaction_reg.len) >= 1 &&
         misc_rdreq_high_cnt <= misc_empty_low_cnt &&
         misc_rdreq_high_cnt == $past(transaction_reg.len)+1 &&
         misc_rdreq_high_cnt == (misc_empty_low_mosi_r_ready_high_cnt[xindex]+(1'b0 == first_misc_empty_low_had_mosi_r_ready_high[xindex])) &&
         misc_rdreq_high_cnt == (misc_empty_low_rdreq_high_mosi_r_ready_high_cnt[xindex]+(1'b0 == first_misc_empty_low_had_mosi_r_ready_high[xindex])) &&
         misc_get_preamble($past(remote_ahbs_rcv_data_out)) == preamble_tail)
  );
endsequence : consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data

sequence compact_consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex);
  ( // This is equivalent to the non-compact version without the sanity checks.
    ##1 (current_state == request_header)[*1:$]         
    ##1 (current_state == request_address)[*1:$]
    ##1 (current_state == request_length)[*1:$]
    ##1 (current_state == reply_header)[*1:$]
    ##1 (current_state == reply_data)[*1:$]
    ##1 (current_state != reply_data &&
         current_state != fv_bad_state)
  );
endsequence : compact_consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data

property ap_send_misc_lt_dword_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  (antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
  |->
  strong(consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)))
endproperty : ap_send_misc_lt_dword_read_req_and_eventually_rcv_data_

property cp_send_misc_lt_dword_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  strong(
    antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
    ##0
    consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
  )
endproperty : cp_send_misc_lt_dword_read_req_and_eventually_rcv_data_

property ap_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  (antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
  |->
  strong(compact_consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)))
endproperty : ap_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_

property cp_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_(xindex);
  @(posedge clk) disable iff(~rst)
  strong(
    antecedent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
    ##0
    compact_consequent_send_misc_lt_dword_read_req_and_eventually_rcv_data(xindex)
  )
endproperty : cp_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_

// Here we assert & cover properties that leverage the assumed property that
// models the NoC sending back a response via plane #5 when the DUT sends a
// non-coherent read request to plane #5. 
//
// It is important that we both assert and cover this property so as to detect false positives.
// Covering the property genuinely helped us a lot during debugging as the tool isn't actually
// great at detecting vacuous proofs.
generate
for (genvar genvar_5 = 0; (genvar_5 < nmst) && (`FAST == 0); genvar_5++)
  begin : genvar_5
    ap_send_misc_lt_dword_read_req_and_eventually_rcv_data: assert property (ap_send_misc_lt_dword_read_req_and_eventually_rcv_data_(genvar_5));
    cp_send_misc_lt_dword_read_req_and_eventually_rcv_data: cover property (cp_send_misc_lt_dword_read_req_and_eventually_rcv_data_(genvar_5));

    // The compact properties are equivalent to the non-compact
    // version without the sanity checks in the consequent.
    ap_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data: assert property (ap_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_(genvar_5));
    cp_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data: cover property (cp_compact_send_misc_lt_dword_read_req_and_eventually_rcv_data_(genvar_5));
  end
endgenerate

/* AXI handshakes */

property ap_write_address_channel_handshake_(xindex);
  @(posedge clk) disable iff(~rst)
  mosi[xindex].aw.valid |-> (mosi[xindex].aw.valid until_with somi[xindex].aw.ready)
endproperty : ap_write_address_channel_handshake_

property ap_write_data_channel_handshake_(xindex);
  @(posedge clk) disable iff(~rst)
  mosi[xindex].w.valid |-> (mosi[xindex].w.valid until_with somi[xindex].w.ready)
endproperty : ap_write_data_channel_handshake_

property ap_write_response_channel_handshake_(xindex);
  @(posedge clk) disable iff(~rst)
  somi[xindex].b.valid |-> (somi[xindex].b.valid until_with mosi[xindex].b.ready)
endproperty : ap_write_response_channel_handshake_

property ap_read_address_channel_handshake_(xindex);
  @(posedge clk) disable iff(~rst)
  mosi[xindex].ar.valid |-> (mosi[xindex].ar.valid until_with somi[xindex].ar.ready)
endproperty : ap_read_address_channel_handshake_

property ap_read_data_channel_handshake_(xindex);
  @(posedge clk) disable iff(~rst)
  somi[xindex].r.valid |-> (somi[xindex].r.valid until_with mosi[xindex].r.ready)
endproperty : ap_read_data_channel_handshake_

generate // assume and assert AXI handshake properties
for (genvar genvar_0 = 0; genvar_0 < nmst; genvar_0++)
  begin : genvar_0
    ap_write_address_channel_handshake: assume property (ap_write_address_channel_handshake_(genvar_0));
    ap_write_data_channel_handshake: assume property (ap_write_data_channel_handshake_(genvar_0));
    ap_read_address_channel_handshake: assume property (ap_read_address_channel_handshake_(genvar_0));

    if (`ENABLE_FIRED == 0) begin : expected_to_pass
      ap_write_response_channel_handshake: assert property (ap_write_response_channel_handshake_(genvar_0));
    end : expected_to_pass
    else if (`ENABLE_FIRED == 1) begin : expected_to_fire
      // Mismatch with AXI4 spec, which states that "When [RVALID is] asserted, RVALID must remain asserted until the rising clock edge after the master asserts RREADY."
      // See https://developer.arm.com/documentation/ihi0022/e/AMBA-AXI3-and-AXI4-Protocol-Specification/Single-Interface-Requirements/Basic-read-and-write-transactions/Channel-signaling-requirements?lang=en#CIACFIIF
      ap_read_data_channel_handshake: assert property (ap_read_data_channel_handshake_(genvar_0));
    end : expected_to_fire
  end
endgenerate

/* test correct transfer of data from NoC to somi */

// Payload data received from plane #3 is forwarded
// to the appropriate AXI channel when a coherent
// read request is performed.
property ap_coherence_payload_content_;
  @(posedge clk) disable iff(~rst)
  (transaction_reg.dst_is_mem == 1'b1)
  |->
  (somi[transaction_reg.xindex].r.data == coherence_rsp_rcv_data_out[ahbdw-1:0])
endproperty : ap_coherence_payload_content_

generate
  if (`ENABLE_FIRED == 0) begin : noc_to_somi
    ap_coherence_payload_content : assert property (ap_coherence_payload_content_());
  end : noc_to_somi
endgenerate
