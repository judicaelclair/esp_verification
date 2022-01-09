-- Copyright (c) 2011-2021 Columbia University, System Level Design Group
-- SPDX-License-Identifier: Apache-2.0

-- I, Judicael S. E. Clair, isolated axislv2noc from ESP, modified its
-- interface and internals so as to perform formal verification on it.
-- Modifications in this file:
-- * modified which libraries are imported, now we rely on env_pkg
--   instead of ESP packages.
-- * redefined retarget_for_dma, mem_axi_port, mem_info, slv_y,
--   and slv_x to be input ports instead of generic parameters.
-- * added an instance of fv_util for instrumenting axislv2noc.
-- * use of mosi_extended and somi_extended ports instead of
--   mosi and somi, which are now defined as aliases.
-- * introduced a counter defined by fv_rem_read_cnt and
--   fv_next_rem_read_cnt, which is used to ensure the correct
--   number of flits are received during a transaction. If too
--   many are received, we transition to a newly introduced
--   fv_bad_state, and we stay in this state indefinitely.

-------------------------------------------------------------------------------
-- This proxy replaces AHB slaves that are hosted in remote tiles and forwards
-- AHB requests from masters to the NoC. Responses from the NoC are returned to
-- the bus master as if the remote device was connected to the local bus.
--
-- This is intended to serve requests from the Leon3 processor, the Ethernet
-- DMA engine and the JTAG or Ethernet debug interfaces. Since these master can
-- only issue requests for up to 32-bits words, ths proxy does not handle
-- larger word sizes.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

--pragma translate_off
use std.textio.all;
use ieee.std_logic_textio.all;
--pragma translate_on

use work.env_pkg.all;

entity axislv2noc is
  generic (
    mem_num          : integer := 1;
    tech             : integer := inferred;
    nmst             : integer := MAX_NMST);
  port (
    -- moved from generic map for verification purposes
    retarget_for_dma : in integer range 0 to 1;
    mem_axi_port     : in integer range -1 to NAHBSLV - 1;
    mem_info         : in tile_mem_info_vector;
    slv_y            : in local_yx;
    slv_x            : in local_yx;
    -- general
    rst                        : in  std_ulogic;
    clk                        : in  std_ulogic;
    local_y                    : in  local_yx;
    local_x                    : in  local_yx;
    mosi_extended              : in  axi_mosi_extended_vector;
    somi_extended              : out axi_somi_extended_vector;
    -- tile->NoC1
    coherence_req_wrreq        : out std_ulogic;
    coherence_req_data_in      : out noc_flit_type;
    coherence_req_full         : in  std_ulogic;
    -- Noc3->tile
    coherence_rsp_rcv_rdreq    : out std_ulogic;
    coherence_rsp_rcv_data_out : in  noc_flit_type;
    coherence_rsp_rcv_empty    : in  std_ulogic;
    -- tile->NoC5
    remote_ahbs_snd_wrreq      : out std_ulogic;
    remote_ahbs_snd_data_in    : out misc_noc_flit_type;
    remote_ahbs_snd_full       : in  std_ulogic;
    -- NoC5->tile
    remote_ahbs_rcv_rdreq      : out std_ulogic;
    remote_ahbs_rcv_data_out   : in  misc_noc_flit_type;
    remote_ahbs_rcv_empty      : in  std_ulogic);
end axislv2noc;

architecture rtl of axislv2noc is
  signal transaction, transaction_reg : transaction_type;
  signal current_state, next_state    : axi_fsm;
  signal selected                     : std_ulogic;
  signal sample_flits                 : std_ulogic;

  signal remote_ahbs_rcv_data_out_hold  : misc_noc_flit_type;
  signal sample_and_hold : std_ulogic;

  -- signals used for formal verification (e.g. to make sure properties mean what we think they mean)
  signal fv_rem_read_cnt, fv_next_rem_read_cnt : std_ulogic_vector(7 downto 0);
  alias mosi : axi_mosi_vector(0 to nmst - 1) is mosi_extended(0 to nmst - 1);
  alias somi : axi_somi_vector(0 to nmst - 1) is somi_extended(0 to nmst - 1);

begin  -- rtl

  -- signal processing for formal verification
  fv_util_inst : entity work.fv_util
    generic map (
      nmst => nmst
    )
    port map (
      rst => rst,
      clk => clk,
      mosi => mosi,
      coherence_rsp_rcv_empty => coherence_rsp_rcv_empty,
      coherence_rsp_rcv_rdreq => coherence_rsp_rcv_rdreq,
      coherence_req_wrreq => coherence_req_wrreq,
      coherence_req_data_in => coherence_req_data_in,
      remote_ahbs_rcv_empty => remote_ahbs_rcv_empty,
      remote_ahbs_rcv_rdreq => remote_ahbs_rcv_rdreq,
      remote_ahbs_snd_wrreq => remote_ahbs_snd_wrreq,
      remote_ahbs_snd_data_in => remote_ahbs_snd_data_in,
      sample_flits => sample_flits,
      transaction_reg => transaction_reg      
    );

  make_packet: process (mosi, local_y, local_x)
    variable tran : transaction_type;
    variable adj_mem_axi_port : integer range 0 to NAHBSLV - 1;
  begin  -- process make_packet

    -- Default
    tran := transaction_none;
    selected <= '0';

    -- Prioritize masters (0 highest priority) and set xindex
    for i in  nmst - 1 downto 0 loop
      -- Higher priority to master with low index
      if (mosi(i).aw.valid or mosi(i).ar.valid) = '1' then
        tran.xindex := i;
        selected <= '1';
      end if;
    end loop;  -- i

    -- Set write
    tran.write := mosi(tran.xindex).aw.valid;

    -- set ctrl
    if tran.write = '1' then
      tran.id     := mosi(tran.xindex).aw.id;
      tran.addr   := mosi(tran.xindex).aw.addr;
      tran.len    := mosi(tran.xindex).aw.len;
      tran.size   := mosi(tran.xindex).aw.size;
      tran.burst  := mosi(tran.xindex).aw.burst;
      tran.lock   := mosi(tran.xindex).aw.lock;
      tran.cache  := mosi(tran.xindex).aw.cache;
      tran.prot   := mosi(tran.xindex).aw.prot;
      tran.qos    := mosi(tran.xindex).aw.qos;
      tran.atop   := mosi(tran.xindex).aw.atop;
      tran.region := mosi(tran.xindex).aw.region;
      tran.user   := mosi(tran.xindex).aw.user;
    else
      tran.id     := mosi(tran.xindex).ar.id;
      tran.addr   := mosi(tran.xindex).ar.addr;
      tran.len    := mosi(tran.xindex).ar.len;
      tran.size   := mosi(tran.xindex).ar.size;
      tran.burst  := mosi(tran.xindex).ar.burst;
      tran.lock   := mosi(tran.xindex).ar.lock;
      tran.cache  := mosi(tran.xindex).ar.cache;
      tran.prot   := mosi(tran.xindex).ar.prot;
      tran.qos    := mosi(tran.xindex).ar.qos;
      tran.region := mosi(tran.xindex).ar.region;
      tran.user   := mosi(tran.xindex).ar.user;
    end if;
    tran.len := mosi(tran.xindex).ar.len + "0000001";

    -- Get routing info
    tran.mem_x := mem_info(0).x;
    tran.mem_y := mem_info(0).y;

    -- TODO: support larger address space / regions
    tran.addr_msb := tran.addr(GLOB_PHYS_ADDR_BITS - 1 downto GLOB_PHYS_ADDR_BITS - 12);

    if mem_num /= 1 then
      for i in 0 to mem_num - 1 loop
        -- Need to match which memory split is selected
        if ((tran.addr_msb xor conv_std_logic_vector(mem_info(i).haddr, 12))
            and conv_std_logic_vector(mem_info(i).hmask, 12)) = X"000" then
          tran.mem_x := mem_info(i).x;
          tran.mem_y := mem_info(i).y;
        end if;
      end loop;  -- i
    end if;

    -- Determine whether memory is selected
    if mem_axi_port = -1 then
      adj_mem_axi_port := 0;
    else
      adj_mem_axi_port := mem_axi_port;
    end if;
    if tran.xindex = adj_mem_axi_port or mem_axi_port = -1 then
      tran.dst_is_mem :=  '1';
    else
      tran.mem_x := slv_x;
      tran.mem_y := slv_y;
    end if;

    -- Set message type
    if tran.size = HSIZE_DWORD then
      tran.hsize_msb := '1';
    end if;

    if tran.write = '1' then
      if retarget_for_dma = 1 then
        tran.msg_type := REQ_DMA_WRITE;  -- This request is non coherent, but
                                         -- noc2ahbmst must skip
                                         -- receive_wrlength, which only exists
                                         -- for DMA_FROM_DEV from accelerators.
                                         -- We use this workaround because
                                         -- systems without DDR controller are
                                         -- using a FPGA-to-memory link on
                                         -- which we do not send the tail bit
                                         -- to save some I/O pads.
      else -- Processor core request
        if tran.dst_is_mem = '1' then
          -- Send to Memory
          if tran.size = HSIZE_BYTE then
            tran.msg_type := REQ_GETM_B;
          elsif tran.size = HSIZE_HWORD then
            tran.msg_type := REQ_GETM_HW;
          else
            tran.msg_type := REQ_GETM_W;
          end if;
        else
          -- Send to remote slave uncached
          tran.msg_type := AHB_WR;
        end if;
      end if;
    else
      if retarget_for_dma = 1 then
        tran.msg_type := DMA_TO_DEV;
      else -- Processor core request
        if tran.dst_is_mem = '1' then
          -- Send to Memory
          if tran.size = HSIZE_BYTE then
            tran.msg_type := REQ_GETS_B;
          elsif tran.size = HSIZE_HWORD then
            tran.msg_type := REQ_GETS_HW;
          else
            tran.msg_type := REQ_GETS_W;
          end if;
        else
          -- Send to remote slave uncached
          tran.msg_type := AHB_RD;
        end if;
      end if;
    end if;

    -- Set address
    tran.payload_address(NOC_FLIT_SIZE-1 downto NOC_FLIT_SIZE-PREAMBLE_WIDTH) := PREAMBLE_BODY;
    tran.payload_address(GLOB_PHYS_ADDR_BITS - 1 downto 0) := tran.addr;

    tran.payload_address_narrow(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE-PREAMBLE_WIDTH) := PREAMBLE_BODY;
    tran.payload_address_narrow(MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH - 1 downto 0) := tran.addr(MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH - 1 downto 0);

    -- Set length (read transaction only)
    tran.payload_length(NOC_FLIT_SIZE-1 downto NOC_FLIT_SIZE-PREAMBLE_WIDTH) := PREAMBLE_TAIL;
    tran.payload_length(7 downto 0) := tran.len;

    tran.payload_length_narrow(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE-PREAMBLE_WIDTH) := PREAMBLE_TAIL;
    tran.payload_length_narrow(7 downto 0) := tran.len;

    -- Create header flit
    tran.reserved             := (others => '0');
    tran.reserved(3)          := tran.hsize_msb;
    tran.reserved(2 downto 0) := tran.prot;

    tran.header := create_header(NOC_FLIT_SIZE, local_y, local_x, tran.mem_y, tran.mem_x, tran.msg_type, tran.reserved);
    tran.header_narrow := create_header(MISC_NOC_FLIT_SIZE, local_y, local_x, tran.mem_y, tran.mem_x, tran.msg_type, tran.reserved)(MISC_NOC_FLIT_SIZE - 1 downto 0);

    -- Write signal
    transaction <= tran;
  end process make_packet;



  -- AXI2NOC
  axi_roundtrip: process (transaction_reg, current_state, selected, mosi,
                          coherence_req_full,
                          coherence_rsp_rcv_data_out, coherence_rsp_rcv_empty,
                          remote_ahbs_snd_full,
                          remote_ahbs_rcv_data_out, remote_ahbs_rcv_empty,
                          remote_ahbs_rcv_data_out_hold)
    variable wdata                   : std_logic_vector(AHBDW - 1 downto 0);
    variable payload_data            : noc_flit_type;
    variable payload_data_narrow_lsb : misc_noc_flit_type;
    variable payload_data_narrow_msb : misc_noc_flit_type;
    variable rsp_preamble            : noc_preamble_type;
    variable slv_ready               : std_ulogic;
    variable slv_valid               : std_ulogic;
    variable mst_ready               : std_ulogic;
    variable mst_valid               : std_ulogic;
    variable mst_bready              : std_ulogic;
    variable last                    : std_ulogic;
  begin  -- process axi_roundtrip

    fv_next_rem_read_cnt <= fv_rem_read_cnt;

    -- Default internal state
    next_state <= current_state;
    sample_flits <= '0';
    sample_and_hold <= '0';

    -- Response data flit (AXI Read)
    if transaction_reg.dst_is_mem = '1' then
      rsp_preamble := get_preamble(NOC_FLIT_SIZE, coherence_rsp_rcv_data_out);
      for i in 0 to nmst - 1 loop
        somi(i).r.data <= (coherence_rsp_rcv_data_out(AHBDW - 1 downto 0));
      end loop;
    else
      rsp_preamble := get_preamble(MISC_NOC_FLIT_SIZE, noc_flit_pad & remote_ahbs_rcv_data_out);
      for i in 0 to nmst - 1 loop
        somi(i).r.data <= (others => '0');
        if transaction_reg.size = HSIZE_DWORD then
          somi(i).r.data(31 downto 0) <= remote_ahbs_rcv_data_out_hold(31 downto 0);
          somi(i).r.data(ARCH_BITS - 1 downto ARCH_BITS - 32) <= (remote_ahbs_rcv_data_out(31 downto 0));
        elsif transaction_reg.size = HSIZE_WORD then
          somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(31 downto 0));
        elsif transaction_reg.size = HSIZE_HWORD then
          case transaction_reg.addr(1) is
            when '0'    => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(15 downto 0));
            when others => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(31 downto 16));
          end case;
        else -- HSIZE_BYTE
          case transaction_reg.addr(1 downto 0) is
            when "00"   => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(7 downto 0));
            when "01"   => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(15 downto 8));
            when "10"   => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(23 downto 16));
            when others => somi(i).r.data <= ahbdrivedata(remote_ahbs_rcv_data_out(31 downto 24));
          end case;
        end if;
      end loop;
    end if;



    -- Default bus slave response
    for i in 0 to nmst - 1 loop
      -- aw
      somi(i).aw.ready <= '0';
      -- ar
      somi(i).ar.ready <= '0';
      -- w
      somi(i).w.ready <= '0';
      -- r
      somi(i).r.id    <= transaction_reg.id;
      somi(i).r.resp  <= RBRESP_OKAY;
      if rsp_preamble = PREAMBLE_TAIL then
        somi(i).r.last  <= '1';
      else
        somi(i).r.last  <= '0';
      end if;
      somi(i).r.user  <= (others => '0');
      somi(i).r.valid <= '0';
      -- b
      somi(i).b.id    <= transaction_reg.id;
      somi(i).b.resp  <= RBRESP_OKAY;
      somi(i).b.user  <= (others => '0');
      somi(i).b.valid <= '0';
    end loop;

    -- Default NoC input port
    coherence_req_data_in   <= (others => '0');
    coherence_req_wrreq     <= '0';
    coherence_rsp_rcv_rdreq <= '0';

    remote_ahbs_snd_data_in <= (others => '0');
    remote_ahbs_snd_wrreq   <= '0';
    remote_ahbs_rcv_rdreq   <= '0';

    -- Data flit (AXI Write)
    if transaction_reg.size = HSIZE_DWORD then
      wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data);
    elsif transaction_reg.size = HSIZE_WORD then
      if AHBDW = 64 then
        case transaction_reg.addr(2) is
          when '0'    => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(31 downto 0));
          when others => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(63 downto 32));
        end case;
      else
        wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data);
      end if;
    elsif transaction_reg.size = HSIZE_HWORD then
      if AHBDW = 64 then
        case transaction_reg.addr(2 downto 1) is
          when "00"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(15 downto 0));
          when "01"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(31 downto 16));
          when "10"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(47 downto 32));
          when others => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(63 downto 48));
        end case;
      else
        case transaction_reg.addr(1) is
          when '0'    => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(15 downto 0));
          when others => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(31 downto 16));
        end case;
      end if;
    else -- HSIZE_BYTE
      if AHBDW = 64 then
        case transaction_reg.addr(2 downto 0) is
          when "000"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(7 downto 0));
          when "001"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(15 downto 8));
          when "010"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(23 downto 16));
          when "011"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(31 downto 24));
          when "100"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(39 downto 32));
          when "101"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(47 downto 40));
          when "110"  => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(55 downto 48));
          when others => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(63 downto 56));
        end case;
      else
        case transaction_reg.addr(1 downto 0) is
          when "00"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(7 downto 0));
          when "01"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(15 downto 8));
          when "10"   => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(23 downto 16));
          when others => wdata := ahbdrivedata(mosi(transaction_reg.xindex).w.data(31 downto 24));
        end case;
      end if;
    end if;

    payload_data            := (others => '0');
    payload_data_narrow_lsb := (others => '0');
    payload_data_narrow_msb := (others => '0');
    payload_data_narrow_lsb(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH) := PREAMBLE_BODY;
    if (mosi(transaction_reg.xindex).w.last = '1') then
      payload_data(NOC_FLIT_SIZE-1 downto NOC_FLIT_SIZE - PREAMBLE_WIDTH)                      := PREAMBLE_TAIL;
      payload_data_narrow_msb(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH) := PREAMBLE_TAIL;
      last                                                                                     := '1';
    else
      payload_data(NOC_FLIT_SIZE-1 downto NOC_FLIT_SIZE - PREAMBLE_WIDTH)                      := PREAMBLE_BODY;
      payload_data_narrow_msb(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH) := PREAMBLE_BODY;
      last                                                                                     := '0';
    end if;
    payload_data(AHBDW - 1 downto 0)                                      := wdata;
    -- TODO: this only works on a 64-bit AXI bus with 32-bit AHB remote slaves
    payload_data_narrow_lsb(31 downto 0) := wdata(31 downto 0);
    payload_data_narrow_msb(31 downto 0) := wdata(ARCH_BITS - 1 downto ARCH_BITS - 32);

    -- Temporary AXI flags
    slv_ready  := '0';
    slv_valid  := '0';
    mst_ready  := mosi(transaction_reg.xindex).r.ready;
    mst_valid  := mosi(transaction_reg.xindex).w.valid;
    mst_bready := mosi(transaction_reg.xindex).b.ready;


    -- FSM
    case current_state is
      when idle =>
        if selected = '1' then
          sample_flits <= '1';
          next_state <= request_header;
        end if;

      when request_header =>
        if transaction_reg.dst_is_mem = '0' and transaction_reg.size = HSIZE_DWORD then
          fv_next_rem_read_cnt <= std_logic_vector(ieee.numeric_std.unsigned(transaction_reg.len) + ieee.numeric_std.unsigned(transaction_reg.len));
        else
          fv_next_rem_read_cnt <= transaction_reg.len;
        end if;

        if transaction_reg.dst_is_mem = '1' then
          if coherence_req_full = '0' then
            coherence_req_data_in <= transaction_reg.header;
            coherence_req_wrreq   <= '1';
            next_state         <= request_address;
            -- Control signals are sampled
            if transaction_reg.write = '1' then
              somi(transaction_reg.xindex).aw.ready <= '1';
            else
              somi(transaction_reg.xindex).ar.ready <= '1';
            end if;
          end if;
        else
          if remote_ahbs_snd_full = '0' then
            remote_ahbs_snd_data_in <= transaction_reg.header_narrow;
            remote_ahbs_snd_wrreq   <= '1';
            next_state           <= request_address;
            -- Control signals are sampled
            if transaction_reg.write = '1' then
              somi(transaction_reg.xindex).aw.ready <= '1';
            else
              somi(transaction_reg.xindex).ar.ready <= '1';
            end if;
          end if;
        end if;

      when request_address =>
        if transaction_reg.dst_is_mem = '1' then
          if coherence_req_full = '0' then
            coherence_req_data_in <= transaction_reg.payload_address;
            coherence_req_wrreq <= '1';
            if transaction_reg.write = '1' then
              next_state <= request_data;
            else
              next_state <= request_length;
            end if;
          end if;
        else
          if remote_ahbs_snd_full = '0' then
            remote_ahbs_snd_data_in <= transaction_reg.payload_address_narrow;
            remote_ahbs_snd_wrreq <= '1';
            if transaction_reg.write = '1' then
              if transaction_reg.size = HSIZE_DWORD then
                next_state <= request_data_lsb;
              else
                next_state <= request_data;
              end if;
            else
              next_state <= request_length;
            end if;
          end if;
        end if;

      when request_length =>
        if transaction_reg.dst_is_mem = '1' then
          if coherence_req_full = '0' then
            coherence_req_data_in <= transaction_reg.payload_length;
            coherence_req_wrreq <= '1';
            -- Ready transaction only
            next_state <= reply_header;
          end if;
        else
          if remote_ahbs_snd_full = '0' then
            remote_ahbs_snd_data_in <= transaction_reg.payload_length_narrow;
            remote_ahbs_snd_wrreq <= '1';
            -- Ready transaction only
            next_state <= reply_header;
          end if;
        end if;

      when request_data =>
        if transaction_reg.dst_is_mem = '1' then
          if coherence_req_full = '0' and mst_valid = '1' then
            slv_ready := '1';
            coherence_req_data_in <= payload_data;
            coherence_req_wrreq <= '1';
            if last = '1' then
              next_state <= request_data_ack;
            end if;
          end if;
        else
          if remote_ahbs_snd_full = '0' and mst_valid = '1' then
            if last = '1' then
              payload_data_narrow_lsb(MISC_NOC_FLIT_SIZE-1 downto MISC_NOC_FLIT_SIZE - PREAMBLE_WIDTH) := PREAMBLE_TAIL;
              next_state <= request_data_ack;
            end if;
            slv_ready := '1';
            remote_ahbs_snd_data_in <= payload_data_narrow_lsb;
            remote_ahbs_snd_wrreq <= '1';
          end if;
        end if;

      when request_data_lsb =>
        if remote_ahbs_snd_full = '0' and mst_valid = '1' then
          remote_ahbs_snd_data_in <= payload_data_narrow_lsb;
          remote_ahbs_snd_wrreq <= '1';
          next_state <= request_data_msb;
        end if;

      when request_data_msb =>
        if remote_ahbs_snd_full = '0' then
          slv_ready := '1';
          remote_ahbs_snd_data_in <= payload_data_narrow_msb;
          remote_ahbs_snd_wrreq <= '1';
          if last = '1' then
            next_state <= request_data_ack;
          else
            next_state <= request_data_lsb;
          end if;
        end if;

      when request_data_ack =>
        somi(transaction_reg.xindex).b.valid <= '1';
        if transaction_reg.lock = '1' then
          -- always return success on RISC-V store conditional
          somi(transaction_reg.xindex).b.resp <= RBRESP_EXOKAY;
        end if;
        if mst_bready = '1' then
          if selected = '0' then
            next_state <= idle;
          else
            sample_flits <= '1';
            next_state <= request_header;
          end if;
        end if;

      when reply_header =>
        if transaction_reg.dst_is_mem = '1' then
          if coherence_rsp_rcv_empty = '0' then
            coherence_rsp_rcv_rdreq <= '1';
            next_state              <= reply_data;
          end if;
        else
          if remote_ahbs_rcv_empty = '0' then
            remote_ahbs_rcv_rdreq <= '1';
            if transaction_reg.size = HSIZE_DWORD then
              next_state <= reply_data_lsb;
            else
              next_state <= reply_data;
            end if;
          end if;
        end if;

      when reply_data =>
        if coherence_rsp_rcv_empty = '0' and mst_ready = '1' then
          coherence_rsp_rcv_rdreq <= '1';
        elsif remote_ahbs_rcv_empty = '0'and mst_ready = '1' then
          remote_ahbs_rcv_rdreq <= '1';
        end if;
        if (coherence_rsp_rcv_empty = '0' or remote_ahbs_rcv_empty = '0') then
          slv_valid := '1';
          if mst_ready = '1' then
            if rsp_preamble = PREAMBLE_TAIL and fv_rem_read_cnt = 1 then
              if selected = '0' then
                next_state <= idle;
              else
                next_state   <= request_header;
                sample_flits <= '1';
              end if;
            elsif fv_rem_read_cnt > 0 then
              fv_next_rem_read_cnt <= fv_rem_read_cnt - 1;
            else
              next_state <= fv_bad_state;
            end if;
          end if;
        end if;

      when reply_data_lsb =>
        if remote_ahbs_rcv_empty = '0' then
          remote_ahbs_rcv_rdreq <= '1';
          sample_and_hold <= '1';
          next_state <= reply_data_msb;

          if fv_rem_read_cnt = 0 then
            next_state <= fv_bad_state;
          else
            fv_next_rem_read_cnt <= fv_rem_read_cnt - 1;
          end if;
        end if;

      when reply_data_msb =>
        if remote_ahbs_rcv_empty = '0' and mst_ready = '1' then
          remote_ahbs_rcv_rdreq <= '1';
        end if;
        if remote_ahbs_rcv_empty = '0' then
          slv_valid := '1';
          if mst_ready = '1' then
            if rsp_preamble = PREAMBLE_TAIL and fv_rem_read_cnt = 1 then
              if selected = '0' then
                next_state <= idle;
              else
                next_state   <= request_header;
                sample_flits <= '1';
              end if;
            else
              next_state <= reply_data_lsb;
            end if;

            if fv_rem_read_cnt = 0 then
              next_state <= fv_bad_state;
            else
              fv_next_rem_read_cnt <= fv_rem_read_cnt - 1;
            end if;
          end if;
        end if;
      
      when fv_bad_state =>
        null;

      when others =>
        next_state <= idle;

    end case;

    somi(transaction_reg.xindex).w.ready <= slv_ready;
    somi(transaction_reg.xindex).r.valid <= slv_valid;

  end process axi_roundtrip;

  -- Update FSM state
  process (clk, rst)
  begin  -- process
    if rst = '0' then                   -- asynchronous reset (active low)
      current_state <= idle;
      transaction_reg <= transaction_none;
      remote_ahbs_rcv_data_out_hold <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
      current_state <= next_state;
      fv_rem_read_cnt <= fv_next_rem_read_cnt;
      if sample_flits = '1' then
        transaction_reg <= transaction;
      end if;
      if sample_and_hold = '1' then
        remote_ahbs_rcv_data_out_hold <= remote_ahbs_rcv_data_out;
      end if;
    end if;
  end process;

end rtl;
