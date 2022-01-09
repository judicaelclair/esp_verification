-- MIT License
-- 
-- Copyright (c) 2022 Judicael S. E. Clair
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.env_pkg.all;

entity top is
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
    mosi                       : in  axi_mosi_extended_vector;
    somi                       : out array_of_axi_somi_extended_vector;
    -- tile->NoC1
    coherence_req_wrreq        : out array_of_ulogic;
    coherence_req_data_in      : out array_of_noc_flit_type;
    coherence_req_full         : in  std_ulogic;
    -- Noc3->tile
    coherence_rsp_rcv_rdreq    : out array_of_ulogic;
    coherence_rsp_rcv_data_out : in  noc_flit_type;
    coherence_rsp_rcv_empty    : in  std_ulogic;
    -- tile->NoC5
    remote_ahbs_snd_wrreq      : out array_of_ulogic;
    remote_ahbs_snd_data_in    : out array_of_misc_noc_flit_type;
    remote_ahbs_snd_full       : in  std_ulogic;
    -- NoC5->tile
    remote_ahbs_rcv_rdreq      : out array_of_ulogic;
    remote_ahbs_rcv_data_out   : in  misc_noc_flit_type;
    remote_ahbs_rcv_empty      : in  std_ulogic);
end;

architecture rtl of top is
begin
    gen_nmst:
    for nmst in 1 to 2 generate
      gen_mem_num:
      for mem_num in 1 to 2 generate
        gen_i:
        for i in 2 * (nmst - 1) + (mem_num - 1) to 2 * (nmst - 1) + (mem_num - 1) generate
          inst : entity work.axislv2noc
            generic map (mem_num => mem_num, nmst => nmst)
            port map (retarget_for_dma => retarget_for_dma,
                      mem_axi_port => mem_axi_port,
                      mem_info => mem_info,
                      slv_y => slv_y,
                      slv_x => slv_x,
                      rst => rst,
                      clk => clk,
                      local_y => local_y,
                      local_x => local_x,
                      mosi_extended => mosi,
                      somi_extended => somi(i),
                      -- tile->NoC1
                      coherence_req_wrreq => coherence_req_wrreq(i),
                      coherence_req_data_in => coherence_req_data_in(i),
                      coherence_req_full => coherence_req_full,
                      -- Noc3->tile
                      coherence_rsp_rcv_rdreq => coherence_rsp_rcv_rdreq(i),
                      coherence_rsp_rcv_data_out => coherence_rsp_rcv_data_out,
                      coherence_rsp_rcv_empty => coherence_rsp_rcv_empty,
                      -- tile->NoC5
                      remote_ahbs_snd_wrreq => remote_ahbs_snd_wrreq(i),
                      remote_ahbs_snd_data_in => remote_ahbs_snd_data_in(i),
                      remote_ahbs_snd_full => remote_ahbs_snd_full,
                      -- NoC5->tile
                      remote_ahbs_rcv_rdreq => remote_ahbs_rcv_rdreq(i),
                      remote_ahbs_rcv_data_out => remote_ahbs_rcv_data_out,
                      remote_ahbs_rcv_empty => remote_ahbs_rcv_empty);
        end generate;
      end generate;
    end generate;
end rtl;
