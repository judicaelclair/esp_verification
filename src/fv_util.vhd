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

entity fv_util is
  generic (
    nmst : integer := MAX_NMST
  );
  port (
    rst                                               : in std_ulogic;
    clk                                               : in std_ulogic;
    mosi                                              : in axi_mosi_vector(0 to nmst - 1);
    coherence_rsp_rcv_empty                           : in std_ulogic;
    coherence_rsp_rcv_rdreq                           : in std_ulogic;
    coherence_req_wrreq                               : in std_ulogic;
    coherence_req_data_in                             : in noc_flit_type;
    remote_ahbs_rcv_empty                             : in std_ulogic;
    remote_ahbs_rcv_rdreq                             : in std_ulogic;
    remote_ahbs_snd_wrreq                             : in std_ulogic;
    remote_ahbs_snd_data_in                           : in misc_noc_flit_type;
    sample_flits                                      : in std_ulogic;
    transaction_reg                                   : in transaction_type
    );
end fv_util;

architecture rtl of fv_util is
  signal coherence_empty_low_cnt : counter_type;
  signal misc_empty_low_cnt : counter_type;
  signal coherence_rdreq_high_cnt : counter_type;
  signal misc_rdreq_high_cnt : counter_type;
  signal coherence_empty_low_mosi_r_ready_high_cnt : counter_extended_vector;
  signal misc_empty_low_mosi_r_ready_high_cnt : counter_extended_vector;
  signal coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt : counter_extended_vector;
  signal misc_empty_low_rdreq_high_mosi_r_ready_high_cnt : counter_extended_vector;
  signal first_coherence_empty_low_had_mosi_r_ready_high : std_ulogic_extended_vector;
  signal first_misc_empty_low_had_mosi_r_ready_high : std_ulogic_extended_vector;
  signal read_req_length : std_ulogic_vector(7 downto 0);
  signal transaction_cnt : counter_type; -- used to restrict state space

  signal prev_sample_flits : std_ulogic; -- store sample_flits of previous clock cycle
begin
  process (clk, rst)
  begin
    if rst = '0' then
      prev_sample_flits <= '0';

      first_coherence_empty_low_had_mosi_r_ready_high <= (others => '0');
      first_misc_empty_low_had_mosi_r_ready_high <= (others => '0');
      coherence_empty_low_cnt <= (others => '0');
      misc_empty_low_cnt <= (others => '0');
      coherence_rdreq_high_cnt <= (others => '0');
      misc_rdreq_high_cnt <= (others => '0');
      coherence_empty_low_mosi_r_ready_high_cnt <= (others => (others => '0'));
      misc_empty_low_mosi_r_ready_high_cnt <= (others => (others => '0'));
      coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt <= (others => (others => '0'));
      misc_empty_low_rdreq_high_mosi_r_ready_high_cnt <= (others => (others => '0'));
      read_req_length <= (others => '0');
      transaction_cnt <= (others => '0');
    elsif rising_edge(clk) then
      if prev_sample_flits = '1' then -- delayed by 1 clock cycle so we can check values at the end of a transaction
        first_coherence_empty_low_had_mosi_r_ready_high <= (others => '0');
        first_misc_empty_low_had_mosi_r_ready_high <= (others => '0');
        coherence_empty_low_cnt <= (others => '0');
        misc_empty_low_cnt <= (others => '0');
        coherence_rdreq_high_cnt <= (others => '0');
        misc_rdreq_high_cnt <= (others => '0');
        coherence_empty_low_mosi_r_ready_high_cnt <= (others => (others => '0'));
        misc_empty_low_mosi_r_ready_high_cnt <= (others => (others => '0'));
        coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt <= (others => (others => '0'));
        misc_empty_low_rdreq_high_mosi_r_ready_high_cnt <= (others => (others => '0'));
        read_req_length <= (others => '0');
        transaction_cnt <= transaction_cnt + 1;
      else
        if not coherence_rsp_rcv_empty then
          coherence_empty_low_cnt <= coherence_empty_low_cnt + 1;
        end if;
        if not remote_ahbs_rcv_empty then
          misc_empty_low_cnt <= misc_empty_low_cnt + 1;
        end if;
        if coherence_rsp_rcv_rdreq then
          coherence_rdreq_high_cnt <= coherence_rdreq_high_cnt + 1;
        end if;
        if remote_ahbs_rcv_rdreq then
          misc_rdreq_high_cnt <= misc_rdreq_high_cnt + 1;
        end if;

        if coherence_req_wrreq then
          if get_preamble(NOC_FLIT_SIZE, coherence_req_data_in) = PREAMBLE_TAIL then
            read_req_length <= coherence_req_data_in(7 downto 0);
          end if;
        end if;

        if remote_ahbs_snd_wrreq then
          if get_preamble(MISC_NOC_FLIT_SIZE, noc_flit_pad & remote_ahbs_snd_data_in) = PREAMBLE_TAIL then
            read_req_length <= remote_ahbs_snd_data_in(7 downto 0);
          end if;
        end if;

        for i in 0 to nmst - 1 loop
          if (not coherence_rsp_rcv_empty) and mosi(i).r.ready then
            coherence_empty_low_mosi_r_ready_high_cnt(i) <= coherence_empty_low_mosi_r_ready_high_cnt(i) + 1;
          end if;
          if (not remote_ahbs_rcv_empty) and mosi(i).r.ready then
            misc_empty_low_mosi_r_ready_high_cnt(i) <= misc_empty_low_mosi_r_ready_high_cnt(i) + 1;
          end if;
          if (not coherence_rsp_rcv_empty) and coherence_rsp_rcv_rdreq and mosi(i).r.ready then
            coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt(i) <= coherence_empty_low_rdreq_high_mosi_r_ready_high_cnt(i) + 1;
          end if;
          if (not remote_ahbs_rcv_empty) and remote_ahbs_rcv_rdreq and mosi(i).r.ready then
            misc_empty_low_rdreq_high_mosi_r_ready_high_cnt(i) <= misc_empty_low_rdreq_high_mosi_r_ready_high_cnt(i) + 1;
          end if;

          if ieee.numeric_std.unsigned(coherence_empty_low_cnt) = 0 then
            if not coherence_rsp_rcv_empty then
              first_coherence_empty_low_had_mosi_r_ready_high(i) <= mosi(i).r.ready;
            end if;
          end if;
          if ieee.numeric_std.unsigned(misc_empty_low_cnt) = 0 then
            if not remote_ahbs_rcv_empty then
              first_misc_empty_low_had_mosi_r_ready_high(i) <= mosi(i).r.ready;
            end if;
          end if;
        end loop;
      end if;
      prev_sample_flits <= sample_flits;
    end if;
  end process;
end rtl;
