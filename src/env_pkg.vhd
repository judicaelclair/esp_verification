-- Copyright (c) 2011-2021 Columbia University, System Level Design Group
-- SPDX-License-Identifier: Apache-2.0

-- There is a mixture of code from ESP and code written by Judicael S. E. Clair.
-- That being said, the vast majority of the code is copy-pasted from ESP.  

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;

--pragma translate_off
use std.textio.all;
use ieee.std_logic_textio.all;
--pragma translate_on

package env_pkg is
  constant NINST : integer := 4;
  constant MAX_NMST : integer := 2;

  constant USE_SPANDEX : integer := 0;
  constant ARCH_BITS : integer := 64;
  constant CFG_AHBDW : integer := ARCH_BITS;
  constant AHBDW     : integer := CFG_AHBDW;
  constant AXIDW     : integer := AHBDW;
  constant XID_WIDTH : integer := 10;
  constant XUSER_WIDTH : integer := 10;
  constant GLOB_PHYS_ADDR_BITS : integer := 32;
  constant CFG_NMEM_TILE : integer := 4;
  constant CFG_NSLM_TILE : integer := 1;
  constant CFG_NSLMDDR_TILE : integer := 1;
  constant NAHBSLV   : integer := 16;  -- maximum AHB slaves
  constant inferred    : integer := 0;

  type axi_aw_mosi_type is record      -- Master Output Slave Input
    id     : std_logic_vector (XID_WIDTH-1 downto 0);
    addr   : std_logic_vector (GLOB_PHYS_ADDR_BITS - 1 downto 0);
    len    : std_logic_vector (7 downto 0);
    size   : std_logic_vector (2 downto 0);
    burst  : std_logic_vector (1 downto 0);
    lock   : std_logic;
    cache  : std_logic_vector (3 downto 0);
    prot   : std_logic_vector (2 downto 0);
    valid  : std_logic;
    qos    : std_logic_vector (3 downto 0);
    atop   : std_logic_vector(5 downto 0);
    region : std_logic_vector(3 downto 0);
    user   : std_logic_vector(XUSER_WIDTH-1 downto 0);
  end record;

  type axi_aw_somi_type is record -- Slave Output Master Input
    ready  : std_logic;
  end record;

  type axi_w_mosi_type is record -- Master Output Slave Input
    data    : std_logic_vector (AXIDW-1 downto 0);
    strb    : std_logic_vector (AXIDW/8-1 downto 0);
    last    : std_logic;
    valid   : std_logic;
    user    : std_logic_vector(XUSER_WIDTH-1 downto 0);
  end record;

  type axi_w_somi_type is record -- Slave Output Master Input
    ready   : std_logic;
  end record;

  type axi_ar_mosi_type is record -- Master Output Slave Input
    id     : std_logic_vector (XID_WIDTH-1 downto 0);
    addr   : std_logic_vector (GLOB_PHYS_ADDR_BITS - 1 downto 0);
    len    : std_logic_vector (7 downto 0);
    size   : std_logic_vector (2 downto 0);
    burst  : std_logic_vector (1 downto 0);
    lock   : std_logic;
    cache  : std_logic_vector (3 downto 0);
    prot   : std_logic_vector (2 downto 0);
    valid  : std_logic;
    qos    : std_logic_vector (3 downto 0);
    region : std_logic_vector(3 downto 0);
    user   : std_logic_vector(XUSER_WIDTH-1 downto 0);
  end record;

  type axi_ar_somi_type is record -- Slave Output Master Input
    ready  : std_logic;
  end record;

  type axi_r_mosi_type is record -- Master Output Slave Input
    ready   : std_logic;
  end record;

  type axi_r_somi_type is record -- Slave Output Master Input
    id    : std_logic_vector (XID_WIDTH-1 downto 0);
    data  : std_logic_vector (AXIDW-1 downto 0);
    resp  : std_logic_vector (1 downto 0);
    last  : std_logic;
    valid : std_logic;
    user  : std_logic_vector(XUSER_WIDTH-1 downto 0);
  end record;

  type axi_b_mosi_type is record -- Master Output Slave Input
    ready   : std_logic;
  end record;

  type axi_b_somi_type is record -- Slave Output Master Input
    id    : std_logic_vector (XID_WIDTH-1 downto 0);
    resp  : std_logic_vector (1 downto 0);
    valid : std_logic;
    user  : std_logic_vector(XUSER_WIDTH-1 downto 0);
  end record;

  type axi_mosi_type is record -- Master Output Slave Input
    aw  : axi_aw_mosi_type;
    w   : axi_w_mosi_type;
    b   : axi_b_mosi_type;
    ar  : axi_ar_mosi_type;
    r   : axi_r_mosi_type;
  end record;

  type axi_somi_type is record -- Slave Output Master Input
    aw  : axi_aw_somi_type;
    w   : axi_w_somi_type;
    b   : axi_b_somi_type;
    ar  : axi_ar_somi_type;
    r   : axi_r_somi_type;
  end record;

  type axi_mosi_vector is array (natural range <>) of axi_mosi_type;
  type axi_somi_vector is array (natural range <>) of axi_somi_type;
  subtype axi_mosi_extended_vector is axi_mosi_vector(0 to MAX_NMST - 1);
  subtype axi_somi_extended_vector is axi_somi_vector(0 to MAX_NMST - 1);

  subtype std_ulogic_extended_vector is std_ulogic_vector(0 to MAX_NMST - 1);
  subtype counter_type is std_ulogic_vector(15 downto 0);
  type counter_vector is array (natural range <>) of counter_type;
  subtype counter_extended_vector is counter_vector(0 to MAX_NMST - 1);

  constant HEADER_ROUTE_L : natural := 4;
  constant HEADER_ROUTE_E : natural := 3;
  constant HEADER_ROUTE_W : natural := 2;
  constant HEADER_ROUTE_S : natural := 1;
  constant HEADER_ROUTE_N : natural := 0;

  constant PREAMBLE_WIDTH      : natural := 2;
  constant YX_WIDTH            : natural := 3;
  constant MSG_TYPE_WIDTH      : natural := 5;
  constant RESERVED_WIDTH      : natural := 8;
  constant NEXT_ROUTING_WIDTH  : natural := 5;
  constant NOC_FLIT_SIZE       : natural := PREAMBLE_WIDTH + ARCH_BITS;
  constant MISC_NOC_FLIT_SIZE  : natural := PREAMBLE_WIDTH + 32;

  subtype local_yx is std_logic_vector(2 downto 0);
  subtype noc_preamble_type is std_logic_vector(PREAMBLE_WIDTH-1 downto 0);
  subtype noc_msg_type is std_logic_vector(MSG_TYPE_WIDTH-1 downto 0);
  subtype noc_flit_type is std_logic_vector(NOC_FLIT_SIZE-1 downto 0);
  subtype misc_noc_flit_type is std_logic_vector(33 downto 0);
  subtype reserved_field_type is std_logic_vector(RESERVED_WIDTH-1 downto 0);
  subtype ports_vec is std_logic_vector(4 downto 0);

  type noc_flit_vector is array (natural range <>) of noc_flit_type;
  type misc_noc_flit_vector is array (natural range <>) of misc_noc_flit_type;


  constant noc_flit_pad : std_logic_vector(NOC_FLIT_SIZE - MISC_NOC_FLIT_SIZE - 1 downto 0) := (others => '0');

  -- Preamble encoding
  constant PREAMBLE_HEADER : noc_preamble_type := "10";
  constant PREAMBLE_TAIL   : noc_preamble_type := "01";
  constant PREAMBLE_BODY   : noc_preamble_type := "00";
  constant PREAMBLE_1FLIT  : noc_preamble_type := "11";

  -- -- Message type encoding
  -- -- Cachable data plane 1 -> request messages
  constant REQ_S          : noc_msg_type := "00000";  -- Writer-invalidated Read
  constant REQ_Odata      : noc_msg_type := "00001";  -- Ownership Write (overwrites all requested data)
  constant REQ_WT         : noc_msg_type := "00010";  -- Write-through Write (overwrites all requested data)
  constant REQ_WB         : noc_msg_type := "00011";  -- Write-back owned data
  constant REQ_O          : noc_msg_type := "00100";  -- Ownership Write (returns the value before update)
  constant REQ_V          : noc_msg_type := "00101";  -- Self-invalidated Read
  constant REQ_WTdata     : noc_msg_type := "00110";  -- Write-through Write (returns the value before update)
  constant REQ_AMO_SWAP   : noc_msg_type := "00110";
  constant REQ_AMO_ADD    : noc_msg_type := "00111";
  constant REQ_AMO_AND    : noc_msg_type := "01000";
  constant REQ_AMO_OR     : noc_msg_type := "01001";
  constant REQ_AMO_XOR    : noc_msg_type := "01010";
  constant REQ_AMO_MAX    : noc_msg_type := "01011";
  constant REQ_AMO_MAXU   : noc_msg_type := "01100";
  constant REQ_AMO_MIN    : noc_msg_type := "01101";
  constant REQ_AMO_MINU   : noc_msg_type := "01110";
  constant REQ_WTfwd      : noc_msg_type := "01111";
  -- Cachable data plane 2 -> forwarded messages
  constant FWD_REQ_S      : noc_msg_type := "00000";
  constant FWD_REQ_Odata  : noc_msg_type := "00001";
  constant FWD_INV_SPDX   : noc_msg_type := "00010";
  constant FWD_WB_ACK     : noc_msg_type := "00011";
  constant FWD_RVK_O      : noc_msg_type := "00100";
  constant FWD_REQ_V      : noc_msg_type := "00111";
  constant FWD_REQ_O      : noc_msg_type := "00110";
  constant FWD_WTfwd      : noc_msg_type := "00101";
  -- Cachable data plane 3 -> response messages
  constant RSP_S              : noc_msg_type := "00000";
  constant RSP_Odata          : noc_msg_type := "00001";
  constant RSP_INV_ACK_SPDX   : noc_msg_type := "00010";
  constant RSP_NACK           : noc_msg_type := "00011";
  constant RSP_RVK_O          : noc_msg_type := "00100";
  constant RSP_V              : noc_msg_type := "00101";
  constant RSP_O              : noc_msg_type := "00110";
  constant RSP_WT             : noc_msg_type := "00111";
  constant RSP_WTdata         : noc_msg_type := "01000";

-- ********
-- Original ESP
-- ********


-- -- Message type encoding
  -- -- Cachable data plane 1 -> request messages
  constant REQ_GETS_W   : noc_msg_type := "11000";  --Get Shared (word)
  constant REQ_GETM_W   : noc_msg_type := "11001";  --Get Modified (word)
  constant REQ_PUTS     : noc_msg_type := "11010";  --Put Shared/Exclusive
  constant REQ_PUTM     : noc_msg_type := "11011";  --Put Modified
  constant REQ_GETS_B   : noc_msg_type := "11100";  --Get Shared (Byte)
  constant REQ_GETS_HW  : noc_msg_type := "11101";  --Get Shared (Half Word)
  constant REQ_GETM_B   : noc_msg_type := "11110";  --Get Modified (Byte)
  constant REQ_GETM_HW  : noc_msg_type := "11111";  --Get Modified (Half word)
  -- Cachable data plane 2 -> forwarded messages
  constant FWD_GETS       : noc_msg_type := "11000";
  constant FWD_GETM       : noc_msg_type := "11001";
  constant FWD_INV        : noc_msg_type := "11010";  --Invalidation
  constant FWD_PUT_ACK    : noc_msg_type := "11011";  --Put Acknowledge
  constant FWD_GETM_NOCOH : noc_msg_type := "11100";  --Recall on exclusive/modified
  constant FWD_INV_NOCOH  : noc_msg_type := "11101";  --Recall on shared
  -- Cachable data plane 3 -> response messages
  constant RSP_DATA     : noc_msg_type := "11000";  --CacheLine
  constant RSP_EDATA    : noc_msg_type := "11001";  --Cache Line (Exclusive)
  constant RSP_INV_ACK  : noc_msg_type := "11010";  --Invalidation Acknowledge
  -- [LLC|Non]-Coherent DMA request plane 6 and response plane 4
  constant DMA_TO_DEV    : noc_msg_type := "11001";
  constant DMA_FROM_DEV  : noc_msg_type := "11010";
  constant RSP_P2P       : noc_msg_type := "11100";
  constant REQ_P2P       : noc_msg_type := "11101";
  constant REQ_DMA_READ  : noc_msg_type := "11110";  -- Read coherent with LLC
  constant REQ_DMA_WRITE : noc_msg_type := "11111";  -- Write coherent with LLC
  constant CPU_DMA       : noc_msg_type := "11100";  -- identify DMA from CPU
  -- Configuration plane 5 -> RD/WR registers
  constant REQ_REG_RD   : noc_msg_type := "11000";
  constant REQ_REG_WR   : noc_msg_type := "11001";
  constant AHB_RD       : noc_msg_type := "11010";
  constant AHB_WR       : noc_msg_type := "11011";
  constant IRQ_MSG      : noc_msg_type := "11100";
  constant RSP_REG_RD   : noc_msg_type := "11101";
  constant RSP_AHB_RD   : noc_msg_type := "11110";
  constant INTERRUPT    : noc_msg_type := "11111";

  constant ROUTE_NOC3 : std_logic_vector(1 downto 0) := "01";
  constant ROUTE_NOC4 : std_logic_vector(1 downto 0) := "10";
  constant ROUTE_NOC5 : std_logic_vector(1 downto 0) := "11";

  -- Ports routing encoding
  -- 4 = Local tile
  -- 3 = East
  -- 2 = West
  -- 1 = South
  -- 0 = North

  -- 0 = AN; 1 = CB
  constant FLOW_CONTROL : integer := 0;
  -- FIFOs depth
  constant ROUTER_DEPTH : integer := 4;

  type yx_vec is array (natural range <>) of std_logic_vector(2 downto 0);

  type tile_mem_info is record
    x     : local_yx;
    y     : local_yx;
    haddr : integer;
    hmask : integer;
  end record;

  constant tile_mem_info_none : tile_mem_info := (
    x => (others => '0'),
    y => (others => '0'),
    haddr => 16#000#,
    hmask => 16#fff#
    );

  type tile_mem_info_vector is array (0 to CFG_NMEM_TILE + CFG_NSLM_TILE + CFG_NSLMDDR_TILE - 1) of tile_mem_info;
  constant tile_mem_info_vector_none : tile_mem_info_vector := (others => tile_mem_info_none);

  constant HTRANS_IDLE:   std_logic_vector(1 downto 0) := "00";
  constant HTRANS_BUSY:   std_logic_vector(1 downto 0) := "01";
  constant HTRANS_NONSEQ: std_logic_vector(1 downto 0) := "10";
  constant HTRANS_SEQ:    std_logic_vector(1 downto 0) := "11";

  constant HBURST_SINGLE: std_logic_vector(2 downto 0) := "000";
  constant HBURST_INCR:   std_logic_vector(2 downto 0) := "001";
  constant HBURST_WRAP4:  std_logic_vector(2 downto 0) := "010";
  constant HBURST_INCR4:  std_logic_vector(2 downto 0) := "011";
  constant HBURST_WRAP8:  std_logic_vector(2 downto 0) := "100";
  constant HBURST_INCR8:  std_logic_vector(2 downto 0) := "101";
  constant HBURST_WRAP16: std_logic_vector(2 downto 0) := "110";
  constant HBURST_INCR16: std_logic_vector(2 downto 0) := "111";

  constant HSIZE_BYTE:    std_logic_vector(2 downto 0) := "000";
  constant HSIZE_HWORD:   std_logic_vector(2 downto 0) := "001";
  constant HSIZE_WORD:    std_logic_vector(2 downto 0) := "010";
  constant HSIZE_DWORD:   std_logic_vector(2 downto 0) := "011";
  constant HSIZE_4WORD:   std_logic_vector(2 downto 0) := "100";
  constant HSIZE_8WORD:   std_logic_vector(2 downto 0) := "101";
  constant HSIZE_16WORD:  std_logic_vector(2 downto 0) := "110";
  constant HSIZE_32WORD:  std_logic_vector(2 downto 0) := "111";

  constant HRESP_OKAY:    std_logic_vector(1 downto 0) := "00";
  constant HRESP_ERROR:   std_logic_vector(1 downto 0) := "01";
  constant HRESP_RETRY:   std_logic_vector(1 downto 0) := "10";
  constant HRESP_SPLIT:   std_logic_vector(1 downto 0) := "11";

  constant RBRESP_OKAY:   std_logic_vector(1 downto 0) := "00";
  constant RBRESP_EXOKAY: std_logic_vector(1 downto 0) := "01";
  constant RBRESP_SLVERR: std_logic_vector(1 downto 0) := "10";
  constant RBRESP_DECERR: std_logic_vector(1 downto 0) := "11";

  type axi_fsm is (idle, request_header, request_address,
                  request_length, request_data, reply_header,
                  reply_data, request_data_lsb, request_data_msb,
                  reply_data_lsb, reply_data_msb, request_data_ack,
                  fv_bad_state);

  type transaction_type is record
    -- Selected master interfaces
    xindex                 : integer range 0 to MAX_NMST - 1;
    -- Transaction info from AXI
    write                  : std_ulogic;
    id                     : std_logic_vector (XID_WIDTH-1 downto 0);
    addr                   : std_logic_vector (GLOB_PHYS_ADDR_BITS - 1 downto 0);
    len                    : std_logic_vector (7 downto 0);
    size                   : std_logic_vector (2 downto 0);
    burst                  : std_logic_vector (1 downto 0);
    lock                   : std_logic;
    cache                  : std_logic_vector (3 downto 0);
    prot                   : std_logic_vector (2 downto 0);
    qos                    : std_logic_vector (3 downto 0);
    atop                   : std_logic_vector(5 downto 0);
    region                 : std_logic_vector(3 downto 0);
    user                   : std_logic_vector(XUSER_WIDTH-1 downto 0);
    -- NoC transaction info
    msg_type               : noc_msg_type;
    reserved               : reserved_field_type;
    mem_x                  : local_yx;
    mem_y                  : local_yx;
    addr_msb               : std_logic_vector(11 downto 0);
    hsize_msb              : std_ulogic;  -- distinguish HSIZE_WORD from HSIZE_DWORD
    dst_is_mem             : std_ulogic;
    -- NoC flits
    header                 : noc_flit_type;
    header_narrow          : misc_noc_flit_type;
    payload_address        : noc_flit_type;
    payload_address_narrow : misc_noc_flit_type;
    payload_length         : noc_flit_type;
    payload_length_narrow  : misc_noc_flit_type;
  end record transaction_type;

  constant transaction_none : transaction_type := (
    xindex                 => 0,
    write                  => '0',
    id                     => (others => '0'),
    addr                   => (others => '0'),
    len                    => (others => '0'),
    size                   => (others => '0'),
    burst                  => (others => '0'),
    lock                   => '0',
    cache                  => (others => '0'),
    prot                   => (others => '0'),
    qos                    => (others => '0'),
    atop                   => (others => '0'),
    region                 => (others => '0'),
    user                   => (others => '0'),
    msg_type               => (others => '0'),
    reserved               => (others => '0'),
    mem_x                  => (others => '0'),
    mem_y                  => (others => '0'),
    addr_msb               => (others => '0'),
    hsize_msb              => '0',
    dst_is_mem             => '0',
    header                 => (others => '0'),
    header_narrow          => (others => '0'),
    payload_address        => (others => '0'),
    payload_address_narrow => (others => '0'),
    payload_length         => (others => '0'),
    payload_length_narrow  => (others => '0')
  );

  type array_of_axi_somi_extended_vector is array(0 to NINST - 1) of axi_somi_extended_vector;
  type array_of_noc_flit_type is array(0 to NINST - 1) of noc_flit_type;
  type array_of_misc_noc_flit_type is array(0 to NINST - 1) of misc_noc_flit_type;
  type array_of_ulogic is array(0 to NINST - 1) of std_ulogic;

  function create_header (
    constant flit_sz : integer;
    local_y           : local_yx;
    local_x           : local_yx;
    remote_y          : local_yx;
    remote_x          : local_yx;
    msg_type          : noc_msg_type;
    reserved          : reserved_field_type)
    return std_logic_vector;
  
  function get_preamble (
    constant flit_sz : integer;
    flit : noc_flit_type)
    return noc_preamble_type;
  
  function ahbdrivedata (hdata : std_logic_vector) return std_logic_vector;
end env_pkg;

package body env_pkg is
  -- purpose: Duplicates 'hdata' to suite AHB data width. If the input vector's
  -- length exceeds AHBDW the low part is returned. 
  function ahbdrivedata (
    hdata : std_logic_vector)
    return std_logic_vector is
    variable data : std_logic_vector(AHBDW-1 downto 0);
  begin  -- ahbdrivedata
    if AHBDW < hdata'length then
      data := hdata(AHBDW+hdata'low-1 downto hdata'low);
    else
      for i in 0 to AHBDW/hdata'length-1 loop
        data(hdata'length-1+hdata'length*i downto  hdata'length*i) := hdata; 
      end loop;
    end if;
    return data;
  end ahbdrivedata;

  function get_preamble (
    constant flit_sz : integer;
    flit : noc_flit_type)
    return noc_preamble_type is
    variable ret : noc_preamble_type;
  begin
    ret := flit(flit_sz - 1 downto flit_sz - PREAMBLE_WIDTH);
    return ret;
  end get_preamble;

  function create_header (
    constant flit_sz : integer;
    local_y           : local_yx;
    local_x           : local_yx;
    remote_y          : local_yx;
    remote_x          : local_yx;
    msg_type          : noc_msg_type;
    reserved          : reserved_field_type)
    return std_logic_vector is
    variable header : std_logic_vector(flit_sz - 1 downto 0);
    variable go_left, go_right, go_up, go_down : std_logic_vector(NEXT_ROUTING_WIDTH - 1 downto 0);
  begin  -- create_header
    header := (others => '0');
    header(flit_sz - 1 downto
           flit_sz - PREAMBLE_WIDTH) := PREAMBLE_HEADER;
    header(flit_sz - PREAMBLE_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - YX_WIDTH) := local_y;
    header(flit_sz - PREAMBLE_WIDTH - YX_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - 2*YX_WIDTH) := local_x;
    header(flit_sz - PREAMBLE_WIDTH - 2*YX_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - 3*YX_WIDTH) := remote_y;
    header(flit_sz - PREAMBLE_WIDTH - 3*YX_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - 4*YX_WIDTH) := remote_x;
    header(flit_sz - PREAMBLE_WIDTH - 4*YX_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - 4*YX_WIDTH - MSG_TYPE_WIDTH) := msg_type;
    header(flit_sz - PREAMBLE_WIDTH - 4*YX_WIDTH - MSG_TYPE_WIDTH - 1 downto
           flit_sz - PREAMBLE_WIDTH - 4*YX_WIDTH - MSG_TYPE_WIDTH - RESERVED_WIDTH) := reserved;

    if local_x < remote_x then
      go_right := "01000";
    else
      go_right := "10111";
    end if;

    if local_x > remote_x then
      go_left := "00100";
    else
      go_left := "11011";
    end if;

    if local_y < remote_y then
      header(NEXT_ROUTING_WIDTH - 1 downto 0) := "01110" and go_left and go_right;
    else
      header(NEXT_ROUTING_WIDTH - 1 downto 0) := "01101" and go_left and go_right;
    end if;

    if local_y = remote_y and local_x = remote_x then
      header(NEXT_ROUTING_WIDTH - 1 downto 0) := "10000";
    end if;

    return header;
  end create_header;
end env_pkg;
