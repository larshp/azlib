REPORT zazlib.
********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2015 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

* todo, refactor to classes: bitstream, conversion?

CONSTANTS: c_maxbits TYPE i VALUE 15.

DATA: gv_output TYPE abap_bool.

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_huffman DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_huffman DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_lengths TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING it_lengths TYPE ty_lengths,
      get_count
        IMPORTING iv_index TYPE i
        RETURNING value(rv_value) TYPE i,
      get_symbol
        IMPORTING iv_index TYPE i
        RETURNING value(rv_value) TYPE i.

  PRIVATE SECTION.

    DATA: mt_count  TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          mt_symbol TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

ENDCLASS.                    "lcl_zlib_huffman DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_huffman DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_huffman IMPLEMENTATION.

  METHOD get_count.
    READ TABLE mt_count INDEX iv_index INTO rv_value.     "#EC CI_SUBRC
  ENDMETHOD.                    "count

  METHOD get_symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.                    "symbol

  METHOD constructor.

    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_length LIKE LINE OF it_lengths,
          lv_prev   TYPE i,
          lv_count  LIKE LINE OF mt_count.
*          lv_value  TYPE i.

    FIELD-SYMBOLS: <lv_offset> LIKE LINE OF lt_offset,
                   <lv_symbol> LIKE LINE OF mt_symbol,
                   <lv_i>      LIKE LINE OF it_lengths.


    DO c_maxbits TIMES.
      APPEND 0 TO mt_count.
    ENDDO.
    LOOP AT it_lengths INTO lv_index.
      IF lv_index = 0.
        CONTINUE.
      ENDIF.
      READ TABLE mt_count INDEX lv_index ASSIGNING <lv_i>.
      ASSERT sy-subrc = 0.
      <lv_i> = <lv_i> + 1.
    ENDLOOP.

*    LOOP AT mt_count ASSIGNING <lv_i>.
*      WRITE: / 'h count', sy-tabix, <lv_i>.
*    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
*      WRITE: / 'offset', lv_prev.
    ENDDO.

    DO lines( it_lengths ) TIMES.
      APPEND 0 TO mt_symbol.
    ENDDO.
    DO lines( it_lengths ) TIMES.
      lv_index = sy-index.
      READ TABLE it_lengths INDEX lv_index INTO lv_length.
      ASSERT sy-subrc = 0.
      IF lv_length = 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_offset INDEX lv_length ASSIGNING <lv_offset>.
      ASSERT sy-subrc = 0.
      READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
      ASSERT sy-subrc = 0.
      <lv_symbol> = lv_index - 1.
      <lv_offset> = <lv_offset> + 1.
    ENDDO.

*    LOOP AT mt_symbol ASSIGNING <lv_i>.
*      WRITE: / 'h symbol', sy-tabix, <lv_i>.
*    ENDLOOP.

  ENDMETHOD.                    "constructor

ENDCLASS.                    "lcl_zlib_huffman DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      decompress
        IMPORTING iv_compressed TYPE xsequence
        RETURNING value(rv_raw) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_fixlcodes TYPE i VALUE 288,
               c_maxdcodes TYPE i VALUE 30.

    CLASS-DATA: gv_compressed TYPE xstring,
                gv_out        TYPE xstring,
                go_lencode    TYPE REF TO lcl_zlib_huffman,
                go_distcode   TYPE REF TO lcl_zlib_huffman,
                gv_bits       TYPE string.

    TYPES: BEGIN OF ty_pair,
             length TYPE i,
             distance TYPE i,
           END OF ty_pair.

    CLASS-METHODS:
      decode
        IMPORTING io_huffman TYPE REF TO lcl_zlib_huffman
        RETURNING value(rv_symbol) TYPE i,
      map_length
        IMPORTING iv_code          TYPE i
        RETURNING value(rv_length) TYPE i,
      map_distance
        IMPORTING iv_code            TYPE i
        RETURNING value(rv_distance) TYPE i,
      dynamic,
      fixed,
      take_bits
        IMPORTING iv_count       TYPE i
        RETURNING value(rv_bits) TYPE string,
      reverse_bits
        IMPORTING iv_count       TYPE i
        RETURNING value(rv_bits) TYPE string,
      hex_to_bits
        IMPORTING iv_hex         TYPE xsequence
        RETURNING value(rv_bits) TYPE string,
      bits_to_int
        IMPORTING iv_bits       TYPE clike
        RETURNING value(rv_int) TYPE i,
      read_pair
        IMPORTING iv_length TYPE i
        RETURNING value(rs_pair) TYPE ty_pair,
      copy_out
        IMPORTING is_pair TYPE ty_pair,
      int_to_hex
        IMPORTING iv_int        TYPE i
        RETURNING value(rv_hex) TYPE xstring,
      reverse
        IMPORTING iv_string TYPE string
        RETURNING value(rv_string) TYPE string.

ENDCLASS.                    "lcl_zlib DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib IMPLEMENTATION.

  METHOD decode.

    DATA: lv_bit   TYPE c LENGTH 1,
          lv_len   TYPE i,
          lv_count TYPE i,
          lv_code  TYPE i,
          lv_index TYPE i,
          lv_first TYPE i,
          lv_bits  TYPE string.


    DO c_maxbits TIMES.
      lv_len = sy-index.

      lv_bit = take_bits( 1 ).
      CONCATENATE lv_bits lv_bit INTO lv_bits.
      lv_code = bits_to_int( lv_bits ).
      lv_count = io_huffman->get_count( lv_len ).

*      IF gv_output = abap_true.
*        WRITE: / 'code', lv_code, 'count', lv_count, 'first', lv_first, 'len', lv_len, 'bit', lv_bit, 'index', lv_index.
*      ENDIF.
      IF lv_code - lv_count < lv_first.
*        DATA: lv_foo TYPE i.
*        lv_foo = lv_index + lv_code - lv_first + 1.
*        IF gv_output = abap_true.
*          BREAK-POINT.
*        ENDIF.
        rv_symbol = io_huffman->get_symbol( lv_index + lv_code - lv_first + 1 ).
        RETURN.
      ENDIF.
      lv_index = lv_index + lv_count.
      lv_first = lv_first + lv_count.
      lv_first = lv_first * 2.
    ENDDO.

  ENDMETHOD.                    "decode

  METHOD reverse_bits.

    DATA: lv_bit TYPE c LENGTH 1.


    DO iv_count TIMES.
      lv_bit = take_bits( 1 ).
      CONCATENATE rv_bits lv_bit INTO rv_bits.
    ENDDO.

  ENDMETHOD.                    "reverse_bits

  METHOD fixed.

    DATA: lt_lengths TYPE lcl_zlib_huffman=>ty_lengths.


    DO 144 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.
    DO 112 TIMES.
      APPEND 9 TO lt_lengths.
    ENDDO.
    DO 24 TIMES.
      APPEND 7 TO lt_lengths.
    ENDDO.
    DO 8 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.


    CLEAR lt_lengths.
    DO c_maxdcodes TIMES.
      APPEND 5 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_lengths.

  ENDMETHOD.                    "fixed

  METHOD reverse.

    DATA: lv_len    TYPE i,
          lv_char   TYPE c LENGTH 1,
          lv_string TYPE string.


    lv_string = iv_string.

    WHILE lv_string <> ''.
      lv_len = strlen( lv_string ) - 1.
      lv_char = lv_string+lv_len(1).
      CONCATENATE rv_string lv_char INTO rv_string.
      lv_string = lv_string(lv_len).
    ENDWHILE.

  ENDMETHOD.                    "reverse

  METHOD copy_out.

* copy one byte at a time, it is not possible to copy using
* string offsets, as it might copy data that does not exist
* in mv_out yet

    DATA: lv_distance TYPE i,
          lv_index    TYPE i,
          lv_x        TYPE x LENGTH 1.


    lv_distance = xstrlen( gv_out ) - is_pair-distance.
    DO is_pair-length TIMES.
      lv_index = sy-index - 1 + lv_distance.
      IF lv_index > xstrlen( gv_out ) OR lv_index < 0.
        BREAK-POINT.
        RETURN.
      ENDIF.
      lv_x = gv_out+lv_index(1).
      CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
    ENDDO.

  ENDMETHOD.                    "copy_out

  METHOD dynamic.

    DATA: lv_nlen    TYPE i,
          lv_ndist   TYPE i,
          lv_ncode   TYPE i,
          lv_index   TYPE i,
*          lv_foo     TYPE i,
          lv_length  TYPE i,
          lv_symbol  TYPE i,
          lt_order   TYPE TABLE OF i,
          lt_lengths TYPE lcl_zlib_huffman=>ty_lengths,
          lt_dists   TYPE lcl_zlib_huffman=>ty_lengths.

    FIELD-SYMBOLS: <lv_length> LIKE LINE OF lt_lengths.


    APPEND 16 TO lt_order.
    APPEND 17 TO lt_order.
    APPEND 18 TO lt_order.
    APPEND  0 TO lt_order.
    APPEND  8 TO lt_order.
    APPEND  7 TO lt_order.
    APPEND  9 TO lt_order.
    APPEND  6 TO lt_order.
    APPEND 10 TO lt_order.
    APPEND  5 TO lt_order.
    APPEND 11 TO lt_order.
    APPEND  4 TO lt_order.
    APPEND 12 TO lt_order.
    APPEND  3 TO lt_order.
    APPEND 13 TO lt_order.
    APPEND  2 TO lt_order.
    APPEND 14 TO lt_order.
    APPEND  1 TO lt_order.
    APPEND 15 TO lt_order.

    lv_nlen = bits_to_int( take_bits( 5 ) ) + 257.
    lv_ndist = bits_to_int( take_bits( 5 ) ) + 1.
    lv_ncode = bits_to_int( take_bits( 4 ) ) + 4.

    DO 19 TIMES.
      APPEND 0 TO lt_lengths.
    ENDDO.

    DO lv_ncode TIMES.
      READ TABLE lt_order INDEX sy-index INTO lv_index.
      lv_index = lv_index + 1.
      READ TABLE lt_lengths INDEX lv_index ASSIGNING <lv_length>.
      ASSERT sy-subrc = 0.
      <lv_length> = bits_to_int( take_bits( 3 ) ).
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CLEAR lt_lengths.
    WHILE lines( lt_lengths ) < lv_nlen + lv_ndist.
      lv_symbol = decode( go_lencode ).
*      DATA: lv_indexx TYPE i.
*      lv_indexx = lines( lt_lengths ).
*      WRITE: / 'dynamic symbol:', lv_symbol, 'index', lv_indexx.
      IF lv_symbol < 16.
        APPEND lv_symbol TO lt_lengths.
      ELSE.
        lv_length = 0.
        IF lv_symbol = 16.
          READ TABLE lt_lengths INDEX lines( lt_lengths ) INTO lv_length.
          lv_symbol = bits_to_int( take_bits( 2 ) ) + 3.
        ELSEIF lv_symbol = 17.
          lv_symbol = bits_to_int( take_bits( 3 ) ) + 3.
        ELSE.
          lv_symbol = bits_to_int( take_bits( 7 ) ) + 11.
        ENDIF.
*        WRITE: / 'ssymbol', lv_symbol, 'length', lv_length.
        DO lv_symbol TIMES.
          APPEND lv_length TO lt_lengths.
        ENDDO.
      ENDIF.
    ENDWHILE.

    lt_dists = lt_lengths.
    DELETE lt_lengths FROM lv_nlen + 1.
    DELETE lt_dists TO lv_nlen.

*    DATA: lv_foo TYPE i.
*    LOOP AT lt_dists INTO lv_foo.
*      WRITE: / 'dist', sy-tabix, lv_foo.
*    ENDLOOP.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_dists.
*    DO lv_ndist TIMES.
*      DATA: lv_foo TYPE i.
*      lv_foo = go_distcode->get_symbol( sy-index ).
*      WRITE: / 'dist symbols:', sy-index, lv_foo.
*    ENDDO.
  ENDMETHOD.                    "dynamic

  METHOD take_bits.

    DATA: lv_left  TYPE i,
          lv_index TYPE i,
          lv_x     TYPE x LENGTH 1.


    WHILE strlen( rv_bits ) < iv_count.
      IF gv_bits IS INITIAL.
        lv_x = gv_compressed(1).
        gv_bits = hex_to_bits( lv_x ).
        gv_compressed = gv_compressed+1.
      ENDIF.
      lv_left = iv_count - strlen( rv_bits ).
      IF lv_left >= strlen( gv_bits ).
        CONCATENATE gv_bits rv_bits INTO rv_bits.
        CLEAR gv_bits.
      ELSE.
        lv_index = strlen( gv_bits ) - lv_left.
        CONCATENATE gv_bits+lv_index(lv_left) rv_bits INTO rv_bits.
        gv_bits = gv_bits(lv_index).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.                    "take_bits

  METHOD int_to_hex.

    DATA: lv_x TYPE x.


    lv_x = iv_int.
    rv_hex = lv_x.

  ENDMETHOD.                    "int_to_hex

  METHOD bits_to_int.

    DATA: lv_c    TYPE c LENGTH 1,
          lv_bits TYPE string.

    lv_bits = iv_bits.

    WHILE NOT lv_bits IS INITIAL.
      lv_c = lv_bits.
      rv_int = rv_int * 2.
      rv_int = rv_int + lv_c.
      lv_bits = lv_bits+1.
    ENDWHILE.

  ENDMETHOD.                    "bits_to_int

  METHOD hex_to_bits.

    DATA: lv_x   TYPE x LENGTH 1,
          lv_c   TYPE c LENGTH 1,
          lv_bit TYPE i,
          lv_hex TYPE xstring.


    lv_hex = iv_hex.
    WHILE NOT lv_hex IS INITIAL.
      lv_x = lv_hex.
      DO 8 TIMES.
        lv_bit = sy-index.
        GET BIT lv_bit OF lv_x INTO lv_c.
        CONCATENATE rv_bits lv_c INTO rv_bits.
      ENDDO.
      lv_hex = lv_hex+1.
    ENDWHILE.

  ENDMETHOD.                    "hex_to_bits

  METHOD read_pair.

    DATA: lv_tmp TYPE string,
          lv_symbol TYPE i,
          lv_int TYPE i.

*    IF gv_output = abap_true.
*      BREAK-POINT.
*    ENDIF.
    rs_pair-length = map_length( iv_length ).
*    IF gv_output = abap_true.
*      WRITE: / 'len =', rs_pair-length, 'start dist decode'.
*    ENDIF.

    lv_symbol = decode( go_distcode ).
*    IF gv_output = abap_true.
*      WRITE: / 'dist symbol =', lv_symbol.
*    ENDIF.
*    lv_tmp = reverse_bits( 5 ).
*    lv_int = bits_to_int( lv_tmp ).
    rs_pair-distance = map_distance( lv_symbol ).
*    IF gv_output = abap_true.
*      WRITE: / 'dist =', rs_pair-distance.
*    ENDIF.

  ENDMETHOD.                    "read_pair

  METHOD map_distance.

    DATA: lv_tmp TYPE string.

    DEFINE _distance.
      lv_tmp = take_bits( &1 ).
      rv_distance = bits_to_int( lv_tmp ).
      rv_distance = rv_distance + &2.
    END-OF-DEFINITION.

    CASE iv_code.
      WHEN 0.
        _distance 0 1.
      WHEN 1.
        _distance 0 2.
      WHEN 2.
        _distance 0 3.
      WHEN 3.
        _distance 0 4.
      WHEN 4.
        _distance 1 5.
      WHEN 5.
        _distance 1 7.
      WHEN 6.
        _distance 2 9.
      WHEN 7.
        _distance 2 13.
      WHEN 8.
        _distance 3 17.
      WHEN 9.
        _distance 3 25.
      WHEN 10.
        _distance 4 33.
      WHEN 11.
        _distance 4 49.
      WHEN 12.
        _distance 5 65.
      WHEN 13.
        _distance 5 97.
      WHEN 14.
        _distance 6 129.
      WHEN 15.
        _distance 6 193.
      WHEN 16.
        _distance 7 257.
      WHEN 17.
        _distance 7 385.
      WHEN 18.
        _distance 8 513.
      WHEN 19.
        _distance 8 769.
      WHEN 20.
        _distance 9 1025.
      WHEN 21.
        _distance 9 1537.
      WHEN 22.
        _distance 10 2049.
      WHEN 23.
        _distance 10 3073.
      WHEN 24.
        _distance 11 4097.
      WHEN 25.
        _distance 11 6145.
      WHEN 26.
        _distance 12 8193.
      WHEN 27.
        _distance 12 12289.
      WHEN 28.
        _distance 13 16385.
      WHEN 29.
        _distance 13 24577.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.                    "map_distance

  METHOD map_length.

    DATA: lv_tmp TYPE string.


    DEFINE _length.
      lv_tmp = take_bits( &1 ).
      rv_length = bits_to_int( lv_tmp ).
      rv_length = rv_length + &2.
    END-OF-DEFINITION.

    CASE iv_code.
      WHEN 257.
        _length 0 3.
      WHEN 258.
        _length 0 4.
      WHEN 259.
        _length 0 5.
      WHEN 260.
        _length 0 6.
      WHEN 261.
        _length 0 7.
      WHEN 262.
        _length 0 8.
      WHEN 263.
        _length 0 9.
      WHEN 264.
        _length 0 10.
      WHEN 265.
        _length 1 11.
      WHEN 266.
        _length 1 13.
      WHEN 267.
        _length 1 15.
      WHEN 268.
        _length 1 17.
      WHEN 269.
        _length 2 19.
      WHEN 270.
        _length 2 23.
      WHEN 271.
        _length 2 27.
      WHEN 272.
        _length 2 31.
      WHEN 273.
        _length 3 35.
      WHEN 274.
        _length 3 43.
      WHEN 275.
        _length 3 51.
      WHEN 276.
        _length 3 59.
      WHEN 277.
        _length 4 67.
      WHEN 278.
        _length 4 83.
      WHEN 279.
        _length 4 99.
      WHEN 280.
        _length 4 115.
      WHEN 281.
        _length 5 131.
      WHEN 282.
        _length 5 163.
      WHEN 283.
        _length 5 195.
      WHEN 284.
        _length 5 227.
      WHEN 285.
        _length 0 258.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.                    "map_length

  METHOD decompress.

    DATA: lv_x      TYPE x LENGTH 1,
          lv_symbol TYPE i,
          lv_bfinal TYPE c LENGTH 1,
          lv_btype  TYPE c LENGTH 2.


    IF iv_compressed IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR gv_out.
    gv_compressed = iv_compressed.

    DO.
      lv_bfinal = take_bits( 1 ).

      lv_btype = take_bits( 2 ).
      CASE lv_btype.
        WHEN '01'.
*          WRITE: / 'fixed'.
          fixed( ).
        WHEN '10'.
          WRITE: / 'dynamic'.
          dynamic( ).
*          RETURN. " todo
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.

      DO.
        lv_symbol = decode( go_lencode ).
        IF xstrlen( gv_out ) >= 462.
          gv_output = abap_true.
        ENDIF.
        IF gv_output = abap_true.
          DATA: lv_len TYPE i.
          lv_len = xstrlen( gv_out ).
          WRITE: / lv_len, 'symbol =', lv_symbol.
        ENDIF.

        IF lv_symbol < 256.
          lv_x = int_to_hex( lv_symbol ).
          CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
        ELSEIF lv_symbol = 256.
          EXIT.
        ELSE.
          copy_out( read_pair( lv_symbol ) ).
        ENDIF.

      ENDDO.

      IF lv_bfinal = '1'.
        EXIT.
      ENDIF.

    ENDDO.

    rv_raw = gv_out.

  ENDMETHOD.                    "decompress

ENDCLASS.                    "lcl_zlib IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_zlib DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      decompress FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.                    "ltcl_zlib DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_zlib IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib IMPLEMENTATION.

  METHOD decompress.

    DATA: lv_raw TYPE xstring.

    CONSTANTS:
      c_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      c_compressed TYPE xstring VALUE 'F348CDC9C95708CF2FCA4951E4E5020024E90455'.


    lv_raw = lcl_zlib=>decompress( c_compressed ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).
    cl_abap_unit_assert=>assert_equals( act = lv_raw
                                        exp = c_raw ).

  ENDMETHOD.                    "decompress

ENDCLASS.                    "ltcl_zlib IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    CONSTANTS:
*     c_compressed TYPE xstring VALUE 'D5586D6FDB3610FE9E5F71D83034CDE6BC10'.
      c_compressed TYPE xstring VALUE '0B492D2EC9CC4B0F815000'.

    DATA: lv_compressed TYPE xstring,
          lv_hex TYPE xstring,
          lv_raw TYPE xstring.


    DEFINE _hex.
      lv_hex = &1.
      concatenate lv_compressed lv_hex into lv_compressed in byte mode.
    END-OF-DEFINITION.

*    _hex '62601805A360148C8251300A46C1281805A360148C8251300A46C1281805'.
*    _hex 'A360148C8251300A46C1281805A360148C8251300A46C1281805A360148C'.
*    _hex '825130A400802038100000000000E2BF1A00000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '0000B20707240000000082FEBF6E47A00200000000000000003013000900'.
*    _hex 'F6FF000000000000000000EDDC810000000080207FEB05462890EC5F0000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '000000000000000000000000000000000000000000000000000000000000'.
*    _hex '00000000000000000000000000000000000000000000000000C008'.

    _hex '75555D6FE246147DF7AFB8521F1610A0AA8F911A'.
    _hex 'C9C1A6B5969034711A352FCE605FF074CD8C3B33'.
    _hex '86657FFD9E191302DBEC8B857DBFCFB9F7F00BED'.
    _hex '2BD1EC8A52ABB5DC448FF13D3DF38A92836A8DA6'.
    _hex 'F806EFF1E26FEACD9D114E9B288A46A34C59279A'.
    _hex '66348A46946D5B6D1CB99A49AFFEE5D2595A1BBD'.
    _hex '25D56D36EC682D1BA64E556CA8118EADA38D7464'.
    _hex 'B8616161B0526D087517D9F23392C5A5933BB811'.
    _hex 'B29FA7846966D81B9E13126DDBC85238A9156A69'.
    _hex '432F68B298DDDDDEE3B19C677FF81613DEEAD0DF'.
    _hex '8771EF61497A7B87ECE9572E3BA4F7639CE53FB3'.
    _hex '9C8370EE42831FCB0F7DB7B5501B26ABB77C848F'.
    _hex '76A2E9D892501559B163383D70233A55D6FD00BE'.
    _hex '938BC44E93E5BEA532A4B301FC27CB186C427155'.
    _hex '013F812A7A1D32A010B850ACC086469DC3F9875A'.
    _hex '3878A3BE2719ECA9CA37D2B3FB16358111483E27'.
    _hex '451EDF2C52D408442B273DF27ECCB38C3EBC15D6'.
    _hex '861CC73E02FFBE1D6DE4462AD104239A09B12B34'.
    _hex '045E3DE35239366B51326DD9D51A88B02BFC2686'.
    _hex '44538AA2D7D757B1126DD4E8A2DCB6FD77FA9D08'.
    _hex '1B5BB85ADAC9357E942D462ABE217040C36994CD'.
    _hex 'E9DC7D725D0B5B08BF528C153F4E094FCA1E295B'.
    _hex '6679162FA61152FE1055864DBB889846E932C9E6'.
    _hex 'D3284AE23CF6FEA709B017CEE8A6C182E7FFDCA7'.
    _hex 'F490CE29BFA36F725F4A3F124DFD0C1FB9FF7F1A'.
    _hex 'B92EDFA7F949D8E4FA02AB4118E0049D8708E54E'.
    _hex 'EFB00218801981CD396E05875D7580442BB2252B'.
    _hex '61A4B657E0F98D88A77B4C9816C94D317FB8BB3D'.
    _hex '1E94A7F404207E9D26BEF81E489E505973F985E4'.
    _hex '3ADC52D919E317D21EACE32D494B07DD19AA78C7'.
    _hex '8D6EB7EFA629E5609558D9CE604B9DDFD7177262'.
    _hex 'D5F89B31908AB6C21656B497AEF62D40B07CCF82'.
    _hex 'F0599056CD015B4509EF8EB5CE978868F6673AFB'.
    _hex '0CCB048DC88ACA963E25A34F3D3213CA8D5036C8'.
    _hex 'D8DEE0FA41E52C14B80A906988823966B5BE8817'.
    _hex '09770A6985AB077FC5E32C1F3FE5E3FB6438262B'.
    _hex 'B72DFA79F709117E160CE88CC44841235F72DCD4'.
    _hex 'E86DCA3DCABCA945E10E2D83CC5F7FA3C1F3454F'.
    _hex '43AF6E4BED7A11B8F5870C593EE0A6B9BDFA998E'.
    _hex 'D1E081FFEBA4017E5E300396FE0E4F087F8CEA90'.
    _hex '560C7F26D329E5FD7B09F0D8F7FF0614B7AD9789'.
    _hex '75B0AC21664755F6FF003D4947C74123BF304DA0'.
    _hex 'FCA536204077CA8D69DD41E72D0E8ED598F86BC9'.
    _hex '0D9E9E8861E01CCAB1626ABB55236D8DDE012316'.
    _hex '558082338D26C5DEA4A986AA92D215440566DFAE'.
    _hex '9F75FEB49CE5D9DDB24817E96DBACC1F7B176C2E'.
    _hex '86F14BF21DD2B86583A502789C33343030333151'.
    _hex '284A4D4CC94DD5CB4D61E8F9CF526533B9F99E89'.
    _hex 'F5DD0E11137B1563BF8D9300E5B40D39B801789C'.
    _hex '0BCBCF4CD15308CFCCC951484A554849CD492D49'.
    _hex '050A700100650107AFB643789C5D524B8FDA3010'.
    _hex 'BEFB578C38ED4AD1F621F5D29B49CC6235C49163'.
    _hex '96720C8921AE428C62A768FF7D6702BBDB564242'.
    _hex 'F3F85EE398CEC2461AC85D638760E1018B47C652'.
    _hex '7F791DDDA98BF0D03CC2D7CF5FBE81AEBBA9077E'.
    _hex 'AD47CB5869C7B30BC1F9015C80CE8EF6F00AA7B1'.
    _hex '1EA26D13388ED6823F42D3D5E3C926103DD4C32B'.
    _hex '5CEC1810E00FB176831B4E5043833A0C37638734'.
    _hex 'C11F23D1E3720B7508BE7135F241EB9BE96C8758'.
    _hex '47D23BBADE067888687C51DD118BC759A4B575CF'.
    _hex 'DC00347B1BC1D5C5CE4F11461BE2E81AE248C00D'.
    _hex '4D3FB5E4E16DDCBBB3BB2B107C0E1F18924E0113'.
    _hex '90CF04CEBE7547FAB773ACCB74E85DE812681D51'.
    _hex '1FA688CD40CDF99609E5F8E44708B6EF193238F4'.
    _hex '3D67FD7037EF90F50B1D34DE4F14A873EDFCF9DF'.
    _hex '242EB0E3340E2869674CEBF164B3E22FDB44EAD0'.

*    TYPES: ty_hex20 TYPE x LENGTH 20.
*    DATA: lv_tmp LIKE lv_compressed,
*          lt_data TYPE TABLE OF ty_hex20.
*    lv_tmp = lv_compressed.
*    WHILE xstrlen( lv_tmp ) > 0.
*      APPEND lv_tmp(20) TO lt_data.
*      lv_tmp = lv_tmp+20.
*    ENDWHILE.
*
*    cl_gui_frontend_services=>gui_download(
*      EXPORTING
*        bin_filesize              = xstrlen( lv_compressed )
*        filename                  = 'foo.raw'
*        filetype                  = 'BIN'
*      CHANGING
*        data_tab                  = lt_data
*      EXCEPTIONS
*        file_write_error          = 1
*        no_batch                  = 2
*        gui_refuse_filetransfer   = 3
*        invalid_type              = 4
*        no_authority              = 5
*        unknown_error             = 6
*        header_not_allowed        = 7
*        separator_not_allowed     = 8
*        filesize_not_allowed      = 9
*        header_too_long           = 10
*        dp_error_create           = 11
*        dp_error_send             = 12
*        dp_error_write            = 13
*        unknown_dp_error          = 14
*        access_denied             = 15
*        dp_out_of_memory          = 16
*        disk_full                 = 17
*        dp_timeout                = 18
*        file_not_found            = 19
*        dataprovider_exception    = 20
*        control_flush_error       = 21
*        not_supported_by_gui      = 22
*        error_no_gui              = 23
*        OTHERS                    = 24
*           ).
*    IF sy-subrc <> 0.
*      BREAK-POINT.
*    ENDIF.

    lv_raw = lcl_zlib=>decompress( lv_compressed ).
*    WRITE: / 'decompressed:', lv_raw.

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_app IMPLEMENTATION

START-OF-SELECTION.
  lcl_app=>run( ).