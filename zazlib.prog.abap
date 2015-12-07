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
      count
        IMPORTING iv_index TYPE i
        RETURNING value(rv_value) TYPE i,
      symbol
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

  METHOD count.
    READ TABLE mt_count INDEX iv_index INTO rv_value.     "#EC CI_SUBRC
  ENDMETHOD.                    "count

  METHOD symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.                    "symbol

  METHOD constructor.

*    TYPES: BEGIN OF ty_offset,
*             value TYPE i,
*             index TYPE i,
*           END OF ty_offset.
*
    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_prev type i,
          lv_count LIKE LINE OF mt_count,
          lv_value  TYPE i.

    FIELD-SYMBOLS: <ls_offset> LIKE LINE OF lt_offset,
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

    LOOP AT mt_count ASSIGNING <lv_i>.
      WRITE: / 'h count', sy-tabix, <lv_i>.
    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
      WRITE: / 'offset', lv_prev.
    ENDDO.

* todo

*    LOOP AT it_lengths INTO lv_value.
*      lv_index = sy-tabix.
*      APPEND INITIAL LINE TO lt_offset ASSIGNING <ls_offset>.
*      <ls_offset>-value = lv_value.
*      <ls_offset>-index = sy-tabix - 1.
*    ENDLOOP.
*    SORT lt_offset BY value ASCENDING index ASCENDING.
*    LOOP AT lt_offset ASSIGNING <ls_offset>.
*      APPEND <ls_offset>-index TO mt_symbol.
*    ENDLOOP.

    LOOP AT mt_symbol ASSIGNING <lv_i>.
      WRITE: / 'h symbol', sy-tabix, <lv_i>.
    ENDLOOP.

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
      lv_count = io_huffman->count( lv_len ).

*      WRITE: / 'code', lv_code, 'count', lv_count, 'first', lv_first, 'len', lv_len.
      IF lv_code - lv_count < lv_first.
        rv_symbol = io_huffman->symbol( lv_index + lv_code - lv_first + 1 ).
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
      lv_x = gv_out+lv_index(1).
      CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
    ENDDO.

  ENDMETHOD.                    "copy_out

  METHOD dynamic.

    DATA: lv_nlen TYPE i,
          lv_ndist TYPE i,
          lv_ncode TYPE i,
          lv_index TYPE i,
          lv_foo TYPE i,
          lt_order TYPE TABLE OF i,
          lt_lengths TYPE lcl_zlib_huffman=>ty_lengths.

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
    WRITE: / 'dynamic'.
    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

  ENDMETHOD.                    "dynamic

  METHOD take_bits.

    DATA: lv_left TYPE i,
          lv_index TYPE i,
          lv_x TYPE x LENGTH 1.


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
          lv_x    TYPE x LENGTH 1,
          lv_bits TYPE string.

    lv_bits = iv_bits.

    WHILE strlen( lv_bits ) < 8.
      CONCATENATE '0' lv_bits INTO lv_bits.
    ENDWHILE.

    WHILE NOT lv_bits IS INITIAL.
      lv_c = lv_bits.
      SET BIT sy-index OF lv_x TO lv_c.
      lv_bits = lv_bits+1.
    ENDWHILE.

    rv_int = lv_x.

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
          lv_int TYPE i.

    rs_pair-length = map_length( iv_length ).

    lv_tmp = reverse_bits( 5 ).
    lv_int = bits_to_int( lv_tmp ).
    rs_pair-distance = map_distance( lv_int ).

  ENDMETHOD.                    "read_pair

  METHOD map_distance.

    DATA: lv_tmp TYPE string.

    DEFINE _distance.
      lv_tmp = reverse_bits( &1 ).
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
      lv_tmp = reverse_bits( &1 ).
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
          fixed( ).
        WHEN '10'.
          dynamic( ).
          RETURN. " todo
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.

      DO.
        lv_symbol = decode( go_lencode ).
*        WRITE: / 'symbol =', lv_symbol.

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

    _hex '62601805A360148C8251300A46C1281805A360148C8251300A46C1281805'.
    _hex 'A360148C8251300A46C1281805A360148C8251300A46C1281805A360148C'.
    _hex '825130A400802038100000000000E2BF1A00000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '0000B20707240000000082FEBF6E47A00200000000000000003013000900'.
    _hex 'F6FF000000000000000000EDDC810000000080207FEB05462890EC5F0000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '000000000000000000000000000000000000000000000000000000000000'.
    _hex '00000000000000000000000000000000000000000000000000C008'.

    lv_raw = lcl_zlib=>decompress( lv_compressed ).
*    WRITE: / 'decompressed:', lv_raw.

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_app IMPLEMENTATION

START-OF-SELECTION.
  lcl_app=>run( ).