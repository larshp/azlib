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

CLASS lcl_zlib DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      compress
        IMPORTING iv_raw               TYPE xsequence
        RETURNING VALUE(rv_compressed) TYPE xstring,
      decompress
        IMPORTING iv_compressed TYPE xsequence
                  iv_expected   TYPE i OPTIONAL
        RETURNING VALUE(rv_raw) TYPE xstring.

  PRIVATE SECTION.
    CLASS-DATA: mv_bits TYPE string.

    CLASS-METHODS:
      map_length
        IMPORTING iv_code          TYPE i
        RETURNING VALUE(rv_length) TYPE i,
      map_distance
        IMPORTING iv_code            TYPE i
        RETURNING VALUE(rv_distance) TYPE i,
      take_bits
        IMPORTING iv_count       TYPE i
        RETURNING VALUE(rv_bits) TYPE string,
      hex_to_bits
        IMPORTING iv_hex         TYPE xsequence
        RETURNING VALUE(rv_bits) TYPE string,
      bits_to_int
        IMPORTING iv_bits       TYPE clike
        RETURNING VALUE(rv_int) TYPE i,
      read_pair
        EXPORTING
          ev_length   TYPE i
          ev_distance TYPE i,
      int_to_hex
        IMPORTING iv_int        TYPE i
        RETURNING VALUE(rv_hex) TYPE xstring.

ENDCLASS.

CLASS lcl_zlib IMPLEMENTATION.

  METHOD take_bits.

    rv_bits = mv_bits(iv_count).
    mv_bits = mv_bits+iv_count.

  ENDMETHOD.

  METHOD int_to_hex.

    DATA: lv_x TYPE x.


    lv_x = iv_int.
    rv_hex = lv_x.

  ENDMETHOD.

  METHOD bits_to_int.

    DATA: lv_c    TYPE c LENGTH 1,
          lv_x    TYPE x LENGTH 1,
          lv_bits TYPE string.

    lv_bits = iv_bits.

    IF strlen( lv_bits ) < 8.
      CONCATENATE '0' lv_bits INTO lv_bits.
    ENDIF.

    WHILE NOT lv_bits IS INITIAL.
      lv_c = lv_bits.
      SET BIT sy-index OF lv_x TO lv_c.
      lv_bits = lv_bits+1.
    ENDWHILE.

    rv_int = lv_x.

  ENDMETHOD.

  METHOD hex_to_bits.

    DATA: lv_x   TYPE x LENGTH 1,
          lv_c   TYPE c LENGTH 1,
          lv_bit TYPE i,
          lv_hex TYPE xstring.


    lv_hex = iv_hex.
    WHILE NOT lv_hex IS INITIAL.
      lv_x = lv_hex.
      DO 8 TIMES.
        lv_bit = 9 - sy-index.
        GET BIT lv_bit OF lv_x INTO lv_c.
        CONCATENATE rv_bits lv_c INTO rv_bits.
      ENDDO.
      lv_hex = lv_hex+1.
    ENDWHILE.

  ENDMETHOD.

  METHOD compress.

    ASSERT NOT iv_raw IS INITIAL.

* todo
  ENDMETHOD.

  METHOD read_pair.

    DATA: lv_tmp TYPE string,
          lv_int TYPE i.


    lv_tmp = take_bits( 7 ).
    lv_int = bits_to_int( lv_tmp ).
    lv_int = lv_int + 256.
    ev_length = map_length( lv_int ).

    lv_tmp = take_bits( 5 ).
    lv_int = bits_to_int( lv_tmp ).
    ev_distance = map_distance( lv_int ).

  ENDMETHOD.

  METHOD map_distance.

*0   0    1
*1   0    2
*2   0    3
*3   0    4
*4   1   5,6
*5   1   7,8
*6   2   9-12
*7   2  13-16
*8   3  17-24
*9   3  25-32
*10   4     33-48
*11   4     49-64
*12   5     65-96
*13   5     97-128
*14   6    129-192
*15   6    193-256
*16   7    257-384
*17   7    385-512
*18   8    513-768
*19   8   769-1024
*20    9   1025-1536
*21    9   1537-2048
*22   10   2049-3072
*23   10   3073-4096
*24   11   4097-6144
*25   11   6145-8192
*26   12  8193-12288
*27   12 12289-16384
*28   13 16385-24576
*29   13 24577-32768

  ENDMETHOD.

  METHOD map_length.

    DATA: lv_tmp TYPE string.


    DEFINE _length.
      lv_tmp = take_bits( &1 ).
      rv_length = bits_to_int( lv_tmp ).
      rv_length = rv_length + &2.
    END-OF-DEFINITION.

    CASE iv_code.
      WHEN '257'.
        rv_length = 3.
      WHEN '258'.
        rv_length = 4.
      WHEN '259'.
        rv_length = 5.
      WHEN '260'.
        rv_length = 6.
      WHEN '261'.
        rv_length = 7.
      WHEN '262'.
        rv_length = 8.
      WHEN '263'.
        rv_length = 9.
      WHEN '264'.
        rv_length = 10.
      WHEN '265'.
        _length 1 11.
      WHEN '266'.
        _length 1 13.
      WHEN '267'.
        _length 1 15.
      WHEN '268'.
        _length 1 17.
      WHEN '269'.
        _length 2 19.
      WHEN '270'.
        _length 2 23.
      WHEN '271'.
        _length 2 27.
      WHEN '272'.
        _length 2 31.
      WHEN '273'.
        _length 3 35.
      WHEN '274'.
        _length 3 43.
      WHEN '275'.
        _length 3 51.
      WHEN '276'.
        _length 3 59.
      WHEN '277'.
        _length 4 67.
      WHEN '278'.
        _length 4 83.
      WHEN '279'.
        _length 4 99.
      WHEN '280'.
        _length 4 115.
      WHEN '281'.
        _length 5 131.
      WHEN '282'.
        _length 5 163.
      WHEN '283'.
        _length 5 195.
      WHEN '284'.
        _length 5 227.
      WHEN '285'.
        rv_length = 258.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.

  METHOD decompress.

    DATA: lv_int        TYPE i,
          lv_x          TYPE x LENGTH 1,
          lv_length     TYPE i,
          lv_distance   TYPE i,
          lv_tmp        TYPE string,
          lv_compressed TYPE xstring.


    IF iv_compressed IS INITIAL.
      RETURN.
    ENDIF.

    mv_bits = hex_to_bits( iv_compressed ).

* skip zlib header, todo
    take_bits( 16 ).

    lv_tmp = take_bits( 3 ).
    WRITE: / 'BFINAL =', lv_tmp(1).
    WRITE: / 'BTYPE  =', lv_tmp+1(2), '(wrong endianness)'.

    WHILE strlen( mv_bits ) > 0.
      WRITE: / mv_bits.
      IF mv_bits >= '00110000' AND mv_bits <= '10111111'.
        lv_tmp = take_bits( 8 ).
        lv_int = bits_to_int( lv_tmp ).
        lv_int = lv_int - 48.

        lv_x = int_to_hex( lv_int ).
        WRITE: / lv_x.
        CONCATENATE rv_raw lv_x INTO rv_raw IN BYTE MODE.
      ELSEIF mv_bits >= '110010000' AND mv_bits <= '111111111'.
        BREAK-POINT.
        EXIT.
      ELSEIF mv_bits = '0000000'.
* todo, end of block
        BREAK-POINT.
      ELSEIF mv_bits > '0000000' AND mv_bits <= '0010111'.
        read_pair(
          IMPORTING
            ev_length   = lv_length
            ev_distance = lv_distance ).
        BREAK-POINT.
        EXIT.
      ELSEIF mv_bits >= '11000000' AND mv_bits <= '11000111'.
        BREAK-POINT.
        EXIT.
      ENDIF.
* todo
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_zlib DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      identity FOR TESTING,
      decompress FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.

CLASS ltcl_zlib IMPLEMENTATION.

  METHOD identity.

    DATA: lv_compressed TYPE xstring,
          lv_raw        TYPE xstring.

    CONSTANTS: c_raw TYPE xstring VALUE '00010203040506070809'.


    lv_compressed = lcl_zlib=>compress( c_raw ).
    lv_raw = lcl_zlib=>decompress( lv_compressed ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).
    cl_abap_unit_assert=>assert_equals( act = lv_raw
                                        exp = c_raw ).

  ENDMETHOD.

  METHOD decompress.

    DATA: lv_raw TYPE xstring.

    CONSTANTS:
      c_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      c_compressed TYPE xstring VALUE '789CF348CDC9C95708CF2FCA4951E4E5020024E90455'.


    lv_raw = lcl_zlib=>decompress( c_compressed ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).
    cl_abap_unit_assert=>assert_equals( act = lv_raw
                                        exp = c_raw ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    CONSTANTS:
*      c_compressed TYPE xstring VALUE '789CF348CDC9C95708CF2FCA4951E4E5020024E90455'.
      c_compressed TYPE xstring VALUE '789C0B492D2EC9CC4B0F815000'.


    DATA(lv_raw) = lcl_zlib=>decompress( c_compressed ).
    WRITE: / 'decompressed:', lv_raw.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).