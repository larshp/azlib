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
    CLASS-METHODS:
      to_bits
        IMPORTING iv_hex         TYPE xsequence
        RETURNING VALUE(rv_bits) TYPE string,
      to_int
        IMPORTING iv_bits       TYPE clike
        RETURNING VALUE(rv_int) TYPE i.

ENDCLASS.

CLASS lcl_zlib IMPLEMENTATION.

  METHOD to_int.

    DATA: lv_c    TYPE c LENGTH 1,
          lv_x    TYPE x LENGTH 1,
          lv_bits TYPE string.


    lv_bits = iv_bits.
    WHILE NOT lv_bits IS INITIAL.
      lv_c = lv_bits.
      SET BIT sy-index OF lv_x TO lv_c.
      lv_bits = lv_bits+1.
    ENDWHILE.

    rv_int = lv_x.

  ENDMETHOD.

  METHOD to_bits.

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

  METHOD decompress.

    DATA: lv_bits TYPE string,
          lv_tmp  TYPE xstring.


    IF iv_compressed IS INITIAL.
      RETURN.
    ENDIF.

    lv_tmp = iv_compressed+2.
    lv_bits = to_bits( lv_tmp ).

    WRITE: / 'BFINAL =', lv_bits(1).
    WRITE: / 'BTYPE  =', lv_bits+1(2), '(wrong endianness)'.
    lv_bits = lv_bits+3.

    WRITE: / lv_bits(8).

    IF lv_bits >= '00110000' AND lv_bits <= '10111111'.
      WRITE: / '1yes'.

      lv_bits = lv_bits(8). " todo
      DATA(lv_int) = to_int( lv_bits ).
      WRITE: / lv_int.
    ENDIF.
    IF lv_bits >= '110010000' AND lv_bits <= '111111111'.
      WRITE: / '2yes'.
    ENDIF.
    IF lv_bits >= '0000000' AND lv_bits <= '0010111'.
      WRITE: / '3yes'.
    ENDIF.
    IF lv_bits >= '11000000' AND lv_bits <= '11000111'.
      WRITE: / '4yes'.
    ENDIF.
* todo

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


    lcl_zlib=>decompress( c_compressed ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).