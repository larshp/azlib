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

ENDCLASS.

CLASS lcl_zlib IMPLEMENTATION.

  METHOD compress.

    ASSERT NOT iv_raw IS INITIAL.

* todo
  ENDMETHOD.

  METHOD decompress.

    IF iv_compressed IS INITIAL.
      RETURN.
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
    WRITE: / 'todo'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).