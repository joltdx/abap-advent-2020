CLASS zcl_advent2020_day04_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_passport,
        byr TYPE string,
        iyr TYPE string,
        eyr TYPE string,
        hgt TYPE string,
        hcl TYPE string,
        ecl TYPE string,
        pid TYPE string,
        cid TYPE string,
      END OF ty_passport.

    DATA mt_passports TYPE STANDARD TABLE OF ty_passport WITH EMPTY KEY.

    METHODS set_passport_data
      IMPORTING
        input TYPE string.

    METHODS parse_passport
      IMPORTING
        passport_string TYPE string
      RETURNING
        VALUE(result) TYPE ty_passport.

    METHODS valid_passport
      IMPORTING
        passport TYPE ty_passport
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS valid_passport_data
      IMPORTING
        passport TYPE ty_passport
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_advent2020_day04_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    set_passport_data( input ).

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD set_passport_data.
    DATA lt_input TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lv_passport_string TYPE string.
    DATA lv_input_line TYPE string.

    SPLIT input AT |\n| INTO TABLE lt_input.
    LOOP AT lt_input REFERENCE INTO lv_input_line.
      IF lv_input_line IS INITIAL.
        APPEND parse_passport( lv_passport_string ) TO mt_passports.
        CLEAR lv_passport_string.
        CONTINUE.
      ENDIF.

      lv_passport_string = |{ lv_passport_string }{ lv_input_line } |.

    ENDLOOP.
    IF lv_passport_string IS NOT INITIAL.
      APPEND parse_passport( lv_passport_string ) TO mt_passports.
    ENDIF.

  ENDMETHOD.

  METHOD parse_passport.
    FIND REGEX 'byr:(\S+) ' IN passport_string SUBMATCHES result-byr.
    FIND REGEX 'iyr:(\S+) ' IN passport_string SUBMATCHES result-iyr.
    FIND REGEX 'eyr:(\S+) ' IN passport_string SUBMATCHES result-eyr.
    FIND REGEX 'hgt:(\S+) ' IN passport_string SUBMATCHES result-hgt.
    FIND REGEX 'hcl:(\S+) ' IN passport_string SUBMATCHES result-hcl.
    FIND REGEX 'ecl:(\S+) ' IN passport_string SUBMATCHES result-ecl.
    FIND REGEX 'pid:(\S+) ' IN passport_string SUBMATCHES result-pid.
    FIND REGEX 'cid:(\S+) ' IN passport_string SUBMATCHES result-cid.
  ENDMETHOD.

  METHOD valid_passport.
    DATA fields_present TYPE i.

    IF passport-byr IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-iyr IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-eyr IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-hgt IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-hcl IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-ecl IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF passport-pid IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

    IF fields_present = 7.
      result = abap_true.
    ENDIF.

    " Optional (for now?)
    IF passport-cid IS NOT INITIAL.
      fields_present = fields_present + 1.
    ENDIF.

  ENDMETHOD.

  METHOD valid_passport_data.
    DATA integer TYPE i.

    integer = passport-byr.
    IF integer < 1920 OR integer > 2002.
      RETURN.
    ENDIF.

    integer = passport-iyr.
    IF integer < 2010 OR integer > 2020.
      RETURN.
    ENDIF.

    integer = passport-eyr.
    IF integer < 2020 OR integer > 2030.
      RETURN.
    ENDIF.

    FIND REGEX '(\d+)in' IN passport-hgt SUBMATCHES integer.
    IF sy-subrc = 0.
      IF integer < 59 OR integer > 76.
        RETURN.
      ENDIF.
    ELSE.
      FIND REGEX '(\d+)cm' IN passport-hgt SUBMATCHES integer.
      IF sy-subrc = 0.
        IF integer < 150 OR integer > 193.
          RETURN.
        ENDIF.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    FIND REGEX '^#[0-9a-f]{6}$' IN passport-hcl.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE passport-ecl.
      WHEN 'amb' OR
           'blu' OR
           'brn' OR
           'gry' OR
           'grn' OR
           'hzl' OR
           'oth'.
        " Fine.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    FIND REGEX '^[0-9]{9}$' IN passport-pid.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.

  METHOD part_1.
    DATA valid_count TYPE i.

    LOOP AT mt_passports INTO DATA(passport).
      IF valid_passport( passport ) = abap_true.
        valid_count = valid_count + 1.
      ENDIF.
    ENDLOOP.

    result = valid_count.

  ENDMETHOD.

  METHOD part_2.

    DATA valid_count TYPE i.

    LOOP AT mt_passports INTO DATA(passport).
      IF valid_passport( passport ) = abap_true AND
          valid_passport_data( passport ) = abap_true.
        valid_count = valid_count + 1.
      ENDIF.
    ENDLOOP.

    result = valid_count.

  ENDMETHOD.
ENDCLASS.