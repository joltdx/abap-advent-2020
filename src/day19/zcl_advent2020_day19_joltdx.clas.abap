CLASS zcl_advent2020_day19_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_rules,
        id TYPE i,
        rule TYPE string,
        rule_regex TYPE string,
      END OF ty_rules.
    DATA mt_rules TYPE STANDARD TABLE OF ty_rules.
    DATA mv_input_rules TYPE string.
    DATA mt_messages TYPE STANDARD TABLE OF string.
    METHODS part_1
      RETURNING
        VALUE(result) TYPE i.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE i.

    METHODS parse_rules
      IMPORTING
        rules_input TYPE string.

    METHODS get_regex_for_rule
      IMPORTING
        rule_id TYPE i
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY19_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n\n| INTO mv_input_rules DATA(input_messages).
    parse_rules( mv_input_rules ).
    SPLIT input_messages AT |\n| INTO TABLE mt_messages.

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA(monster_regex) = |^{ get_regex_for_rule( 0 ) }$|.
    LOOP AT mt_messages INTO DATA(message).
      FIND REGEX monster_regex IN message.
      IF sy-subrc = 0.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_2.
    DATA(rule_42) = get_regex_for_rule( 42 ).
    DATA(rule_31) = get_regex_for_rule( 31 ).
    DATA(rule_42_31) = |({ rule_42 }{ rule_31 })|.

    CLEAR mt_rules[].
    parse_rules( mv_input_rules ).
    " 8: 42 | 42 8
    READ TABLE mt_rules WITH KEY id = 8 ASSIGNING FIELD-SYMBOL(<rule>).
    <rule>-rule_regex = |({ rule_42 })+|.

    " 11: 42 31 | 42 11 31
    READ TABLE mt_rules WITH KEY id = 11 ASSIGNING <rule>.
    " <rule>-rule_regex = |(({ rule_42_31 })\|({ rule_42 }(?R)?{ rule_31 }(?0)?))|.
    <rule>-rule_regex = |(|.
    DO 5 TIMES.
      IF sy-index > 1.
        <rule>-rule_regex = |{ <rule>-rule_regex }\||.
      ENDIF.
      <rule>-rule_regex = |{ <rule>-rule_regex }({ rule_42 }\{{ sy-index }\}{ rule_31 }\{{ sy-index }\})|.
    ENDDO.
    <rule>-rule_regex = |{ <rule>-rule_regex })|.

    DATA(monster_regex) = |^{ get_regex_for_rule( 0 ) }$|.
    LOOP AT mt_messages INTO DATA(message).
      FIND REGEX monster_regex IN message.
      IF sy-subrc = 0.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_rules.
    DATA rule_entry TYPE ty_rules.

    SPLIT rules_input AT |\n| INTO TABLE DATA(rules_table).
    LOOP AT rules_table INTO DATA(rules_line).
      FIND REGEX '^(\d+): (.+)$' IN rules_line SUBMATCHES rule_entry-id rule_entry-rule.
      FIND REGEX '"(.)"' IN rule_entry-rule SUBMATCHES rule_entry-rule_regex.
      APPEND rule_entry TO mt_rules.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_regex_for_rule.
    DATA this_rule TYPE string.
    DATA next_step_rule TYPE string.
    READ TABLE mt_rules WITH KEY id = rule_id ASSIGNING FIELD-SYMBOL(<rule>).
    IF <rule>-rule_regex IS NOT INITIAL.
      result = <rule>-rule_regex.
      RETURN.
    ENDIF.

    SPLIT <rule>-rule AT | \| | INTO TABLE DATA(next_rules).
    <rule>-rule_regex = |(|.
    LOOP AT next_rules INTO DATA(next_rule).
      IF sy-tabix > 1.
        <rule>-rule_regex = |{ <rule>-rule_regex }\||.
      ENDIF.
      SPLIT next_rule AT | | INTO TABLE DATA(tokens).
      LOOP AT tokens INTO DATA(token).
        <rule>-rule_regex = |{ <rule>-rule_regex }{ get_regex_for_rule( token ) }|.
      ENDLOOP.
    ENDLOOP.
    <rule>-rule_regex = |{ <rule>-rule_regex })|.
    result = <rule>-rule_regex.
  ENDMETHOD.
ENDCLASS.
