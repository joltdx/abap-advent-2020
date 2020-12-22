CLASS zcl_advent2020_day21_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_allergens,
        done TYPE abap_bool,
        allergen TYPE string,
        ingredient TYPE string,
      END OF ty_allergens.

    DATA mt_allergens TYPE STANDARD TABLE OF ty_allergens.
    DATA mv_all_the_things TYPE string.
    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS determine_allergens.

    METHODS remove_unique_from_others
      IMPORTING
        ingredient TYPE string.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY21_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.
    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    SPLIT input AT |\n| INTO TABLE DATA(lines).
    LOOP AT lines INTO DATA(line).
      FIND REGEX '(.*) \(contains (.*)\)' IN line SUBMATCHES DATA(ingredients) DATA(allergens).
      mv_all_the_things = |{ mv_all_the_things }{ ingredients } |.
      SPLIT allergens AT |, | INTO TABLE DATA(allergen_tab).
      LOOP AT allergen_tab INTO DATA(allergen).
        READ TABLE mt_allergens WITH KEY allergen = allergen ASSIGNING FIELD-SYMBOL(<allergen>).
        IF sy-subrc <> 0.
          APPEND VALUE #( allergen = allergen ) TO mt_allergens.
          READ TABLE mt_allergens WITH KEY allergen = allergen ASSIGNING <allergen>.
        ENDIF.
        IF <allergen>-ingredient IS NOT INITIAL.
          <allergen>-ingredient = |{ <allergen>-ingredient }\||.
        ENDIF.
        <allergen>-ingredient = |{ <allergen>-ingredient }{ ingredients }|.
      ENDLOOP.
    ENDLOOP.

    determine_allergens( ).

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }\n|.

  ENDMETHOD.

  METHOD part_1.
    DATA ingredients TYPE string.
    DATA(lv_all_the_things) = mv_all_the_things.

    LOOP AT mt_allergens INTO DATA(allergen).
      REPLACE ALL OCCURRENCES OF allergen-ingredient IN lv_all_the_things WITH ||.
      result = |{ result }{ allergen-allergen }: { allergen-ingredient }\n|.
    ENDLOOP.
    CONDENSE lv_all_the_things.
    REPLACE ALL OCCURRENCES OF |  | IN lv_all_the_things WITH | |.
    FIND ALL OCCURRENCES OF | | IN lv_all_the_things MATCH COUNT DATA(match_count).
    result = |{ result }\nAppearance of cant-possibly-contain: { match_count + 1 }|.
  ENDMETHOD.

  METHOD part_2.
    DATA cdil TYPE string.

    LOOP AT mt_allergens INTO DATA(allergen).
      IF sy-tabix = 1.
        cdil = |{ allergen-ingredient }|.
      ELSE.
        cdil = |{ cdil },{ allergen-ingredient }|.
      ENDIF.
    ENDLOOP.
    result = |Canonical dangerous ingredient list: { cdil }|.
  ENDMETHOD.

  METHOD determine_allergens.
    DATA count TYPE i.
    DATA source_count TYPE i.
    DATA unique_match TYPE string.

    LOOP AT mt_allergens ASSIGNING FIELD-SYMBOL(<allergen>) WHERE done = abap_false.
      SPLIT <allergen>-ingredient AT |\|| INTO TABLE DATA(lines).
      READ TABLE lines INDEX 1 INTO DATA(first_line).
      SPLIT first_line AT | | INTO TABLE DATA(ingredients).
      source_count = lines( lines ).
      CLEAR unique_match.
      LOOP AT ingredients INTO DATA(ingredient).
        CLEAR count.
        LOOP AT lines INTO DATA(line).
          IF line CS ingredient.
            count = count + 1.
          ENDIF.
        ENDLOOP.
        IF count = source_count.
          IF unique_match IS NOT INITIAL.
            CLEAR unique_match.
            EXIT.
          ELSE.
            unique_match = ingredient.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF unique_match IS NOT INITIAL.
        remove_unique_from_others( unique_match ).
        <allergen>-ingredient = unique_match.
        <allergen>-done = abap_true.
      ENDIF.
    ENDLOOP.

    READ TABLE mt_allergens WITH KEY done = abap_false.
    IF sy-subrc = 0.
      determine_allergens( ).
    ENDIF.

    SORT mt_allergens BY allergen.
  ENDMETHOD.

  METHOD remove_unique_from_others.
    LOOP AT mt_allergens ASSIGNING FIELD-SYMBOL(<allergen>) WHERE ingredient CS ingredient.
      REPLACE ALL OCCURRENCES OF ingredient IN <allergen>-ingredient WITH ||.
      REPLACE ALL OCCURRENCES OF REGEX '(\|\|)|(\| )' IN <allergen>-ingredient WITH |\||.
      REPLACE ALL OCCURRENCES OF REGEX '(^\|)|(\|$)' IN <allergen>-ingredient WITH ||.
      REPLACE ALL OCCURRENCES OF |  | IN <allergen>-ingredient WITH | |.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
