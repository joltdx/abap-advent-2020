CLASS zcl_advent2020_day17_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_dim_x,
        x TYPE i,
        s TYPE c,
      END OF ty_dim_x.
    TYPES ty_dim_x_tt TYPE STANDARD TABLE OF ty_dim_x WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_dim_y,
        y TYPE i,
        x TYPE ty_dim_x_tt,
      END OF ty_dim_y.
    TYPES ty_dim_y_tt TYPE STANDARD TABLE OF ty_dim_y WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_dim_z,
        z TYPE i,
        y TYPE ty_dim_y_tt,
      END OF ty_dim_z.
    TYPES ty_dim_z_tt TYPE STANDARD TABLE OF ty_dim_z WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_dim_w,
        w TYPE i,
        z TYPE ty_dim_z_tt,
      END OF ty_dim_w.
    TYPES ty_dim_w_tt TYPE STANDARD TABLE OF ty_dim_w WITH EMPTY KEY.

    TYPES ty_space_tt TYPE ty_dim_z_tt.
    TYPES ty_space_p2_tt TYPE ty_dim_w_tt.

    DATA: mt_map TYPE ty_space_tt.
    DATA: mt_map_p2 TYPE ty_space_p2_tt.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS init_map
      IMPORTING
        input TYPE string
        buffer TYPE i.

    METHODS init_map_p2
      IMPORTING
        input TYPE string
        buffer TYPE i.

    METHODS part_1_cycle.

    METHODS part_2_cycle.

    METHODS count_neighbours_up_to_4
      IMPORTING
        z TYPE i
        y TYPE i
        x TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS count_neighbours_up_to_4_p2
      IMPORTING
        w TYPE i
        z TYPE i
        y TYPE i
        x TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS print_the_map
      RETURNING
        VALUE(result) TYPE string.

    METHODS count_the_map
      RETURNING
        VALUE(result) TYPE i.

    METHODS count_the_map_p2
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY17_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    init_map( input = input
              buffer = 7 ).

    init_map_p2( input = input
                 buffer = 8 ).

    output = |\nPart 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    DO 6 TIMES.
      part_1_cycle( ).
    ENDDO.

    result = |{ count_the_map( ) }|.

    RETURN.
    print_the_map( ).
  ENDMETHOD.

  METHOD part_2.

    DO 6 TIMES.
      part_2_cycle( ).
    ENDDO.

    result = |{ count_the_map_p2( ) }|.

  ENDMETHOD.

  METHOD init_map.
    DATA x TYPE i.
    DATA y TYPE i.
    DATA z TYPE i.
    DATA s TYPE c.
    DATA dim_x TYPE ty_dim_x.
    DATA dim_y TYPE ty_dim_y.
    DATA t_dim_y TYPE ty_dim_y_tt.
    DATA dim_z TYPE ty_dim_z.

    SPLIT input AT |\n| INTO TABLE DATA(input_lines).
    DATA(height) = lines( input_lines ).
    READ TABLE input_lines INDEX 1 INTO DATA(first_line).
    DATA(width) = strlen( first_line ).

    DO buffer * 2 TIMES.
      z = sy-index.
      dim_z-z = z.
      CLEAR dim_z-y.
      DO ( buffer * 2 ) + height TIMES.
        y = sy-index.
        dim_y-y = y.
        CLEAR dim_y-x.
        DO ( buffer * 2 ) + width TIMES.
          x = sy-index.
          APPEND VALUE #( x = x
                          s = |.| ) TO dim_y-x.
        ENDDO.
        APPEND dim_y TO dim_z-y.
      ENDDO.
      APPEND dim_z TO mt_map.
    ENDDO.

    READ TABLE mt_map WITH KEY z = buffer ASSIGNING FIELD-SYMBOL(<init_z>).
    LOOP AT input_lines INTO DATA(input_line).
      y = sy-tabix + buffer.
      DO strlen( input_line ) TIMES.
        x = sy-index - 1.
        s = input_line+x(1).
        IF s = |#|.
          READ TABLE <init_z>-y WITH KEY y = y ASSIGNING FIELD-SYMBOL(<init_y>).
          READ TABLE <init_y>-x WITH KEY x = x + buffer + 1 ASSIGNING FIELD-SYMBOL(<init_x>).
          <init_x>-s = s.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD init_map_p2.
    DATA x TYPE i.
    DATA y TYPE i.
    DATA z TYPE i.
    DATA w TYPE i.
    DATA s TYPE c.
    DATA dim_x TYPE ty_dim_x.
    DATA dim_y TYPE ty_dim_y.
    DATA dim_z TYPE ty_dim_z.
    DATA dim_w TYPE ty_dim_w.

    SPLIT input AT |\n| INTO TABLE DATA(input_lines).
    DATA(height) = lines( input_lines ).
    READ TABLE input_lines INDEX 1 INTO DATA(first_line).
    DATA(width) = strlen( first_line ).

    DO buffer * 2 TIMES.
      w = sy-index.
      dim_w-w = w.
      CLEAR dim_w-z.
      DO buffer * 2 TIMES.
        z = sy-index.
        dim_z-z = z.
        CLEAR dim_z-y.
        DO ( buffer * 2 ) + height TIMES.
          y = sy-index.
          dim_y-y = y.
          CLEAR dim_y-x.
          DO ( buffer * 2 ) + width TIMES.
            x = sy-index.
            APPEND VALUE #( x = x
                            s = |.| ) TO dim_y-x.
          ENDDO.
          APPEND dim_y TO dim_z-y.
        ENDDO.
        APPEND dim_z TO dim_w-z.
      ENDDO.
      APPEND dim_w TO mt_map_p2.
    ENDDO.

    READ TABLE mt_map_p2 WITH KEY w = buffer ASSIGNING FIELD-SYMBOL(<init_w>).
    READ TABLE <init_w>-z WITH KEY z = buffer ASSIGNING FIELD-SYMBOL(<init_z>).
    LOOP AT input_lines INTO DATA(input_line).
      y = sy-tabix + buffer.
      DO strlen( input_line ) TIMES.
        x = sy-index - 1.
        s = input_line+x(1).
        IF s = |#|.
          READ TABLE <init_z>-y WITH KEY y = y ASSIGNING FIELD-SYMBOL(<init_y>).
          READ TABLE <init_y>-x WITH KEY x = x + buffer + 1 ASSIGNING FIELD-SYMBOL(<init_x>).
          <init_x>-s = s.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD print_the_map.
    DATA(lt_map) = mt_map.
    result = |\n|.
    LOOP AT lt_map ASSIGNING FIELD-SYMBOL(<z>).
      result = |{ result }\nz={ <z>-z }\n|.
      LOOP AT <z>-y INTO DATA(y).
        LOOP AT y-x INTO DATA(x).
          result = |{ result }{ x-s }|.
        ENDLOOP.
        result = |{ result }\n|.
      ENDLOOP.
      result = |{ result }\n|.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_the_map.
    LOOP AT mt_map INTO DATA(z).
      LOOP AT z-y INTO DATA(y).
        LOOP AT y-x INTO DATA(x).
          IF x-s = |#|.
            result = result + 1.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_the_map_p2.
    LOOP AT mt_map_p2 INTO DATA(w).
      LOOP AT w-z INTO DATA(z).
        LOOP AT z-y INTO DATA(y).
          LOOP AT y-x INTO DATA(x).
            IF x-s = |#|.
              result = result + 1.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD part_1_cycle.
    DATA(lt_map) = mt_map.

    DO lines( lt_map ) TIMES.
      READ TABLE lt_map INDEX sy-index ASSIGNING FIELD-SYMBOL(<z>).
      DO lines( <z>-y ) TIMES.
        READ TABLE <z>-y INDEX sy-index ASSIGNING FIELD-SYMBOL(<y>).
        DO lines( <y>-x ) TIMES.
          READ TABLE <y>-x INDEX sy-index ASSIGNING FIELD-SYMBOL(<x>).
          IF <x>-s = |#| AND count_neighbours_up_to_4( z = <z>-z
                                                       y = <y>-y
                                                       x = <x>-x ) NOT BETWEEN 2 AND 3.
            <x>-s = |.|.
          ELSEIF <x>-s = |.| AND count_neighbours_up_to_4( z = <z>-z
                                                           y = <y>-y
                                                           x = <x>-x ) = 3.
            <x>-s = |#|.
          ENDIF.
        ENDDO.
      ENDDO.
    ENDDO.

    mt_map = lt_map.
  ENDMETHOD.

  METHOD part_2_cycle.
    DATA(lt_map_p2) = mt_map_p2.

    DO lines( lt_map_p2 ) TIMES.
      READ TABLE lt_map_p2 INDEX sy-index ASSIGNING FIELD-SYMBOL(<w>).
      DO lines( <w>-z ) TIMES.
        READ TABLE <w>-z INDEX sy-index ASSIGNING FIELD-SYMBOL(<z>).
        DO lines( <z>-y ) TIMES.
          READ TABLE <z>-y INDEX sy-index ASSIGNING FIELD-SYMBOL(<y>).
          DO lines( <y>-x ) TIMES.
            READ TABLE <y>-x INDEX sy-index ASSIGNING FIELD-SYMBOL(<x>).
            IF <x>-s = |#| AND count_neighbours_up_to_4_p2( w = <w>-w
                                                            z = <z>-z
                                                            y = <y>-y
                                                            x = <x>-x ) NOT BETWEEN 2 AND 3.
              <x>-s = |.|.
            ELSEIF <x>-s = |.| AND count_neighbours_up_to_4_p2( w = <w>-w
                                                                z = <z>-z
                                                                y = <y>-y
                                                                x = <x>-x ) = 3.
              <x>-s = |#|.
            ENDIF.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDDO.

    mt_map_p2 = lt_map_p2.
  ENDMETHOD.

  METHOD count_neighbours_up_to_4.
    DATA s_z TYPE i.
    DATA s_y TYPE i.
    DATA s_x TYPE i.

    DO 3 TIMES.
      s_z = ( z + sy-index ) - 2.
      READ TABLE mt_map WITH KEY z = s_z INTO DATA(map_z).
      DO 3 TIMES.
        s_y = ( y + sy-index ) - 2.
        READ TABLE map_z-y WITH KEY y = s_y INTO DATA(map_y).
        DO 3 TIMES.
          s_x = ( x + sy-index ) - 2.
          IF s_x = x AND s_y = y AND s_z = z.
            CONTINUE.
          ENDIF.
          READ TABLE map_y-x WITH KEY x = s_x INTO DATA(map_x).
          IF map_x-s = |#|.
            result = result + 1.
            IF result = 4.
              RETURN.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDDO.
    ENDDO.

  ENDMETHOD.

  METHOD count_neighbours_up_to_4_p2.
    DATA s_w TYPE i.
    DATA s_z TYPE i.
    DATA s_y TYPE i.
    DATA s_x TYPE i.

    DO 3 TIMES.
      s_w = ( w + sy-index ) - 2.
      READ TABLE mt_map_p2 WITH KEY w = s_w INTO DATA(map_w).
      DO 3 TIMES.
        s_z = ( z + sy-index ) - 2.
        READ TABLE map_w-z WITH KEY z = s_z INTO DATA(map_z).
        DO 3 TIMES.
          s_y = ( y + sy-index ) - 2.
          READ TABLE map_z-y WITH KEY y = s_y INTO DATA(map_y).
          DO 3 TIMES.
            s_x = ( x + sy-index ) - 2.
            IF s_x = x AND s_y = y AND s_z = z AND s_w = w.
              CONTINUE.
            ENDIF.
            READ TABLE map_y-x WITH KEY x = s_x INTO DATA(map_x).
            IF map_x-s = |#|.
              result = result + 1.
            ENDIF.
            IF result = 4.
              RETURN.
            ENDIF.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
