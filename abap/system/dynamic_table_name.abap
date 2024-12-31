DATA:
  o_ref TYPE REF TO data.
FIELD-SYMBOLS:
  <lt_table> TYPE STANDARD TABLE,
  <fs>       TYPE ANY,
  <field>    TYPE ANY,
  <field1>   TYPE ANY.
PARAMETERS:
  p_tab       TYPE tabname,       " Table name (eg: MARA)
  p_field(20) TYPE c.                 " Field name (eg: MATNR)

START-OF-SELECTION.
  CREATE DATA o_ref TYPE TABLE OF (p_tab).

  ASSIGN p_field TO <field1>.
  ASSIGN o_ref->* TO <lt_table>.

  SELECT *
    INTO TABLE <lt_table>
    FROM (p_tab).

  LOOP AT <lt_table> ASSIGNING <fs>.
    ASSIGN COMPONENT <field1> OF STRUCTURE <fs>
                  TO <field>.
    IF sy-subrc = 0.
      WRITE:/ <field>.
    ENDIF.
  ENDLOOP.
=====================
https://www.erpgreat.com/abap/how-can-we-give-dynamic-table-name-in-select-statement.htm
