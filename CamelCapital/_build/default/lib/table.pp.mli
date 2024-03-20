Caml1999N033����            -lib/table.mli����  :�    $_  "�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,CamelCapital��A@A�A@M@@��A@@� A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@� ��A�  # �!t��-lib/table.mliA@E�A@F@@@@A@���)ocaml.doc���@@ ���@@ �A�������? An alias for the type [Table] ��BGG�BGk@@��BGG�BGk@@@@��BGG�BGk@@��BGG�BGk@@��A@@� A@F@@��"A@@�#A@F@������-NoColumnFound��,Dmw�-Dm D@�@�@@��1Dmm�2Dm D@���0�� @@ ��@@ �A�������	e [exception NoColumnFound] is raised when a column with a given title is not
    present in a table. ��BE E E�CF � �@@��EE E E�FF � �@@@@��HE E E�IF � �@@��KE E E�LF � �@@��@@ ��@@ �A@��QDmm�RDm D@���Р+empty_table��ZH � ��[H � �@����!t��bH � ��cH � �@@��eH � ��fH � �@@@@���d��4@@ ��5@@ �A�������	! The empty table has no columns. ��vI � ��wI � �@@��yI � ��zI � �@@@@��|I � ��}I � �@@��I � ���I � �@@���H � ���H � �@���H � ���H � �@���Р*add_column���K � ���K � �@��@����!t���K � ���K � �@@���K � ���K � �@@@��@�����&Column!t���K ���K �@@���K ���K �@@@����!t���K ���K �@@���K ���K �@@@���K ���K �@@@���K � ���K �@@@@�������@@ ���@@ �A�������	r [add_column tbl col] is a copy of table [tbl] with column [col] appended to
    the right-hand side of the table.���L��Ma�@@���L��Ma�@@@@���L��Ma�@@���L��Ma�@@���K � ���K �@���K � ���K �@���Р-remove_column���O����O��@��@����!t���O����O��@@���O����O��@@@��@����&string���O����O��@@���O����O��@@@����!t��O���O��@@��	O���
O��@@@��O���O��@@@��O���O��@@@@������@@ ���@@ �A�������	� [remove_column tbl col_name] is a copy of table [tbl] with the column titled
    [col_name] removed. If no column with that title exists, [tbl] is returned.�� P���!Q Q@@��#P���$Q Q@@@@��&P���'Q Q@@��)P���*Q Q@@��,O���-O��@��/O���0O��@���Р2get_column_by_name��8SSW�9SSi@��@����!t��BSSl�CSSm@@��ESSl�FSSm@@@��@����&string��OSSq�PSSw@@��RSSq�SSSw@@@�����&Column!t��\SS{�]SS�@@��_SS{�`SS�@@@��bSSq�cSS�@@@��eSSl�fSS�@@@@���d��4@@ ��5@@ �A�������	� [get_column_by_name tbl name] is the column in table [tbl] with the title
    [name]. Raises NoColumnFound if no column by the given title exists in
    [tbl].��vT���wV)@@��yT���zV)@@@@��|T���}V)@@��T����V)@@���SSS��SS�@���SSS��SS�@���Р&length���X+/��X+5@��@����!t���X+8��X+9@@���X+8��X+9@@@����#int���X+=��X+@@@���X+=��X+@@@@���X+8��X+@@@@@������x@@ ��y@@ �A�������	0 [length tbl] is the number of columns in [tbl].���YAA��YAv@@���YAA��YAv@@@@���YAA��YAv@@���YAA��YAv@@���X++��X+@@���X++��X+@@���Р+print_table���[x|��[x�@���%index����#int���[x���[x�@@���[x���[x�@@@���(num_rows����#int���[x���[x�@@���[x���[x�@@@��@����!t���[x���[x�@@���[x���[x�@@@����$unit��[x��[x�@@��[x��	[x�@@@��[x��[x�@@@��[x��[x�@@@��[x��[x�@@@@������@@ ���@@ �A�������	m [print_table tbl num_rows] prints the titles of each column and then the
    first [num_rows] rows of [tbl].��"\���#] %@@��%\���&] %@@@@��(\���)] %@@��+\���,] %@@��.[xx�/[x�@��1[xx�2[x�@���Р5create_table_from_csv��:_'+�;_'@@��@����&string��D_'C�E_'I@@��G_'C�H_'I@@@����!t��O_'M�P_'N@@��R_'M�S_'N@@@��U_'C�V_'N@@@@���T��$@@ ��%@@ �A�������	� [create_table_from_csv filename] is a table with contents retrieved from the
    file at data/"filename".csv. Do not include the ".csv" Requires: the
    provided CSV table is rectangular and all columns are uniform in type. ��f`OO�gb�6@@��i`OO�jb�6@@@@��l`OO�mb�6@@��o`OO�pb�6@@��r_''�s_'N@��u_''�v_'N@���Р.map_int_column��~d8<�d8J@��@����!t���d8M��d8N@@���d8M��d8N@@@��@����&string���d8R��d8X@@���d8R��d8X@@@��@��@����#int���d8]��d8`@@���d8]��d8`@@@����#int���d8d��d8g@@���d8d��d8g@@@���d8]��d8g@@@����!t���d8l��d8m@@���d8l��d8m@@@���d8\��d8m@@@���d8R��d8m@@@���d8M��d8m@@@@���Ȱ��@@ ���@@ �A�������
   [map_int_column tbl col_name func] is the table created by applying [func]
    to every entry in the column with name [col_name]. Requires: The column with
    name [col_name] has integer entries. Raises: [NoColumnFound] if there is no
    column in [tbl] with the name [col_name]. ���enn��h^�@@���enn��h^�@@@@���enn��h^�@@���enn��h^�@@���d88��d8m@���d88��d8m@���Р0map_float_column���j����j��@��@����!t���j����j��@@���j��� j��@@@��@����&string��	j���
j��@@��j���j��@@@��@��@����%float��j���j��@@��j���j��@@@����%float��#j���$j��@@��&j���'j��@@@��)j���*j��@@@����!t��1j���2j��@@��4j���5j��@@@��7j���8j��@@@��:j���;j��@@@��=j���>j��@@@@���<��@@ ��@@ �A�������
   [map_float_column tbl col_name func] is the table created by applying [func]
    to every entry in the column with name [col_name]. Requires: The column with
    name [col_name] has float entries. Raises: [NoColumnFound] if there is no
    column in [tbl] with the name [col_name]. ��Nk���On��@@��Qk���Rn��@@@@��Tk���Un��@@��Wk���Xn��@@��Zj���[j��@��]j���^j��@���Р1map_string_column��fp���gp�@��@����!t��pp��qp�@@��sp��tp�@@@��@����&string��}p��~p�@@���p���p�@@@��@��@����&string���p���p�@@���p���p�@@@����&string���p� ��p�&@@���p� ��p�&@@@���p���p�&@@@����!t���p�+��p�,@@���p�+��p�,@@@���p���p�,@@@���p���p�,@@@���p���p�,@@@@�������@@ ���@@ �A�������
   [map_string_column tbl col_name func] is the table created by applying
    [func] to every entry in the column with name [col_name]. Requires: The
    column with name [col_name] has string entries. Raises: [NoColumnFound] if
    there is no column in [tbl] with the name [col_name]. ���q--��t		O@@���q--��t		O@@@@���q--��t		O@@���q--��t		O@@���p����p�,@���p����p�,@���Р&equals���v	Q	U��v	Q	[@��@����!t���v	Q	^��v	Q	_@@���v	Q	^��v	Q	_@@@��@����!t���v	Q	c��v	Q	d@@���v	Q	c��v	Q	d@@@����$bool���v	Q	h��v	Q	l@@���v	Q	h� v	Q	l@@@��v	Q	c�v	Q	l@@@��v	Q	^�v	Q	l@@@@������@@ ���@@ �A�������	� [equals t1 t2] is true if tables t1 and t2 have structurally equivalent
    (under [Column.equals])) columns in the same order.��w	m	m�x	�	�@@��w	m	m�x	�	�@@@@��w	m	m�x	�	�@@��w	m	m� x	�	�@@��"v	Q	Q�#v	Q	l@��%v	Q	Q�&v	Q	l@���Р4filter_by_int_column��.z	�	��/z	�
@��@����!t��8z	�
�9z	�
@@��;z	�
�<z	�
@@@��@��@����&option��Gz	�
�Hz	�
@�����#int��Pz	�
�Qz	�
@@��Sz	�
�Tz	�
@@@@��Vz	�
�Wz	�
@@@����$bool��^z	�
#�_z	�
'@@��az	�
#�bz	�
'@@@��dz	�
�ez	�
'@@@��@����&string��nz	�
,�oz	�
2@@��qz	�
,�rz	�
2@@@����!t��yz	�
6�zz	�
7@@��|z	�
6�}z	�
7@@@��z	�
,��z	�
7@@@���z	�
��z	�
7@@@���z	�
��z	�
7@@@@������T@@ ��U@@ �A�������
  s [filter_by_int_column tbl crit_func col_name] is the table created by
    removing all rows of [tbl] in which the entries in the column titled
    [col_name] are false under the criteria function [crit_func]. Requires: the
    column titled [col_name] has integer entries. Raises: [Column.WrongFunction]
    if the column titled [col_name] does not have integer entries.���{
8
8��l�@@���{
8
8��l�@@@@���{
8
8��l�@@���{
8
8��l�@@���z	�	���z	�
7@���z	�	���z	�
7@���Р7filter_by_string_column��� A���� A��@��@����!t��� A���� A��@@��� A���� A��@@@��@��@����&option��� A���� A��@�����&string��� A���� A��@@��� A���� A��@@@@��� A���� A��@@@����$bool��� A���� A��@@��� A���� A��@@@��� A���� A��@@@��@����&string��� A���� A��@@��� A���� A��@@@����!t��� A���� A��@@��� A���� A��@@@��� A���  A��@@@�� A��� A��@@@�� A��� A��@@@@������@@ ���@@ �A�������
  t [filter_by_string_column tbl crit_func col_name] is the table created by
    removing all rows of [tbl] in which the entries in the column titled
    [col_name] are false under the criteria function [crit_func]. Requires: the
    column titled [col_name] has string entries. Raises: [Column.WrongFunction]
    if the column titled [col_name] does not have string entries.�� B��� F2u@@�� B��� F2u@@@@�� B��� F2u@@�� B���  F2u@@��" A���# A��@��% A���& A��@���Р6filter_by_float_column��. Hw{�/ Hw�@��@����!t��8 Hw��9 Hw�@@��; Hw��< Hw�@@@��@��@����&option��G Hw��H Hw�@�����%float��P Hw��Q Hw�@@��S Hw��T Hw�@@@@��V Hw��W Hw�@@@����$bool��^ Hw��_ Hw�@@��a Hw��b Hw�@@@��d Hw��e Hw�@@@��@����&string��n Hw��o Hw�@@��q Hw��r Hw�@@@����!t��y Hw��z Hw�@@��| Hw��} Hw�@@@�� Hw��� Hw�@@@��� Hw��� Hw�@@@��� Hw��� Hw�@@@@������T@@ ��U@@ �A�������
  q [filter_by_float_column tbl crit_func col_name] is the table created by
    removing all rows of [tbl] in which the entries in the column titled
    [col_name] are false under the criteria function [crit_func]. Requires: the
    column titled [col_name] has float entries. Raises: [Column.WrongFunction]
    if the column titled [col_name] does not have float entries.��� I���� M�5@@��� I���� M�5@@@@��� I���� M�5@@��� I���� M�5@@��� Hww�� Hw�@��� Hww�� Hw�@���Р.reduce_int_col��� O7;�� O7I@��@����!t��� O7L�� O7M@@��� O7L�� O7M@@@��@����&string��� O7Q�� O7W@@��� O7Q�� O7W@@@��@��@��!a��� O7\�� O7^@@@��@����&option��� O7f�� O7l@�����#int��� O7b�� O7e@@��� O7b�� O7e@@@@��� O7b�� O7l@@@��!a��� O7p�� O7r@@@��� O7b�� O7r@@@��� O7\�� O7r@@@��@��!a��� O7w�  O7y@@@��!a�� O7}� O7@@@�� O7w�	 O7@@@�� O7[� O7@@@�� O7Q� O7@@@�� O7L� O7@@@@������@@ ���@@ �A�������
   [reduce_int_col tbl col_name func init] is the result of folding left over
    the body of the column titled [col_name] with [func] and [init]. Requires:
    [col_name] has integer entries. Raises: [WrongFunction] if [col_name] does
    not have integer entries. ��" P���# Sm�@@��% P���& Sm�@@@@��( P���) Sm�@@��+ P���, Sm�@@��. O77�/ O7@��1 O77�2 O7@���Р0reduce_float_col��: U���; U��@��@����!t��D U���E U��@@��G U���H U��@@@��@����&string��Q U���R U��@@��T U���U U��@@@��@��@��!a��^ U���_ U��@@@��@����&option��h U���i U��@�����%float��q U���r U��@@��t U���u U��@@@@��w U���x U��@@@��!a��} U���~ U��@@@��� U���� U��@@@��� U���� U��@@@��@��!a��� U���� U��@@@��!a��� U���� U��@@@��� U���� U��@@@��� U���� U��@@@��� U���� U��@@@��� U���� U��@@@@������l@@ ��m@@ �A�������
   [reduce_float_col tbl col_name func init] is the result of folding left over
    the body of the column titled [col_name] with [func] and [init]. Requires:
    [col_name] has float entries. Raises: [WrongFunction] if [col_name] does not
    have float entries. ��� V���� Y��@@��� V���� Y��@@@@��� V���� Y��@@��� V���� Y��@@��� U���� U��@��� U���� U��@���Р1reduce_string_col��� [���� [��@��@����!t��� [��� [�@@��� [��� [�@@@��@����&string��� [��� [�@@��� [��� [�@@@��@��@��!a��� [��� [�@@@��@����&option��� [��� [�$@�����&string��� [��� [�@@��  [�� [�@@@@�� [�� [�$@@@��!a��	 [�(�
 [�*@@@�� [�� [�*@@@�� [�� [�*@@@��@��!a�� [�/� [�1@@@��!a�� [�5� [�7@@@��  [�/�! [�7@@@��# [��$ [�7@@@��& [��' [�7@@@��) [��* [�7@@@@���(���@@ ���@@ �A�������
  	 [reduce_string_col tbl col_name func init] is the result of folding left
    over the body of the column titled [col_name] with [func] and [init].
    Requires: [col_name] has string entries. Raises: [WrongFunction] if
    [col_name] does not have string entries. ��: \88�; _F@@��= \88�> _F@@@@��@ \88�A _F@@��C \88�D _F@@��F [���G [�7@��I [���J [�7@@