{
   Copyright (C) 2006 The devFlowcharter project.
   The initial author of this file is Michal Domagala.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}



unit Constants;

interface

uses
   Vcl.Graphics, Types, System.UITypes;

const   // Global constants

        PROGRAM_NAME        = 'devFlowcharter';

        NEW_PROJECT_CAPTION = PROGRAM_NAME;
        PROJECT_CAPTION     = NEW_PROJECT_CAPTION + ' - ';

        // hint duration in milliseconds
        HINT_PAUSE       = 5000;

        INCORRECT_IDENT  = -6;
        DUPLICATED_IDENT = -7;
        RESERVED_IDENT   = -8;
        INVALID_INIT_VAL = -9;
        VALID_IDENT      =  1;

        LOOP_BLOCKS = [blWhile, blRepeat, blFor];

        EDITOR_DEFAULT_INDENT_LENGTH = 2;
        EDITOR_DEFAULT_FONT_SIZE = 10;
        LABEL_DEFAULT_FONT_SIZE = 10;
        EDITOR_DEFAULT_GUTTER_FONT_SIZE = 8;

        TAB_CHAR        = #9;
        SPACE_CHAR      = #32;
        INDENT_XML_CHAR = TAB_CHAR;

        TO_MAIN_FORM_KEYS = [vkDelete, vkF10, vkF11, vkF12];

        PAGE_CAPTION_ATTR = 'tab';
        PAGE_FRONT_ATTR   = 'pageFront';
        LANG_ATTR         = 'language';
        FOLDED_ATTR       = 'folded';
        FRAME_ATTR        = 'frame';
        BLOCK_TYPE_ATTR   = 'type';
        ID_ATTR           = 'hash';
        BRANCH_STMNT_ATTR = 'bstmnt_hash';
        FONT_SIZE_ATTR    = 'fontsize';
        FONT_STYLE_ATTR   = 'fontstyle';
        Z_ORDER_ATTR      = 'ZOrdVal';
        EXTERN_ATTR       = 'extern';
        SIZE_ATTR         = 'size';
        INIT_ATTR         = 'init';
        VALUE_ATTR        = 'value';
        NAME_ATTR         = 'name';
        TYPE_ATTR         = 'type';
        IS_HEADER_ATTR    = 'isHeader';
        APP_VERSION_ATTR  = 'devFVersion';
        KIND_ATTR         = 'kind';
        POINTER_ATTR      = 'pointer';
        BLOCK_TAG         = 'block';
        BRANCH_TAG        = 'branch';
        TEXT_TAG          = 'text';
        VAR_TAG           = 'var';
        CONST_TAG         = 'const';
        DATATYPE_TAG      = 'datatype';
        FUNCTION_TAG      = 'routine';
        HEADER_TAG        = 'header';
        COMMENT_TAG       = 'comment';
        FOLD_TEXT_TAG     = 'foldtext';
        PROGRAM_TEMPLATE_TAG = 'FileContentsTemplate';

        LB_PHOLDER  = '#!';
        LB_PHOLDER2  = '##';

        VERSION_NUMBER_SEPARATOR  = '.';

        MAIN_PAGE_MARKER  = 'mainPage#!';

        MARGIN_X = 50;
        MARGIN_Y = 50;

        OK_COLOR = clGreen;
        NOK_COLOR = clRed;
        WARN_COLOR = clOlive;
        TEXT_COLOR = clGrayText;
        BLACK_COLOR = clWindowText;
        DEFAULT_DESKTOP_COLOR = clWhite;
        MATCH_BRACKET_COLOR = clRed;

        ID_ALLOW_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

        APPLICATION_DEFAULT_FONT_NAME = 'Tahoma';
        APPLICATION_DEFAULT_FONT_SIZE = 8;
        FLOWCHART_DEFAULT_FONT_NAME = 'Tahoma';
        FLOWCHART_MIN_FONT_SIZE = 8;
        FLOWCHART_MAX_FONT_SIZE = FLOWCHART_MIN_FONT_SIZE + 4;
        FLOWCHART_VALID_FONT_SIZES = [FLOWCHART_MIN_FONT_SIZE..FLOWCHART_MAX_FONT_SIZE];
        FLOWCHART_FONT_NAMESIZE_SEP = ' : ';

        ROW_NOT_FOUND = -1;
        BRANCH_IDX_NOT_FOUND = -1;

        FUNCTION_TYPE_IND = -5;

        MAX_SUPPORTED_PPI = 120;

        PRIMARY_PLACEHOLDER = '%s1';

        DEF_PAGE_CAPTION_KEY = 'mainPage';

        SETTINGS_SECTION = 'Settings';

        PRINT_SCALE_BASE     = 100;   // 100 %
        DEFAULT_PRINT_MARGIN = 5;     //   5 %

        DECLARATIONS_FORM_RIGHT_MARGIN = 16;

        BRANCH_PLACEHOLDER = '%b';

        RTF_FILES_FILTER_KEY = 'RTFFilesFilter';
        HTML_FILES_FILTER_KEY = 'HTMLFilesFilter';
        XML_FILES_FILTER_KEY = 'XMLFilesFilter';
        BMP_FILES_FILTER_KEY = 'BMPFilesFilter';
        PNG_FILES_FILTER_KEY = 'PNGFilesFilter';
        JPG_FILES_FILTER_KEY = 'JPGFilesFilter';
        EDITOR_DIALOG_FILTER_KEYS: TArray<string> = [RTF_FILES_FILTER_KEY, HTML_FILES_FILTER_KEY];
        PROJECT_DIALOG_FILTER_KEYS: TArray<string> = [XML_FILES_FILTER_KEY, BMP_FILES_FILTER_KEY, PNG_FILES_FILTER_KEY, JPG_FILES_FILTER_KEY];

        // Language identifiers; must be identical to value in <Name> tag in XML language definition file
        PASCAL_LANG_ID  = 'Pascal';
        C_LANG_ID       = 'ANSI C';
        TIBASIC_LANG_ID = 'TIBASIC';
        PYTHON_LANG_ID  = 'Python 3';
        JAVA_LANG_ID    = 'Java';

        COLOR_SHAPES: array[TColorShape] of TColorShape = (shpEllipse, shpParallel, shpDiamond, shpRectangle, shpRoadSign, shpRoutine, shpFolder);

implementation

end.


