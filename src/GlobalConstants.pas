{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: GlobalConstants.pas,v 1.10 2004/04/04 20:17:12 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit GlobalConstants;

interface

uses messages;

const

    // Windows messages
    WM_IMAPOPOVER = WM_USER + 1;
    WM_LOGINOVER = WM_USER + 2;
    WM_SIZE_CHECKER_OVER = WM_USER+3;
    WM_CONVERTER_OVER = WM_USER+4;
    WM_UPDATE_TREE_SIZE_CALC = WM_USER + 5;
    WM_UPDATE_TREE_SIZE_CALC_NO_IMG = WM_USER + 6;
    WM_PROGRESS_BAR_UPDATE = WM_USER + 7;
    WM_MAIN_PROGRESS_BAR_VISIBILITY = WM_USER + 8;
    WM_GOTCONNECTION = WM_USER + 9;
    WM_REQUEST_OVER = WM_USER + 10;
    WM_SHOW_QUOTA = WM_USER + 11;
    WM_HIERARCHY_CHANGE = WM_USER + 12;
    WM_GLOBAL_SEARCH_UPDATE = WM_USER + 13;
    WM_GLOBAL_SEARCH_OVER = WM_USER + 14;
    WM_UPDATE_MESSAGE_LIST = WM_USER + 15;
    WM_ACTIVITY_DLG_REMOVE_REQUEST = WM_USER + 16;

    // Character abreviations
    CR = #$0d;
    LF = #$0a;
    CRLF = CR + LF;
    BACKSLASH = #$5c;  // '\'

    // Regular expressions
    invCharRE = '[\\/:\*\?""<>\|]';  // Invalid Windows filename characters
    invPathRE = '[/:\*\?""<>\|]';  // Invalid Windows file path characters (same as above, without the folder separator)
    NUM_LITERAL_RE = '{\d+}';        // Numeric part of a string literal

    MBOX_TREE_SIZE_INCREMENT = 50;
    FETCH_PAGE_INCREMENT = 40;  // Number of messages to fetch before updating the message list (if paging is enabled)

    RFC822_HEADER_IMAPSIZE = 'X-IMAPSize: ';

    STRING_FIFO_QUEUE_DONE_SIGNAL = 'DONE';  // string to add to a fifo queue which signals an end of an operation


implementation

end.