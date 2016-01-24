(*
    Menuet OS and Kolibri OS executable file headers

    Copyright (c) 2013-2015 Vladislav Javadov (Freeman)
*)

{
    db     'MENUET01'              ; 8 byte id
    dd     0x01                    ; header version
    dd     START                   ; start of code
    dd     I_END                   ; size of image
    dd     0x1000                  ; memory for app
    dd     0x1000                  ; esp
    dd     0x0 , 0x0               ; I_Param , I_Icon

    db     'MENUET02'
    dd     1
    dd     start
    dd     i_end
    dd     mem
    dd     mem
    dd     0       ; cmdline
    dd     0       ; path
    dd     0       ; tls data
}

type
  TMenuetHeader = packed record
    Magic: array[0..7] of LegacyChar; // 'MENUET01', 'MENUET02';
    HeaderVersion, EntryPoint, ImageSize, HeapSize, ESP,
    CommandLine: LongWord;
    case Byte of
      1: (Icon: LongWord); // when MENUET01
      2: (TLS: LongWord);  // when MENUET02
  end;

