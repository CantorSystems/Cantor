(*
    Menuet and Kolibri OS executable file headers

    Copyright (c) 2013-2014 Vladislav Javadov (Freeman)
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

  TKolibriMagic = packed record
    case Byte of
      0: (Sign: array[0..6] of LegacyChar; // 'KOLIBRI'#0
          Version: Byte);
      1: (AsQuadWord: QuadWord);
  end;

  TKolibriHeader = packed record  // by Freeman
    Magic: TKolibriMagic;
    Alignment: Byte;
    Reserved: array[0..2] of Byte;
    ImageBase, EntryPoint,
    CodeSize, ImportsSize, ExportsSize, InitDataSize,
    UninitDataSize, StackSize: LongWord;
    CommandLine, ExecPath, Environment, TLS: Pointer;
  end;

{ // by Freeman
    magic         db    'KOLIBRI', 0  ; sign, version
    alignment     db    4   ; power of 2: 3 = 8 bytes, 4 = 16 bytes
    reserved      db    3 dup 0
    imagebase     dd    0   ; image base, for better flexibility
    entrypoint    dd    0   ; entry point, for better flexibility
    code          dd    0   ; size of code section
    import        dd    0   ; size of import section, 0 = section is not present
    export        dd    0   ; size of export section, 0 = application
    data_         dd    0   ; size of initialized data section
    mem           dd    0   ; size of uninitialized memory for application, as the same in MENUET01
    stack_        dd    0   ; size of stack
    cmdline       dd    -1  ; command line, kernel-defined; -1 = prompt for params
    execpath      dd    0   ; exec path, kernel-defined
    env           dd    0   ; environment variables, kernel-defined
    tls           dd    0   ; address of thread local storage (TLS), kernel-defined
}

{ // by Serge
    db 'KOLIBRI',0     ;+00 banner + revision 0-255
    dd start           ;+08 application entry
    dd ecode           ;+12 end of code and constant data
    dd import          ;+16 import section, if present
    dd eimport         ;+20 end of import section
    dd edata           ;+24 end of initialized data, end of disk image
    dd emem            ;+28 end of uninitialized data
    dd STACKSIZE       ;+32 size of stack
    dd 0               ;+36 address of tls info structure kernel-defined
    dd 0               ;+38 exec_path  kernel-defined
    dd 0               ;+40 cmdline    kernel-defined
    dd 0               ;+44 env        kernel-defined
}