{
    db     'MENUET01'              ; 8 byte id
    dd     0x01                    ; header version
    dd     START                   ; start of code
    dd     I_END                   ; size of image
    dd     0x1000                  ; memory for app
    dd     0x1000                  ; esp
    dd     0x0 , 0x0               ; I_Param , I_Icon
}
{
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