//
// Header for NLS Files (native-nt-toolkit)
//
typedef struct _NLS_FILE_HEADER
{
    USHORT HeaderSize;
    USHORT CodePage;
    USHORT MaximumCharacterSize;
    USHORT DefaultChar;
    USHORT UniDefaultChar;
    USHORT TransDefaultChar;
    USHORT TransUniDefaultChar;
    USHORT DBCSCodePage;
    UCHAR LeadByte[MAXIMUM_LEADBYTES];
} NLS_FILE_HEADER, *PNLS_FILE_HEADER;


// express-os

typedef _Packed struct _NLS_FILE_HEADER
{
        USHORT  HeaderSize;
        USHORT  CodePage;
        USHORT  CharacterSize;  /* SBCS = 1, DBCS = 2 */
        USHORT  DefaultChar;
        USHORT  UniDefaultChar;
        USHORT  TransDefaultChar;
        USHORT  TransUniDefaultChar;
        UCHAR   LeadByte[MAX_LEADBYTES];
        USHORT  CodePageClass;
} *PNLS_FILE_HEADER;

