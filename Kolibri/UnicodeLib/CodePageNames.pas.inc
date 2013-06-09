(*
    Kolibri OS Unicode Library (UnicodeLib)

    Code page names -- Russian and English

    Windows-1251 character set used here just for Delphi,
    this section has no mean for Kolibri

    Copyright © 2012 The Unified Environment Laboratory
*)

const
  sRu437 = 'США (DOS)';
  sRu737 = 'Греческая (DOS)';
  sRu775 = 'Балтийская (DOS)';
  sRu850 = 'Западноевропейская (DOS)';
  sRu852 = 'Центральноевропейская (DOS)';
  sRu855 = 'Кириллица (DOS)';
  sRu857 = 'Турецкая (DOS)';
//  sRu858 = 'Западноевропейская + Евро (DOS)';
  sRu860 = 'Португальская (DOS)';
  sRu861 = 'Исландская (DOS)';
//  sRu862 = 'Hebrew (DOS)';
  sRu863 = 'Франко-канадская (DOS)';
  sRu865 = 'Скандинавская (DOS)';
  sRu866 = 'Русская (DOS)';

  sRu1250 = 'Центральноевропейская (Windows)';
  sRu1251 = 'Кириллица (Windows)';
  sRu1252 = 'Западноевропейская (Windows)';
  sRu1253 = 'Греческая (Windows)';
  sRu1254 = 'Турецкая (Windows)';
//  sRu1255 = 'Иврит (Windows)';
//  sRu1256 = 'Арабская (Windows)';
  sRu1257 = 'Балтийская (Windows)';

  sRu20866 = 'Русская КОИ-8';
  sRu21866 = 'Украинская КОИ-8';

  sRu28591 = 'Западноевропейская (ISO 8859-1)';
  sRu28592 = 'Центральноевропейская (ISO 8859-2)';
  sRu28593 = 'Латинская 3 (ISO 8859-3)';
  sRu28594 = 'Балтийская (ISO 8859-4)';
  sRu28595 = 'Кириллица (ISO 8859-5)';
  sRu28597 = 'Греческая (ISO 8859-7)';
  sRu28599 = 'Латинская 5 (ISO 8859-9)';
  sRu28605 = 'Латинская 9 (ISO 8859-15)';

  sEn437 = 'United States (DOS)';
  sEn737 = 'Greek (DOS)';
  sEn775 = 'Baltic (DOS)';
  sEn850 = 'Western European (DOS)';
  sEn852 = 'Central European (DOS)';
  sEn855 = 'Cyrillic (DOS)';
  sEn857 = 'Turkish (DOS)';
//  sEn858 = 'Western European + Euro (DOS)';
  sEn860 = 'Portuguese (DOS)';
  sEn861 = 'Icelandic (DOS)';
//  sEn862 = 'Hebrew (DOS)';
  sEn863 = 'Canadian French (DOS)';
  sEn865 = 'Nordic (DOS)';
  sEn866 = 'Russian (DOS)';

  sEn1250 = 'Central European (Windows)';
  sEn1251 = 'Cyrillic (Windows)';
  sEn1252 = 'Western European (Windows)';
  sEn1253 = 'Greek (Windows)';
  sEn1254 = 'Turkish (Windows)';
//  sEn1255 = 'Hebrew (Windows)';
//  sEn1256 = 'Arabic (Windows)';
  sEn1257 = 'Baltic (Windows)';

  sEn20866 = 'KOI8-Russian';
  sEn21866 = 'KOI8-Ukrainian';

  sEn28591 = 'Western European (ISO 8859-1)';
  sEn28592 = 'Central European (ISO 8859-2)';
  sEn28593 = 'Latin 3 (ISO 8859-3)';
  sEn28594 = 'Baltic (ISO 8859-4)';
  sEn28595 = 'Cyrillic (ISO 8859-5)';
  sEn28597 = 'Greek (ISO 8859-7)';
  sEn28599 = 'Latin 5 (ISO 8859-9)';
  sEn28605 = 'Latin 9 (ISO 8859-15)';

  CodePageMIME: TCodePageNames = (
    'cp437', 'cp737', 'cp775', 'cp850', 'cp852', 'cp855', 'cp857', 'cp860', 'cp861', 'cp863', 'cp865', 'cp866',
    'windows-1250', 'windows-1251', 'windows-1252', 'windows-1253', 'windows-1254', 'windows-1257',
    'KOI8-R', 'KOI8-U',
    'ISO-8859-1', 'ISO-8859-2', 'ISO-8859-3', 'ISO-8859-4', 'ISO-8859-5', 'ISO-8859-7', 'ISO-8859-9', 'ISO-8859-15'
  );

  CodePageNamesRu: TCodePageNames = (
    { DOS }     sRu437, sRu737, sRu775, sRu850, sRu852, sRu855, sRu857, sRu860, sRu861, sRu863, sRu865, sRu866,
    { Windows } sRu1250, sRu1251, sRu1252, sRu1253, sRu1254, sRu1257,
    { KOI }     sRu20866, sRu21866,
    { ISO }     sRu28591, sRu28592, sRu28593, sRu28594, sRu28595, sRu28597, sRu28599, sRu28605
  );

  CodePageNamesEn: TCodePageNames = (
    { DOS }     sEn437, sEn737, sEn775, sEn850, sEn852, sEn855, sEn857, sEn860, sEn861, sEn863, sEn865, sEn866,
    { Windows } sEn1250, sEn1251, sEn1252, sEn1253, sEn1254, sEn1257,
    { KOI }     sEn20866, sEn21866,
    { ISO }     sEn28591, sEn28592, sEn28593, sEn28594, sEn28595, sEn28597, sEn28599, sEn28605
  );

